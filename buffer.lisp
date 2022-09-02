(in-package #:sc)

(defclass buffer ()
  ((bufnum :initarg :bufnum :initform nil :accessor bufnum)
   (frames :initarg :frames :initform nil :accessor frames)
   (chanls :initarg :chanls :initform nil :accessor chanls)
   (sr :initarg :sr :initform nil :accessor sr)
   (path :initarg :path :initform nil :accessor path)
   (server :initarg :server :initform nil :accessor server)))

(defmethod print-object ((self buffer) stream)
  (format stream "#<~s :server ~s :bufnum ~s :frames ~s :channels ~s :samplerate ~s :path ~s>"
          'buffer (server self) (bufnum self) (frames self) (chanls self) (sr self) (path self)))

(defmethod initialize-instance :after ((self buffer) &key)
  (setf (elt (buffers (server self)) (bufnum self)) self))

(defmethod floatfy ((buffer buffer))
  (floatfy (bufnum buffer)))

(defun get-next-buffer (server &optional bufnum)
  (bt:with-lock-held ((server-lock server))
    (let* ((bufnum (or bufnum (position nil (buffers server)))))
      (setf (elt (buffers server) bufnum) (make-instance 'buffer :bufnum bufnum :server server)))))

(defmacro with-sync-or-call-handle ((server buffer path complete-handler) &body body)
  `(progn
     (if ,complete-handler (bt:with-lock-held ((server-lock server))
			     (let* ((handlers (gethash (list ,path (floor (floatfy ,buffer))) (buffer-get-handlers ,server))))
			       (setf (gethash (list ,path (floor (floatfy ,buffer))) (buffer-get-handlers ,server))
				 (append handlers (list (lambda (buffer) (funcall ,complete-handler buffer))))))
			     ,@body)
       (progn
	 ,@body
	 (sync ,server)))
     ,buffer))


(defun buffer-alloc (frames &key (chanls 1) bufnum (server *s*) complete-handler)
  (let* ((buffer (get-next-buffer server bufnum))
	 (bufnum (slot-value buffer 'bufnum)))
    (setf (slot-value buffer 'frames) frames
          (slot-value buffer 'chanls) chanls
          (slot-value buffer 'server) server)
    (with-sync-or-call-handle (server buffer "/b_alloc" complete-handler) 
      (apply #'send-message server (list "/b_alloc" bufnum (floor frames) (floor chanls)
					 (sc-osc::encode-message "/b_query" bufnum))))))

(defun buffer-read (path &key bufnum (server *s*))
  (let ((file-path (full-pathname path)))
    (assert (probe-file file-path) (path) "File does not exist: ~a" file-path)
    (let* ((buffer (get-next-buffer server bufnum))
	   (bufnum (slot-value buffer 'bufnum)))
      (setf (slot-value buffer 'path) file-path
            (slot-value buffer 'server) server)
      (apply #'send-message server (list "/b_allocRead" bufnum file-path 0 -1 (sc-osc::encode-message "/b_query" bufnum)))
      (sync server)
      buffer)))

(defun buffer-read-channel (path &key channels bufnum (server *s*))
  (let ((file-path (full-pathname path)))
    (assert (probe-file file-path) (path) "File does not exist: ~a" file-path)
    (let* ((buffer (get-next-buffer server bufnum))
	   (bufnum (slot-value buffer 'bufnum)))
      (setf (slot-value buffer 'path) file-path
            (slot-value buffer 'server) server)
      (apply #'send-message server `("/b_allocReadChannel" ,bufnum ,file-path 0 -1 ,@(alexandria:ensure-list channels) ,(sc-osc::encode-message "/b_query" bufnum)))
      (sync server)
      buffer)))

(defun buffer-cue-soundfile (path &key (server *s*) (start-frame 0) (chanls 2) (frames 32768))
  (let* ((file-path (full-pathname path)))
    (assert (probe-file file-path) (path) "File does not exist: ~a" file-path)
    (let* ((buffer (buffer-alloc frames :chanls chanls :server server)))
      (send-message server "/b_read" (bufnum buffer) file-path start-frame frames 0 1
		    (sc-osc::encode-message "/b_query" (bufnum buffer)))
      (sync server)
      buffer)))

(defun buffer-write (buffer path &key (server *s*) (frames -1) (start-frame 0) (format :int24)
				   leave-open-p complete-handler)
  "Make audio-file from Buffer."
  (let ((bufnum (bufnum buffer))
	(file-path (full-pathname path)))
    (with-sync-or-call-handle (server buffer "/b_write" complete-handler)
      (send-message server "/b_write" bufnum file-path (pathname-type file-path) (ecase format
										   (:int16 "int16")
										   (:int24 "int24")
										   (:float "float")
										   (:double "double"))
		    frames start-frame (if leave-open-p 1 0)))))

(defun buffer-close (buffer &key (server *s*) complete-handler)
  (with-sync-or-call-handle (server buffer "/b_close" complete-handler)
    (send-message server "/b_close" (floatfy buffer) 0)))

(defmethod buffer-free ((buffer fixnum) &key (server *s*) complete-handler)
  (let* ((free-buffer nil))
    (bt:with-lock-held ((server-lock server))
      (assert (elt (buffers server) buffer) nil "bufnum ~d already free." buffer)
      (setf free-buffer (elt (buffers server) buffer))
      (setf (elt (buffers server) buffer) nil))
    (with-sync-or-call-handle (server free-buffer "/b_free" complete-handler) 
      (send-message server "/b_free" buffer))
    free-buffer))

(defmethod buffer-free ((buffer buffer) &key server complete-handler)
  (buffer-free (bufnum buffer) :server (or server (slot-value buffer 'server)) :complete-handler complete-handler))

(defmethod free ((buffer buffer))
  (buffer-free buffer))

(defun buffer-normalize (buffer &key (server *s*) (new-max 1.0) wavetable-p complete-handler)
  (with-sync-or-call-handle (server buffer "/b_gen" complete-handler)
    (send-message server "/b_gen" (floatfy buffer) (if wavetable-p "wnormalize" "normalize") new-max)))

(defun buffer-zero (buffer &key (server *s*) complete-handler)
  (with-sync-or-call-handle (server buffer "/b_zero" complete-handler)
    (send-message (server buffer) "/b_zero" (bufnum buffer) 0)))

(defmethod buffer-dur ((buffer buffer))
  "Get the duration in seconds of BUFFER."
  (/ (frames buffer) (sr buffer)))




(defun buffer-get (buffer index &optional action)
  "Get the frame at INDEX from BUFFER. ACTION can be a function of one argument that is called on the result; without it, the value is simply returned.

To get more than one frame from a buffer, functions like `buffer-to-list' and `buffer-to-array' are generally preferred. Additionally, since this function is synchronous, it should not be called in the reply thread."
  (let* ((bufnum (bufnum buffer))
	 (server (server buffer))
	 (result nil)
	 (handle (or action (lambda (value) (setf result value)))))
    (bt:with-lock-held ((server-lock server))
      (let* ((handlers (gethash (list "/b_set" bufnum index) (buffer-get-handlers server))))
	(setf (gethash (list "/b_set" bufnum index) (buffer-get-handlers server)) (append handlers (list handle)))))
    (send-message server "/b_get" bufnum index)
    (unless action
      (sync (server buffer))
      result)))

(defun buffer-getn (buffer start end &optional action)
  "Get a list of the frames of BUFFER, from START up to END. ACTION can be a function of one argument that is called on the result; without it, the list of frames is simply returned.

Note that the number of frames this function can get is limited by network packet size (which in this implementation is a maximum of 400), so in most cases it is recommended to use functions like `buffer-to-list' or `buffer-to-array' instead. Additionally, since this function is synchronous, it should not be called in the reply thread."
  (let* ((frames (- end start))
	 (bufnum (bufnum buffer))
	 (server (server buffer))
	 (result nil)
	 (handle (or action (lambda (value) (setf result value)))))
    (check-type start (integer 0))
    (assert (>= (* (chanls buffer) (frames buffer)) end)
	    (end)
	    "Buffer index ~a out of range (~a frames * ~a channels = ~a)."
	    end (frames buffer) (chanls buffer) (* (frames buffer) (chanls buffer)))
    (assert (> end start)
	    (start end)
	    "Invalid range requested (END (~a) must be greater than START (~a))."
	    end start)
    (assert (>= 400 frames)
	    nil
	    "Number of requested frames too large (~a requested, <= 400 supported). Use `buffer-get-to-list', `buffer-load-to-list', `buffer-get-to-array', or `buffer-load-to-array' instead."
	    frames)
    (bt:with-lock-held ((server-lock server))
      (let ((handlers (gethash (list "/b_setn" bufnum start frames) (buffer-get-handlers server))))
	(setf (gethash (list "/b_setn" bufnum start frames) (buffer-get-handlers server))
	      (append handlers (list handle)))))
    (send-message server "/b_getn" bufnum start frames)
    (unless action
      (sync (server buffer))
      result)))

(defun buffer-get-to-list (buffer &key (start 0) (end (* (chanls buffer) (frames buffer))))
  "Get a flat list of the frames of BUFFER, from START up to END, defaulting to the entire buffer.

Unlike `buffer-getn', this function is not limited by OSC packet size and can return any number of frames, though it may be slower since it has to make multiple requests over OSC. `buffer-load-to-list' returns the same results and may be faster in setups where it is supported, but `buffer-to-list' should be preferred since it automatically picks the fastest available function.

Note that this is a synchronous function, so you should not call it in the reply thread."
  (check-type start (integer 0))
  (assert (>= (* (chanls buffer) (frames buffer)) end)
	  (end)
	  "Buffer index ~a out of range (~a frames * ~a channels = ~a)"
	  end (frames buffer) (chanls buffer) (* (frames buffer) (chanls buffer)))
  (loop :for s :from start :below end :by 400
	:for e := (min end (+ s 400))
	:append (buffer-getn buffer s e)))

(defun buffer-load-to-list (buffer &key (start 0) (end (* (chanls buffer) (frames buffer))))
  "Write BUFFER to a temporary file, then load the frames from START up to END into a list and return it.

Returns the same results as `buffer-get-to-list' but the use of a temporary file rather than multiple OSC requests means it may be faster in setups where it is supported (i.e. local servers). Generally `buffer-to-list' is preferred since it automatically picks the fastest available function.

Note that this is a synchronous function, so you should not call it in the reply thread."
  (assert (is-local-p (server buffer)) nil "This function only works on local servers.")
  (check-type start (integer 0))
  (assert (>= (* (chanls buffer) (frames buffer)) end)
	  (end)
	  "Buffer index ~a out of range (~a frames * ~a channels = ~a)"
	  end (frames buffer) (chanls buffer) (* (frames buffer) (chanls buffer)))
  (uiop:with-temporary-file (:stream file
			     :pathname path
			     :type "raw"
			     :element-type '(unsigned-byte 32))
    (buffer-write buffer path :format :float :frames (- end start) :start-frame start)
    (file-position file 0)
    (loop :for frame := (read-byte file nil)
	  :while frame
	  :collect (ieee-floats:decode-float32 frame))))

(defun buffer-to-list (buffer &key (start 0) (end (* (chanls buffer) (frames buffer))) get-function)
  "Get a flat list of BUFFER's frames in the range from START up to END, defaulting to the entire buffer. GET-FUNCTION is the function used to acquire the list of frames (usually either `buffer-get-to-list' or `buffer-load-to-list'); it defaults to the fastest one available.

This function (and `buffer-get-to-list', `buffer-load-to-list') simply returns a flat list of the frames in the format SuperCollider stores them in (i.e. interlaced). It may be preferrable to use `buffer-to-array' instead as it automatically divides up the frames into an array of channels.

Additionally, since this is a synchronous function, you should not call it in the reply thread."
  (funcall (or get-function
	       (if (is-local-p (server buffer))
		   #'buffer-load-to-list
		   #'buffer-get-to-list))
	   buffer :start start :end end))

(defun buffer-to-array (buffer &key (start 0) (end (frames buffer)) channels get-function)
  "Get an array of CHANNELS containing the frames of BUFFER, from START up to END, defaulting to the entire buffer. GET-FUNCTION is the function used to acquire the list of frames; it defaults to the fastest one available.

Unlike the `buffer-to-list' functions, this function divides up the frames into their respective channels rather than returning them exactly as they appear in SuperCollider's buffer format (i.e. interlaced).

Note that this is a synchronous function, so you should not call it in the reply thread."
  (check-type start (integer 0))
  (assert (>= (frames buffer) end) (end)
	  "Buffer index ~a out of range (buffer size: ~a)"
	  end (frames buffer))
  (check-type channels (or (integer 0) list))
  (let* ((buf-channels (chanls buffer))
	 (get-function (or get-function (if (is-local-p (server buffer))
					    #'buffer-load-to-list
					    #'buffer-get-to-list)))
	 (channels (or channels (alexandria:iota buf-channels)))
	 (channels-list (alexandria:ensure-list channels))
	 (num-frames (- end start))
	 (array (make-array (if (listp channels)
				(list (length channels) num-frames)
				(list num-frames))
			    :element-type 'single-float))
	 (frames (funcall get-function buffer :start start :end end)))
    (loop :for frame :in frames
	  :for idx :from 0
	  :for chan-num := (mod idx buf-channels)
	  :for frame-num := (truncate (/ idx buf-channels))
	  :for chan-pos := (position chan-num channels-list)
	  :if chan-pos
	    :do (if (listp channels)
		    (setf (aref array chan-pos frame-num) frame)
		    (setf (aref array frame-num) frame)))
    array))

(defun buffer-get-to-array (buffer &key (start 0) (end (frames buffer)) channels)
  "Get an array of CHANNELS containing the frames of BUFFER, from START up to END, defaulting to the entire buffer.

Similar to `buffer-load-to-array' but uses multiple OSC requests to download the buffer, for situations (i.e. non-local servers) where using a temporary file is not possible. Generally `buffer-to-array' is preferred since it automatically picks the fastest available function.

Additionally, since this is a synchronous function, you should not call it in the reply thread."
  (buffer-to-array buffer :start start :end end :channels channels :get-function #'buffer-get-to-list))

(defun buffer-load-to-array (buffer &key (start 0) (end (frames buffer)) channels)
  "Get an array of CHANNELS containing the frames of BUFFER, from START up to END, defaulting to the entire buffer.

Similar to `buffer-get-to-array' but uses a temporary file a la `buffer-load-to-list', meaning it may be faster in setups (i.e. local servers) that support it. Generally `buffer-to-array' is preferred since it automatically picks the fastest available function.

Additionally, since this is a synchronous function, you should not call it in the reply thread."
  (buffer-to-array buffer start end channels #'buffer-load-to-list))

(defun buffer-set (buffer index value)
  (send-message (server buffer) "/b_set" (bufnum buffer) index value)
  (sync (server buffer)))

(defun buffer-setn (buffer data)
  (multiple-value-bind (repeat rest-message-len)
      (floor (length data) 1024)
    (let ((server (server buffer)))
      (dotimes (i repeat)
	(let ((msg (subseq data (* i 1024) (+ (* i 1024) 1024))))
	  (apply #'send-message server (append (list "/b_setn" (bufnum buffer) (* i 1024) 1024) msg))))
      (unless (zerop rest-message-len)
	(let ((msg (subseq data (* repeat 1024) (+ (* repeat 1024) rest-message-len))))
	  (apply #'send-message server (append (list "/b_setn" (bufnum buffer) (* repeat 1024) rest-message-len) msg)))))
    (sync (server buffer))
    buffer))


(defun buffer-copy (bufnum-src bufnum-dst &optional (start-dst 0) (start-src 0) (nframes -1))
  (apply #'send-message
	 *s*
	 (append (list "\b_gen" bufnum-dst "copy" start-dst bufnum-src start-src nframes)))
  (sync *s*))

(defun buffer-fill (buffer wave amplitudes &key frequencies phases
					     (server *s*) (normalize t) (as-wavetable t) (clear-first t))
  "Fill BUFFER with either: a series of sine wave partials, when WAVE is `:sine'; or a series of chebyshev polynomials, when WAVE is `:cheby'. 

In the case of sine wave partials, AMPLITUDES is a list whose first value specifies the amplitude of the first partial, the second value specifies the amplitude of the second partial, and so on. FREQUENCIES is a list of partial frequencies, in cycles per buffer. It's assumed to be an integer series of partials if the list is not supplied. When frequencies are specified, a list of PHASES can also be used where each partial may have a nonzero starting phase.

Chebyshev polynomials can be defined as cheby(n) = amplitude * cos(n * acos(x)). In this case, the first value of AMPLITUDES specifies the amplitude for n = 1, the second value specifies the amplitude for n = 2, and so on. FREQUENCIES and PHASES are ignored.

When NORMALIZE is T, the peak amplitude of the wave is normalized to 1.0. If WAVETABLE is set to T, the buffer is written in a special wavetable format so that it can be read by interpolating oscillators. Setting CLEAR-FIRST to T clears the buffer before new partials are written into it. If NIL, the new partials are summed with the existing contents of the buffer."
  (apply #'send-message
	 (server buffer)
	 (append (list "/b_gen" (bufnum buffer)
		       (ecase wave
			 (:cheby "cheby")
			 (:sine (cond ((and frequencies phases) "sine3")
				      (frequencies "sine2")
				      (t "sine1"))))
		       (+ (if normalize 1 0)
			  (if as-wavetable 2 0)
			  (if clear-first 4 0)))
		 (if (and (eql wave :sine) frequencies)
		     (append frequencies amplitudes phases)
		     amplitudes)))
  (sync server))

;; see http://doc.sccode.org/Classes/Wavetable.html#Advanced%20notes:%20wavetable%20format
(defun vector-in-wavetable-format (sequence)
  "Convert a sequence of numbers to a vector in SuperCollider's wavetable format."
  (let* ((len (length sequence))
	 (vec (make-array (list (* len 2)))))
    (dotimes (i len vec)
      (let ((a0 (nth-wrap i sequence))
	    (a1 (nth-wrap (1+ i) sequence)))
	(setf (elt vec (* i 2)) (- (* 2 a0) a1)
	      (elt vec (1+ (* i 2))) (- a1 a0))))))

(defun buffer-read-as-wavetable (path &key bufnum (server *s*))
  "Read a soundfile located at PATH as a wavetable."
  (let* ((tmp-buf (prog1 (buffer-read path :server server)
                    (sync)))
         (full-path (slot-value tmp-buf 'path))
         (file-frames (slot-value tmp-buf 'frames))
         (powers-of-two '#.(mapcar (lambda (x) (expt 2 x)) (alexandria:iota 16 :start 1)))
         (num-frames (nth (or (position-if (lambda (x) (>= x file-frames)) powers-of-two)
			      15)
			  powers-of-two))
         (frames (prog1 (buffer-to-array tmp-buf :channels 0)
                   (buffer-free tmp-buf)))
         (buffer (buffer-alloc (* 2 num-frames) :bufnum bufnum :server server)))
    (buffer-setn buffer (coerce (vector-in-wavetable-format (linear-resample frames num-frames)) 'list))
    (setf (slot-value buffer 'path) full-path)
    buffer))
