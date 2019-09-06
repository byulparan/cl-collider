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
      (apply #'send-message server (list "/b_alloc" bufnum  (floor frames) (floor chanls)
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

(defun buffer-read-channel (path &key (channels -1) bufnum (server *s*))
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

(defmethod buffer-free ((buffer buffer) &key (server *s*) complete-handler)
  (buffer-free (bufnum buffer) :server server :complete-handler complete-handler))

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
  (let ((bufnum (bufnum buffer))
	(server (server buffer)))
    (let* ((result nil)
	   (handle (if action action (lambda (value) (setf result value)))))
      (bt:with-lock-held ((server-lock server))
	(let* ((handlers (gethash (list "/b_set" bufnum index) (buffer-get-handlers server))))
	  (setf (gethash (list "/b_set" bufnum index) (buffer-get-handlers server)) (append handlers (list handle)))))
      (send-message server "/b_get" bufnum index)
      (unless action
	(sync (server buffer))
	result))))

(defun buffer-getn (buffer start frames &optional action)
  (assert (>= (frames buffer) (+ start frames)) nil "Buffer index ~a out of range (buffer size: ~a)" (+ start frames) (frames buffer))
  (assert (>= 400 frames) nil "requested frames too large. use `buffer-load-to-list` or `buffer-get-to-list`.")
  (let ((bufnum (bufnum buffer))
	(server (server buffer)))
    (let* ((result nil)
	   (handle (if action action (lambda (value) (setf result value)))))
      (bt:with-lock-held ((server-lock server))
	(let* ((handlers (gethash (list "/b_setn" bufnum start frames) (buffer-get-handlers server))))
	  (setf (gethash (list "/b_setn" bufnum start frames) (buffer-get-handlers server))
	    (append handlers (list handle)))))
      (send-message server "/b_getn" bufnum start frames)
      (unless action
	(sync (server buffer))
	result))))

(defun buffer-get-to-list (buffer &optional (start 0) (frames (slot-value buffer 'frames)))
  "Get a list of the frames of BUFFER. Unlike `buffer-get-list', this function is not limited by OSC packet size and can return any number of frames, though it may be slower.

Note that this is a synchronous function, so you should not call it in the reply thread."
  (let ((end (+ start frames)))
    (assert (>= (frames buffer) end) nil "Buffer index ~a out of range (buffer size: ~a)" (+ start frames) (frames buffer))
    (loop :while (< start end)
       :append
         (let ((dec (min 400 (- end start))))
           (prog1
               (buffer-getn buffer start dec)
             (incf start dec))))))

(defun buffer-load-to-list (buffer &optional (start 0) (frames (slot-value buffer 'frames)))
  "Write BUFFER to a temporary file, then load the values back into a list and return it. The values are from index START and for the number of FRAMES, if provided, or otherwise until the end of the buffer. ACTION is a function which will be passed the resulting list as an argument and evaluated once the file has been read. This is synchronous function, do not call in reply thread."
  (assert (is-local-p (server buffer)) nil "This function only work on localhost server.")
  (when frames (assert (>= (frames buffer) (+ start frames)) nil
		       "Buffer index ~a out of range (buffer size: ~a)"
		       (+ start frames) (frames buffer)))
  (uiop:with-temporary-file (:stream file
			     :pathname path
			     :type "raw"
			     :element-type '(unsigned-byte 32))
    (buffer-write buffer path :format :float :frames (or frames -1) :start-frame start)
    (file-position file 0)
    (loop :for frame := (read-byte file nil)
	  :while frame
	  :collect (ieee-floats:decode-float32 frame))))

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


;;; wavetable
(defun wavetable (buffer wave data &key (server *s*) (normalize t) (as-wavetable t) (clear-first t))
  (apply #'send-message
	 (server buffer) 
	 (append (list "/b_gen" (bufnum buffer)
		       (ecase wave
			 (:cheby "cheby")
			 (:sine1 "sine1"))
		       (+ (if normalize 1 0)
			  (if as-wavetable 2 0)
			  (if clear-first 4 0)))
		 data))
  (sync server))

;; see http://doc.sccode.org/Classes/Wavetable.html#Advanced%20notes:%20wavetable%20format
(defun list-in-wavetable-format (list)
  "Convert a list of numbers LIST to SuperCollider's wavetable format."
  (loop :for i :from 0 :below (length list)
     :append (let ((a0 (nth-wrap i list))
                   (a1 (nth-wrap (1+ i) list)))
               (list (- (* 2 a0) a1) (- a1 a0)))))

(defun buffer-read-as-wavetable (path)
  "Read a soundfile located at PATH as a wavetable."
  (let* ((tmp-buf (prog1 (buffer-read path)
                    (sync)))
         (full-path (slot-value tmp-buf 'path))
         (file-frames (slot-value tmp-buf 'frames))
         (powers-of-two (mapcar (lambda (x) (expt 2 (1+ x))) (alexandria:iota 16)))
         (num-frames (nth (position-if (lambda (x) (>= x file-frames)) powers-of-two) powers-of-two))
         (frames (prog1
                     (buffer-get-to-list tmp-buf)
                   (buffer-free tmp-buf)))
         (buffer (buffer-alloc (* 2 num-frames))))
    (buffer-setn buffer (list-in-wavetable-format (coerce (linear-resample frames num-frames) 'vector)))
    (setf (slot-value buffer 'path) full-path)
    buffer))
