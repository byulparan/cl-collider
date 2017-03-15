
(in-package #:sc)

(defugen (play-buf "PlayBuf")
    (chanls &optional (bufnum 0) (rate 1.0) &key (trig 1.0) (start-pos 0.0) (loop 0.0) (act :no-action))
  ((:ar (multinew new 'multiout-ugen chanls bufnum rate trig start-pos loop (act act)))
   (:kr (multinew new 'multiout-ugen chanls bufnum rate trig start-pos loop (act act)))))



(defugen (tgrains "TGrains")
    (chanls &optional (trigger 0) (bufnum 0) (rate 1) (center-pos 0)
		  (dur 0.1) (pan 0.0) (amp 0.1) (interp 4))
  ((:ar (progn (when (< chanls 2) (error "TGrains needs at least two channels."))
	       (multinew new 'multiout-ugen chanls trigger bufnum rate center-pos dur pan amp interp)))))

(defugen (tgrains2 "TGrains2")
    (chanls &optional (trigger 0) (bufnum 0) (rate 1) (center-pos 0)
	    (dur .1) (pan 0) (amp .1) (attk .5) (dec .5) (interp 4))
  ((:ar (progn (when (< chanls 2) (error "TGrains needs at least two channels."))
	       (multinew new 'multiout-ugen chanls trigger bufnum rate center-pos dur pan amp
			 attk dec interp)))))

(defugen (tgrains3 "TGrains3")
    (chanls &optional (trigger 0) (bufnum 0) (rate 1) (center-pos 0)
	    (dur .1) (pan 0.0) (amp .1) (attk .5) (dec .5) (window 1) (interp 4))
  ((:ar (progn (when (< chanls 2) (error "TGrains needs at least two channels."))
	       (multinew new 'multiout-ugen chanls trigger bufnum rate center-pos dur pan amp
			 attk dec window interp)))))

(defugen (buf-rd "BufRd")
    (chanls &optional (bufnum 0) (phase 0.0) (loop 1) (interp 2))
  ((:ar (multinew new 'multiout-ugen chanls bufnum phase loop interp))
   (:kr (multinew new 'multiout-ugen chanls bufnum phase loop interp)))
  :check-fn #'check-when-audio)


(defugen (buf-wr "BufWr")
    (input-array &optional (bufnum 0) (phase 0.0) (loop 1.0))
  ((:ar (multinew-list new 'ugen (append (list bufnum phase loop) (alexandria:ensure-list input-array))))
   (:kr (multinew-list new 'ugen (append (list bufnum phase loop) (alexandria:ensure-list input-array)))))
  :check-fn #'check-when-audio)


(defugen (record-buf "RecordBuf")
    (input-array &optional (bufnum 0) &key (offset 0.0) (rec-level 1.0) (pre-level 0.0)
		 (run 1.0) (loop 1.0) (trig 1.0) (act :no-action))
  ((:ar (multinew-list new 'ugen (append (list bufnum offset rec-level pre-level run loop trig (act act))
					 (alexandria:ensure-list input-array))))
   (:kr (multinew-list new 'ugen (append (list bufnum offset rec-level pre-level run loop trig (act act))
					 (alexandria:ensure-list input-array))))))

(defugen (scope-out "ScopeOut") (input-array &optional (bufnum 0))
  ((:ar (progn (multinew-list new 'ugen (cons bufnum (alexandria:ensure-list input-array))) 0))
   (:kr (progn (multinew-list new 'ugen (cons bufnum (alexandria:ensure-list input-array))) 0))))

(defugen (scope-out2 "ScopeOut2") (input-array &optional (scope-num 0) (max-frames 4096) scope-frames)
  ((:ar (progn (multinew-list new 'ugen (append (list scope-num max-frames (if scope-frames scope-frames max-frames))
					      (alexandria:ensure-list input-array))) 0))
   (:kr (progn (multinew-list new 'ugen (append (list scope-num max-frames (if scope-frames scope-frames max-frames))
					      (alexandria:ensure-list input-array))) 0))))

(defun tap.ar (&optional (bufnum 0) (chanls 1) (delay 0.2))
  (let ((n (mul delay (neg (sample-rate.ir)))))
    (play-buf.ar chanls bufnum 1 :trig 0 :start-pos n :loop 1)))



;;;
;;; for LocalBuf
;;; 

(defun make-max-local-bufs ()
  (ugen-new "MaxLocalBufs" :scalar 'ugen #'identity :bipolar 0))

(defun increment (max-local-bufs)
  (incf (nth 0 (inputs max-local-bufs))))



(defclass local-buf (width-first-ugen)
  ())

(defmethod frames ((buffer local-buf))
  (nth 1 (inputs buffer)))

(defmethod chanls ((buffer local-buf))
  (nth 0 (inputs buffer)))

(defmethod new1 ((ugen local-buf) &rest inputs)
  (let ((max-local-bufs (max-local-bufs (synthdef ugen))))
    (unless max-local-bufs
      (setf max-local-bufs (make-max-local-bufs))
      (setf (max-local-bufs (synthdef ugen)) max-local-bufs))
    (increment max-local-bufs)
    (setf (inputs ugen) (append inputs (list max-local-bufs)))
    (add-to-synth ugen)
    (alexandria:appendf (width-first-ugens (synthdef ugen)) (list ugen))
    ugen))

(defun local-buf (&optional (frames 1) (chanls 1))
  (multinew
   (lambda (cls &rest inputs) (apply #'ugen-new "LocalBuf" :scalar cls #'identity :bipolar inputs))
   'local-buf
   chanls frames))



(defun set-buf (local-buf values &optional (offset 0))
  (unless (every 'numberp (cons offset values))
    (error "VALUES and OFFSET must all be numbers."))
  (multinew-list (lambda (cls &rest inputs) (apply #'ugen-new "SetBuf" :scalar cls #'identity :bipolar inputs))
		 'width-first-ugen
		 (append (list local-buf offset (length values)) values)))

(defun local-buf-list (values)
  (labels ((shape (list)
	     (unless (or (every 'numberp list)
			 (and (every 'listp list)
			      (apply #'= (mapcar (lambda (obj) (length obj)) list))
			      (every #'identity
				     (mapcar (lambda (obj) (every 'numberp obj)) list))))
	       (error "Invalid shape in local-buf-list."))
	     (if (every 'numberp list) (list (length list) 1)
		 (list (length (car list)) (length list)))))
    (let ((buf (apply 'local-buf (shape values))))
      (set-buf buf (alexandria:flatten (flop values)))
      buf)))

(defun clear-buf (local-buf)
  (multinew (lambda (cls &rest inputs) (apply #'ugen-new "ClearBuf" :scalar cls #'identity :bipolar inputs))
	    'width-first-ugen
	    local-buf))




