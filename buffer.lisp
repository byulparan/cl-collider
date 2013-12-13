
(in-package #:sc)

(defclass buffer ()
  ((bufnum :initarg :bufnum :initform nil :accessor bufnum)
   (frames :initarg :frames :initform nil :accessor frames)
   (chanls :initarg :chanls :initform nil :accessor chanls)
   (sr :initarg :sr :initform nil :accessor sr)
   (path :initarg :path :initform nil :accessor path)
   (server :initarg :server :initform nil :accessor server)))

(defmethod print-object ((self buffer) stream)
  (format stream "#<BUFFER :server ~a :bufnum ~a :frames ~a :channels ~a :samplerate ~a :path ~s>"
	  (server self) (bufnum self) (frames self) (chanls self) (sr self) (path self)))

(defmethod initialize-instance :after ((self buffer) &key)
  (setf (gethash (bufnum self) (buffers (server self))) self))

(defmethod floatfy ((buffer buffer))
  (floatfy (bufnum buffer)))

(defun buffer-read (server path &key bufnum)
  (let ((file-path (su:full-pathname path)))
    (assert (probe-file file-path) (path) "not have file< ~a >" file-path)
    (let* ((bufnum (if bufnum bufnum (get-next-buffer-number server)))
	   (new-buffer (make-instance 'buffer :path file-path
					      :bufnum bufnum
					      :server server))
	   (msg (list "/b_allocRead" bufnum file-path 0 -1 (osc:encode-message "/b_query" bufnum))))
      (apply #'send-message server msg)
      (sync *s*)
      new-buffer)))


(defun buffer-alloc (server frames &key (chanls 1) bufnum)
  (let* ((bufnum (if bufnum bufnum (get-next-buffer-number server)))
	 (new-buffer (make-instance 'buffer :bufnum bufnum :frames frames :chanls chanls :server server)))
    (let ((msg (list "/b_alloc" bufnum (floor frames) (floor chanls)
		     (osc:encode-message "/b_query" bufnum))))
      (apply #'send-message server msg)
      (sync *s*)
      new-buffer)))

(defun buffer-write (buffer path &key (num-frames -1) (start-frames 0) (format :int24))
  "Make audio-file from Buffer."
  (let ((bufnum (bufnum buffer))
	(server (server buffer))
	(file-path (su:full-pathname path)))
    (send-message server "/b_write" bufnum file-path (pathname-type file-path) (ecase format
										 (:int16 "int16")
										 (:int24 "int24")
										 (:float "float")
										 (:double "double"))
		  num-frames start-frames 0)
    (sync *s*)
    buffer))


(defun buffer-get (buffer index &optional action)
  (let ((bufnum (bufnum buffer))
	(server (server buffer)))
    (let ((result nil))
      (setf (gethash (list "/b_set" bufnum index) (buffer-get-handlers server)) (if action action #!(setf result %)))
      (send-message server "/b_get" bufnum index)
      (unless action
	(sync *s*)
	result))))

(defun buffer-get-list (buffer start frames &optional action)
  (assert (>= (frames buffer) (+ start frames)) nil "index out of range(~a) : ~a" (frames buffer) (+ start frames))
  (let ((bufnum (bufnum buffer))
	(server (server buffer)))
    (let ((result nil))
      (setf (gethash (list "/b_setn" bufnum start frames) (buffer-get-handlers server)) (if action action #!(setf result %)))
      (send-message server "/b_getn" bufnum start frames)
      (unless action
	(sync *s*)
	result))))

(defun buffer-set (buffer index value)
  (send-message (server buffer) "/b_set" (bufnum buffer) index value))

(defun buffer-set-list (buffer data)
  (multiple-value-bind (repeat rest-message-len)
      (floor (length data) 1024)
    (let ((server (server buffer)))
      (dotimes (i repeat)
	(let ((msg (subseq data (* i 1024) (+ (* i 1024) 1024))))
	  (apply #'send-message server (append (list "/b_setn" (bufnum buffer) (* i 1024) 1024) msg))))
      (unless (zerop rest-message-len)
	(let ((msg (subseq data (* repeat 1024) (+ (* repeat 1024) rest-message-len))))
	  (apply #'send-message server (append (list "/b_setn" (bufnum buffer) (* repeat 1024) rest-message-len) msg)))))
    buffer))

(defun b-cheby-msg (buffer data &optional (normalize t) (as-wavetable t) (clear-first t))
  (append (list "/b_gen" (bufnum buffer) "cheby" (+ (if normalize 1 0) (if as-wavetable 2 0) (if clear-first 4 0))) data))



