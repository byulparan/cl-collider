;;; in CCL send via "chanl queue". but SBCL use socket directly.
;;; because, sending via chanl is too many slow in SBCL.(many latency..)
;;; As far as I know, UDP socket buffer is thread-safe. so send-message in SBCL is thread-safe.
;;; but, I think use "chanl queue" is better way.

(in-package :sc)

(defvar *external-servers*)
(defgeneric host (scsynth-server))
(defgeneric port (scsynth-server))

(defclass message ()
  ((message :initarg :message :reader message)
   (address :initarg :address :reader address)
   (port :initarg :port :reader port)))

(defclass osc-device ()
  ((name :initarg :name :reader name)
   (socket :initarg :socket :reader socket)
   (reply-handle-table :initarg :reply-handle-table :reader reply-handle-table)
   #+ccl   (send-q :initform (make-instance 'chanl:bounded-channel :size 1024) :reader send-q)
   #+ccl   (send-thread :accessor send-thread)
   (listening-thread :accessor listening-thread)
   (print-log-p :initarg :print-log-p :initform nil :accessor print-log-p)))

(defun make-osc-device (name)
  (let ((device (make-instance 'osc-device
			       :name name 
			       :socket (usocket:socket-connect nil nil
							       :protocol :datagram)
			       :reply-handle-table (make-hash-table :test #'equalp))))
    #+ccl    (setf (send-thread device) (make-send-thread device))
    (setf (listening-thread device) (make-listening-thread device))
    device))

(defun close-osc-device (osc-device)
  #+ccl  (bt:destroy-thread (send-thread osc-device))
  (bt:destroy-thread (listening-thread osc-device))
  #+ccl  (join-thread (send-thread osc-device))
  (join-thread (listening-thread osc-device))
  (usocket:socket-close (socket osc-device))
  (values))

(defun insert-reply-handler (osc-device cmd-name f)
  (setf (gethash cmd-name (reply-handle-table osc-device)) f))

(defun remove-reply-handler (osc-device cmd-name)
  (remhash cmd-name (reply-handle-table osc-device)))

(defun send-message-to-osc-device (osc-device address port &rest message)
  (let ((msg (apply #'osc:encode-message message)))
    #+ccl(chanl:send (send-q osc-device) (make-instance 'message :message msg
								 :address address
								 :port port))
    #+sbcl(usocket:socket-send (socket osc-device) msg (length msg) :host address :port port)
    (values)))

(defun send-bundle-to-osc-device (osc-device address port timestamp &rest messages)
  (let ((msg (osc:encode-bundle messages timestamp)))
    #+ccl(chanl:send (send-q osc-device) (make-instance 'message :message msg
								 :address address
								 :port port))
    #+sbcl(values (usocket:socket-send (socket osc-device) msg (length msg) :host address :port port))))

#+ccl
(defun make-send-thread (osc-device)
  (bt:make-thread
   (lambda ()
     (loop 
       (let* ((recv-message (chanl:recv (send-q osc-device)))
	      (message (message recv-message))
	      (address (address recv-message))
	      (port (port recv-message)))
	 (usocket:socket-send (socket osc-device) message (length message) :host address :port port))))
   :name (format nil "send-thread-for-~a" (name osc-device))))



(defun print-log (address port message)
  (format t "[~{ ~a~} ] from ~a~%" message
	  (find-if #!(and (string= (host %) #+sbcl (usocket:vector-quad-to-dotted-quad address)
					    #+ccl (usocket:hbo-to-dotted-quad address))
			  (= (port %) port)) *external-servers*)))

(defun make-listening-thread (osc-device)
  (bt:make-thread
   (lambda ()
     (labels ((listen-fun ()
		(let ((buffer (make-sequence '(vector (unsigned-byte 8)) 1024)))
		  (handler-case 
		      (loop
			do (multiple-value-bind (buffer length address port)
			       (usocket:socket-receive (socket osc-device) buffer (length buffer))
			     (declare (ignore length))
			     (multiple-value-bind (message timetag)
				 (osc:decode-bundle buffer)
			       (declare (ignore timetag))
			       (when (print-log-p osc-device)
				 (print-log address port message))
			       (alexandria:if-let ((f (gethash (car message) (reply-handle-table osc-device))))
				 (funcall f (cdr message))
				 (format t "not found reply handler : ~a [ ~{~a ~}]~%" (car message) (cdr message))))))
		    (error (c) (format t "error ~a in receive-thread-for ~a~%" c (name osc-device))
		      (listen-fun))))))
       (listen-fun)))
   :name (format nil "receive-thread-for-~a" (name osc-device))))

