(in-package :osc)

(defun join-thread (thread)
  #+ccl (bt:join-thread thread)
  #+sbcl (sb-thread:join-thread thread :default nil))

(defstruct (net-addr (:constructor net-addr (host port)))
  host
  port)

(defclass osc-device ()
  ((reply-handle-table
    :initform (make-hash-table :test #'equal)
    :reader reply-handle-table)
   (status
    :initform :not-running
    :accessor status)
   (socket
    :initarg :socket
    :reader socket)
   (send-q
    :initform (make-instance 'chanl:bounded-channel :size 1024)
    :reader send-q)
   (send-thread
    :accessor send-thread)
   (listening-thread
    :accessor listening-thread)
   (local-port
    :initarg :local-port
    :reader local-port)))


(defun osc-device (local-port &optional listen-p)
  (let ((device (make-instance 'osc-device
			       :socket (usocket:socket-connect nil nil 
							       :protocol :datagram
							       :local-port local-port))))
    (setf (send-thread device) (make-send-thread device))
    (when listen-p
      (setf (listening-thread device) (make-listening-thread device)))
    (setf (status device) :running)
    device))

(defun check-running (osc-device)
  (unless (eql (status osc-device) :running)
    (error "osc-device is not active")))

(defun add-osc-responder (osc-device cmd-name f)
  (check-running osc-device)
  (setf (gethash cmd-name (reply-handle-table osc-device)) f))

(defun remove-osc-responder (osc-device cmd-name)
  (check-running osc-device)
  (remhash cmd-name (reply-handle-table osc-device)))

(defun send-message (osc-device net-addr &rest message)
  (check-running osc-device)
  (let ((msg (apply #'osc:encode-message message)))
    (chanl:send (send-q osc-device) (cons msg net-addr))
    (values)))

(defun send-bundle (timestamp osc-device net-addr  &rest messages)
  (check-running osc-device)
  (let ((msg (osc:encode-bundle messages timestamp)))
    (chanl:send (send-q osc-device) (cons msg net-addr))
    (values)))

(defun close-device (osc-device)
  (check-running osc-device)
  (bt:destroy-thread (send-thread osc-device))
  (join-thread (send-thread osc-device))
  (when (listening-thread osc-device)
    (bt:destroy-thread (listening-thread osc-device))
    (join-thread (listening-thread osc-device)))
  (usocket:socket-close (socket osc-device))
  (setf (status osc-device) :not-running))

(defun make-send-thread (osc-device)
  (bt:make-thread
   (lambda ()
     (loop 
       (let* ((recv-data (chanl:recv (send-q osc-device)))
	      (message (car recv-data))
	      (addr (cdr recv-data)))
	 (usocket:socket-send (socket osc-device) message (length message) :host (net-addr-host addr)
									   :port (net-addr-port addr)))))
   :name (format nil "send-thread-for-OSC-device")))

(defun make-listening-thread (osc-device)
  (bt:make-thread
   (lambda ()
     (labels ((listen-fun ()
		(let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
		  (handler-case 
		      (loop
			do (multiple-value-bind (buffer length host port)
			       (usocket:socket-receive (socket osc-device) buffer (length buffer))
			     (declare (ignore length))
			     (multiple-value-bind (message timetag)
				 (osc:decode-bundle buffer)
			       (declare (ignore timetag))
			       (alexandria:if-let ((f (gethash (car message) (reply-handle-table osc-device))))
				 (funcall f (cdr message) (net-addr host port))
				 (format t "not found reply handler : ~a [ ~{~a ~}]~%" (car message) (cdr message))))))
		    (error (c) (format t "error ~a in receive-thread-for-OSC-device~%" c)
		      (listen-fun))))))
       (listen-fun)))
   :name (format nil "receive-thread-for-OSC-device")))

