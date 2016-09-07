(in-package :osc)

(defun join-thread (thread)
  #+ccl (bt:join-thread thread)
  #+sbcl (sb-thread:join-thread thread :default nil))

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
   (listening-thread
    :accessor listening-thread)
   (local-port
    :initarg :local-port
    :reader local-port)))


(defun osc-device (host port &key local-port)
  (let ((device (make-instance 'osc-device
			       :socket (usocket:socket-connect host port
							       :protocol :datagram
							       :local-host "127.0.0.1"
							       :local-port local-port))))
    #+sbcl (setf (sb-bsd-sockets:sockopt-send-buffer (usocket:socket (socket device)))
		 usocket:+max-datagram-packet-size+)
    #+ccl
    (let ((result (ccl::int-setsockopt (ccl:socket-os-fd (usocket:socket (socket device)))
				       #$SOL_SOCKET #$SO_SNDBUF
				       usocket:+max-datagram-packet-size+)))
      (assert (zerop result) nil "fail increase socket sndbuf"))
    (when local-port
      (setf (listening-thread device) (make-listening-thread device)))
    (setf (status device) :running)
    device))

(defun add-osc-responder (osc-device cmd-name f)
  (setf (gethash cmd-name (reply-handle-table osc-device)) f))

(defun remove-osc-responder (osc-device cmd-name)
  (remhash cmd-name (reply-handle-table osc-device)))

(defun send-message (osc-device &rest message)
  (let ((msg (apply #'osc:encode-message message)))
    (usocket:socket-send (socket osc-device) msg (length msg))
    (values)))

(defun send-bundle (timestamp osc-device &rest messages)
  (let ((msg (osc:encode-bundle messages timestamp)))
    (usocket:socket-send (socket osc-device) msg (length msg))
    (values)))

(defun close-device (osc-device)
  (when (listening-thread osc-device)
    (bt:destroy-thread (listening-thread osc-device))
    (join-thread (listening-thread osc-device)))
  (usocket:socket-close (socket osc-device))
  (setf (status osc-device) :not-running))

(defun make-listening-thread (osc-device)
  (bt:make-thread
   (lambda ()
     (labels ((listen-fun ()
		(let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
		  (handler-case 
		      (loop
			do (multiple-value-bind (buffer length host port)
			       (usocket:socket-receive (socket osc-device) buffer (length buffer))
			     (declare (ignore host port length))
			     (multiple-value-bind (message timetag)
				 (osc:decode-bundle buffer)
			       (declare (ignore timetag))
			       (alexandria:if-let ((f (gethash (car message) (reply-handle-table osc-device))))
				 (apply f (cdr message))
				 (format t "not found reply handler : ~a [ ~{~a ~}]~%" (car message) (cdr message))))))
		    (error (c) (format t "error ~a in receive-thread-for-OSC-device~%" c)
		      (listen-fun))))))
       (listen-fun)))
   :name (format nil "receive-thread-for-OSC-device")))

