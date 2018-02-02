(in-package :sc-osc)

(defun join-thread (thread)
  #+ccl (bt:join-thread thread)
  #+sbcl (sb-thread:join-thread thread :default nil))

(defclass osc-device ()
  ((reply-handle-table
    :initform (make-hash-table :test #'equal)
    :reader reply-handle-table)
   (host
    :initarg :host
    :reader host)
   (port
    :initarg :port
    :reader port)
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
		  :host host
		  :port port
		  :socket (usocket:socket-connect nil nil
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
  (let ((msg (apply #'encode-message message)))
    (usocket:socket-send (socket osc-device) msg (length msg)
			 :port (port osc-device)
			 :host (host osc-device))
    (values)))

(defun send-bundle (timestamp osc-device &rest messages)
  (let ((msg (encode-bundle messages timestamp)))
    (usocket:socket-send (socket osc-device) msg (length msg)
			 :port (port osc-device)
			 :host (host osc-device))
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
     (let ((buffer (make-array 2048 :element-type '(unsigned-byte 8))))
       (loop
	 do #+ecl
	    (setf buffer (make-array 2048 :element-type '(unsigned-byte 8))) ;; maybe This is ECL/USocket bug 
	    (multiple-value-bind (buffer length host port)
		(usocket:socket-receive (socket osc-device) buffer (length buffer))
	      (declare (ignore host port length))
	      (let* ((message (decode-bundle buffer) )
		     (handler (gethash (car message) (reply-handle-table osc-device))))
		(if handler (handler-case (apply handler (cdr message))
			     (error (c) (format t "Error ~a on received message ~s ~%" c (car message))))
		  (format t "Reply handler not found: ~a [ ~{~a ~}]~%" (car message) (cdr message))))))))
   :name (format nil "OSC device receive thread")))

