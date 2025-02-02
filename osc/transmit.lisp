(in-package :sc-osc)

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
    :initform nil
    :accessor listening-thread)
   (local-port
    :initarg :local-port
    :reader local-port)))


(defun osc-device (host port &key (local-host "0.0.0.0") local-port)
  (let ((device (make-instance 'osc-device
		  :host host
		  :port port
		  :socket (usocket:socket-connect nil nil
						  :protocol :datagram
						  :local-host local-host
						  :local-port local-port))))
    #+sbcl (setf (sb-bsd-sockets:sockopt-send-buffer (usocket:socket (socket device)))
	     usocket:+max-datagram-packet-size+)
    #+(or ccl lispworks)
    (let* ((sol-socket #+linux 1 #-linux #xffff)
	   (so-sndbuf #+linux 7 #-linux #x1001))
      (let ((result #+ccl (ccl::int-setsockopt (ccl:socket-os-fd (usocket:socket (socket device)))
					       sol-socket so-sndbuf
					       usocket:+max-datagram-packet-size+)
		    #+lispworks (cffi:with-foreign-objects ((max-len :int))
				  (setf (cffi:mem-ref max-len :int) usocket:+max-datagram-packet-size+)
				  (cffi:foreign-funcall "setsockopt" :int (usocket:socket (socket device))
								     :int sol-socket
								     :int so-sndbuf
								     :pointer max-len
								     :int 4
								     :int))))
	(assert (zerop result) nil "fail increase socket sndbuf")))
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
  (assert (eql (status osc-device) :running) nil "~a not running" osc-device)
  (let* ((socket (socket osc-device)))
    (when (listening-thread osc-device)
      (let* ((msg (encode-message "/done" "/quit")))
	(usocket:socket-send socket msg (length msg)
			     :host "127.0.0.1"
			     :port (usocket:get-local-port socket)))
      (bt:join-thread (listening-thread osc-device)))
    (usocket:socket-close (socket osc-device)))
  (setf (status osc-device) :not-running))

(defparameter *debugging-make-listening-thread* nil)

(defun make-listening-thread (osc-device)
  (bt:make-thread
   (lambda ()
     (setf *random-state* (make-random-state t))
     (let ((running-p t)
           #+lispworks(starting-calls 100)
	   (buffer (make-array 2048 :element-type '(unsigned-byte 8))))
       (loop while running-p
	     do 
	     #+ecl (setf buffer (make-array 2048 :element-type '(unsigned-byte 8))) ;; maybe This is ECL/USocket bug 
		   (multiple-value-bind (buffer length host port)
		       #+lispworks
                       (if (> starting-calls 0)
                           (progn (decf starting-calls)
				  (ignore-errors 	
				   (usocket:socket-receive (socket osc-device) buffer (length buffer))))
                         (usocket:socket-receive (socket osc-device) buffer (length buffer)))
		     #-lispworks (usocket:socket-receive (socket osc-device) buffer (length buffer))
		     (declare (ignore host port))
                     (if buffer  ; nil when some error inside usocket:socket-receive was caught by the ignore-errors. 
                         (let* ((messages (cdr (decode-bundle buffer))))
                           (loop for message in messages
                                 for handler = (gethash (car message) (reply-handle-table osc-device))
                                 do (if handler (handler-case (progn (apply handler (cdr message))
                                                                     (when (and (string= (car message) "/done")
										(string= (second message) "/quit"))
                                                                       (setf running-p nil)
                                                                       (return)))
                                                  (error (c) (format t "Error ~a on received message ~s ~%" c (car message))))
                                      (if (and (string= (car message) "/done")
                                               (string= (second message) "/quit"))
                                          (progn
                                            (setf running-p nil)
                                            (return))
                                        (format t "Reply handler not found: ~a [ ~{~a ~}]~%" (car message) (cdr message))))))
                       ;; We reach here if we get an error during the first 100 calls. We assume the server not
                       ;; ready yet, so just sleep a little and try again. 
                       (progn 
                         (when *debugging-make-listening-thread*
                           (format t " make-listening-thread thread sleeping, error is : ~a~%" length))
                         (sleep 0.2)))))))
   :name (format nil "OSC device receive thread")))
