
(in-package #:sc)

(defclass bus ()
  ((busnum :initarg :busnum :initform nil :accessor busnum)
   (type :initarg :type :initform :audio)
   (chanls :initarg :chanls :initform nil :accessor chanls)
   (server :initarg :server :initform nil :accessor server)))

(defmethod print-object ((self bus) stream)
  (format stream "#<~s :type ~s :server ~s :busnum ~s :channels ~s>"
          'bus (slot-value self 'type) (server self) (busnum self) (chanls self)))

(defmethod floatfy ((bus bus))
  (floatfy (busnum bus)))

(defun get-next-bus (server &optional (type :audio) (channels 1) busnum)
  (assert (member type '(:audio :control)) (type))
  (labels ((find-consecutive-nil (&optional (type :audio) (channels 1) (start 0))
             (let* ((buses (if (eq type :audio)
                               (audio-buses server)
                               (control-buses server)))
                    (pos (position nil buses :start start)))
               (if (loop :for i :upto (1- channels)
                      :do (when (not (null (elt buses (+ pos i))))
                            (return t)))
                   (find-consecutive-nil type channels (1+ pos))
                   pos))))
    (bt:with-lock-held ((server-lock server))
      (let* ((busnum (or busnum (find-consecutive-nil type channels)))
             (bus-obj (make-instance 'bus :type type :busnum busnum :server server :chanls channels)))
        (loop :for i :upto (1- channels)
           :do (setf (elt (if (eq type :audio)
                              (audio-buses server)
                              (control-buses server))
                          (+ busnum i))
                     bus-obj))
        bus-obj))))

(defun bus-alloc (type &key (chanls 1) busnum (server *s*))
  (assert (member type '(:audio :control)) (type))
  (get-next-bus server type chanls busnum))

(defun bus-audio (&key (chanls 1) busnum (server *s*))
  (bus-alloc :audio :chanls chanls :busnum busnum :server server))

(defun bus-control (&key (chanls 1) busnum (server *s*))
  (bus-alloc :control :chanls chanls :busnum busnum :server server))

(defun bus-free (bus &key (server *s*))
  (let ((type (slot-value bus 'type)))
    (bt:with-lock-held ((server-lock server))
      (loop :for i :upto (1- (chanls bus))
         :do
         (setf (elt (if (eq type :audio)
                        (audio-buses server)
                        (control-buses server))
                    (+ i (busnum bus)))
               nil)))))

(defun bus-string (bus)
  "Make a string representing the bus that the server can understand."
  (with-slots (type busnum) bus
    (format nil "~a~a"
            (if (eq :audio type)
                "a"
                "c")
            busnum)))
