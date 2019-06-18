(in-package #:sc)

#+sbcl
(defun unix-time ()
  (multiple-value-bind (secs usecs)
      (sb-ext:get-time-of-day)
    (+ secs (* usecs 1.0d-6))))

#+ccl
(defun unix-time ()
  (ccl:rlet ((tv :timeval))
    (ccl::gettimeofday tv)
    (multiple-value-bind (secs usecs)
	(values (ccl:pref tv :timeval.tv_sec) (ccl:pref tv :timeval.tv_usec))
      (+ secs (* usecs 1.0d-6)))))
 
#+ecl
(progn
  (cffi:defctype time_t :long)
  (cffi:defctype seconds_t :int)

  (cffi:defcstruct timeval
    (tv_sec time_t)
    (tv_usec seconds_t))

  (cffi:defcfun gettimeofday :int
    (timeval :pointer)
    (pointer :pointer))

  (defun unix-time ()
    (cffi:with-foreign-object (tv '(:struct timeval))
      (gettimeofday tv (cffi::null-pointer))
      (+ (cffi:mem-ref tv 'time_t) (* (cffi:mem-ref tv 'seconds_t (cffi:foreign-type-size 'time_t)) 1.0d-6)))))

#-windows
(cffi:defcstruct sched-param
  (priority :int))

#+darwin
(cffi:defcenum sched-policy
  (:sched_other 1)
  (:sched_rr 2)
  (:sched_fifo 4))

#+linux
(cffi:defcenum sched-policy
  (:sched_other 0)
  (:sched_fifo 1)
  (:sched_rr 2))

#-windows
(defun set-thread-realtime-priority ()
  "This function is made high priority to calling thread, and sched-policy set SCHED_RR."
  (cffi:with-foreign-objects ((param '(:pointer (:struct sched-param))))
    (cffi:with-foreign-slots ((priority dummy) param (:struct sched-param))
      (setf priority 76))
    (cffi:foreign-funcall "pthread_setschedparam" :pointer (cffi:foreign-funcall "pthread_self" :pointer)
						  :int (cffi:foreign-enum-value 'sched-policy :sched_rr)
						  :pointer param)))

#-windows
(defun get-thread-priority ()
  "Get the thread-info of calling thread. If you want get thread-info of *main-scheduler*,
 eval the '(callback (now) #'get-thread-priority)."
  (cffi:with-foreign-objects ((param '(:pointer (:struct sched-param)))
			      (policy :int))
    (cffi:foreign-funcall "pthread_getschedparam" :pointer (cffi:foreign-funcall "pthread_self" :pointer)
						  :pointer policy
						  :pointer param)
    (format t "~&policy: ~d~%priority: ~d" (let ((policy (cffi:mem-ref policy :int)))
					     (cffi:foreign-enum-keyword 'sched-policy policy))
	    (cffi:with-foreign-slots ((priority dummy) param (:struct sched-param))
	      priority))))

#+windows
(defun set-thread-realtime-priority ()
  "Not implements windows,yet"
  (values))

#+windows
(defun get-thread-priority ()
  "Not implements windows,yet"
  (values))


(defstruct sched-event timestamp task)

(defclass scheduler ()
  ((name
    :initarg :name
    :initform nil
    :reader sched-name)
   (mutex
    :reader mutex)
   (condition-var
    :initform #-ecl (bt:make-condition-variable)
	      #+ecl (bt-sem:make-semaphore)
    :reader condition-var)
   (in-queue
    :initform (pileup:make-heap #'<= :size 100 :key #'sched-event-timestamp)
    :reader in-queue)
   (sched-thread
    :initform nil
    :accessor sched-thread)
   (status
    :initform :stop
    :accessor sched-status)
   (ahead
    :initarg :sched-ahead
    :initform .3
    :accessor sched-ahead)
   (timestamp
    :initarg :timestamp
    :initform #'unix-time
    :reader timestamp
    :documentation
    "This Function is get current scheduler time. That must based on seconds.")))

(defmethod initialize-instance :after ((self scheduler) &key)
  ;;; pilep:heap include lock. so scheduler use that lock.
  (with-slots (mutex in-queue) self
    #-ecl (setf mutex (slot-value in-queue 'pileup::lock))
    #+ecl (setf mutex (bt:make-recursive-lock))))


;;; timed wait -----------------------------------------------------------------------------------------

(defun condition-wait (condition-variable lock)
  #-ecl (bt:condition-wait condition-variable lock)
  #+ecl
  (progn
    (bt:release-lock lock)
    (unwind-protect (bt-sem:wait-on-semaphore condition-variable)
      (bt:acquire-lock lock t))))

(defun condition-timed-wait (condition-variable lock time)
  #+sbcl (unless (bt:condition-wait condition-variable lock :timeout time)
	   (bt:acquire-lock lock t))
  #-sbcl
  (progn
    (bt:release-lock lock)
    (unwind-protect
	 #+ccl (ccl:timed-wait-on-semaphore condition-variable time)
      #+ecl(bt-sem:wait-on-semaphore condition-variable :timeout time)
      (bt:acquire-lock lock t))))

;;; -----------------------------------------------------------------------------------------------------

(defun sched-time (scheduler)
  (funcall (timestamp scheduler)))

(defun sched-quant (scheduler quantized-time &optional (offset-time 0.0d0))
  "Return a time which quantized to given a quantized-time."
  (let ((time (+ offset-time (sched-time scheduler))))
    (+ time (- quantized-time (mod time quantized-time)))))

(defun sched-run (scheduler)
  (when (eql (sched-status scheduler) :stop)
    (setf (sched-thread scheduler)
      (bt:make-thread
       (lambda ()
	 (setf *random-state* (make-random-state t))
	 (labels ((run ()
		    (handler-case
			(let* ((run-p t))
			  (loop while run-p do
			    (loop :while (pileup:heap-empty-p (in-queue scheduler))
				  :do (condition-wait (condition-var scheduler) (mutex scheduler)))
			    (loop :while (not (pileup:heap-empty-p (in-queue scheduler)))
				  :do (let ((timeout (- (sched-event-timestamp (pileup:heap-top (in-queue scheduler))) (sched-time scheduler))))
					(unless (plusp timeout) (return))
					(condition-timed-wait (condition-var scheduler) (mutex scheduler) timeout)))
			    (loop :while (and (not (pileup:heap-empty-p (in-queue scheduler)))
					      (>= (sched-time scheduler) (sched-event-timestamp (pileup:heap-top (in-queue scheduler)))))
				  :do (when (eql 'ensure-scheduler-stop-quit ;; it's magic code. it seems chagne..
						 (funcall (sched-event-task (pileup:heap-pop (in-queue scheduler)))))
					(setf run-p nil)
					(return)))))
		      (error (c) (format t "~&Error \"~a\" in scheduler thread~%" c)
			(run)))))
	   (set-thread-realtime-priority) ;thread-boost!!
	   (bt:with-lock-held ((mutex scheduler))
	     (setf (sched-status scheduler) :running)
	     (run))))
       :name (format nil "~@[~a ~]scheduler thread" (sched-name scheduler))))
    :running))

(defun sched-add (scheduler time f &rest args)
  "Insert task and time-info to scheduler queue. scheduler have ahead of time value(default to 0.3).
 '(- time (sched-ahead scheduler)) is actual time it runs to f."
  (bt:with-recursive-lock-held ((mutex scheduler))
    (pileup:heap-insert (make-sched-event :timestamp (- time (sched-ahead scheduler))
				   :task (lambda () (apply f args)))
			(in-queue scheduler))
    #-ecl (bt:condition-notify (condition-var scheduler))
    #+ecl (bt-sem:signal-semaphore (condition-var scheduler)))
  (values))

(defun sched-clear (scheduler)
  "Clear to scheduler queue."
  (bt:with-recursive-lock-held ((mutex scheduler))
    (let ((queue (in-queue scheduler)))
      (loop :while (not (pileup:heap-empty-p queue))
	    :do (pileup:heap-pop queue)))
    #-ecl (bt:condition-notify (condition-var scheduler))
    #+ecl (bt-sem:signal-semaphore (condition-var scheduler)))
  (values))

(defun sched-stop (scheduler)
  "Stop the scheduler."
  (when (eql (sched-status scheduler) :running)
    (sched-clear scheduler)
    (sched-add scheduler -1 (lambda () 'ensure-scheduler-stop-quit))
    (bt:join-thread (sched-thread scheduler))
    (setf (sched-status scheduler) :stop)))


;;; TempoClock
(defclass tempo-clock (scheduler)
  ((bpm :initarg :bpm :accessor bpm)
   (beat-dur :initarg :beat-dur)
   (base-seconds :initarg :base-seconds :accessor base-seconds)
   (base-beats :initarg :base-beats :accessor base-beats)))

(defmethod beat-dur ((tempo-clock tempo-clock))
  (slot-value tempo-clock 'beat-dur))

(defmethod beats-to-secs ((tempo-clock tempo-clock) beats)
  (with-slots (base-beats beat-dur base-seconds) tempo-clock
    (+ (* (- beats base-beats) beat-dur) base-seconds)))

(defmethod secs-to-beats ((tempo-clock tempo-clock) secs)
  (with-slots (base-seconds bpm base-beats) tempo-clock
    (+ (* (- secs base-seconds) (/ bpm 60.0)) base-beats)))

(defmethod tempo-clock-run ((tempo-clock tempo-clock))
  (when (eql (sched-status tempo-clock) :stop)
    (setf (sched-thread tempo-clock)
      (bt:make-thread
       (lambda ()
	 (setf *random-state* (make-random-state t))
	 (labels ((run ()
		    (handler-case
			(let* ((run-p t))
			  (loop while run-p do
			    (loop :while (pileup:heap-empty-p (in-queue tempo-clock))
				  :do (condition-wait (condition-var tempo-clock) (mutex tempo-clock)))
			    (loop :while (not (pileup:heap-empty-p (in-queue tempo-clock)))
				  :do (let ((timeout (- (- (beats-to-secs tempo-clock (sched-event-timestamp (pileup:heap-top (in-queue tempo-clock))))
							   (sched-ahead tempo-clock))
							(unix-time))))
					(unless (plusp timeout) (return))
					(condition-timed-wait (condition-var tempo-clock) (mutex tempo-clock) timeout)))
			    (loop :while (and (not (pileup:heap-empty-p (in-queue tempo-clock)))
					      (>= (unix-time)
						  (- (beats-to-secs tempo-clock (sched-event-timestamp (pileup:heap-top (in-queue tempo-clock))))
						     (sched-ahead tempo-clock))))
				  :do (when (eql 'ensure-scheduler-stop-quit ;; it's magic code. it seems chagne..
						 (funcall (sched-event-task (pileup:heap-pop (in-queue tempo-clock)))))
					(setf run-p nil)
					(return)))))
		      (error (c) (format t "~&Error \"~a\" in Tempo-Clock thread~%" c)
			(run)))))
	   (set-thread-realtime-priority)
	   (bt:with-lock-held ((mutex tempo-clock))
	     (setf (sched-status tempo-clock) :running)
	     (run))))
       :name (format nil "~@[~a ~]TempoClock thread" (sched-name tempo-clock))))
    :running))

(defmethod tempo-clock-beats ((tempo-clock tempo-clock))
  (secs-to-beats tempo-clock (sched-time tempo-clock)))

(defun tempo-clock-add (tempo-clock beats f &rest args)
  (bt:with-recursive-lock-held ((mutex tempo-clock))
    (pileup:heap-insert (make-sched-event :timestamp beats
				   :task (lambda () (apply f args)))
			(in-queue tempo-clock))
    (bt:condition-notify (condition-var tempo-clock)))
  (values))

(defmethod tempo-clock-stop ((tempo-clock tempo-clock))
  (when (eql (sched-status tempo-clock) :running)
    (tempo-clock-add tempo-clock (tempo-clock-beats tempo-clock) (lambda () 'ensure-scheduler-stop-quit))
    (bt:join-thread (sched-thread tempo-clock))
    (setf (sched-status tempo-clock) :stop)))

(defmethod tempo-clock-set-bpm ((tempo-clock tempo-clock) new-bpm)
  (bt:with-recursive-lock-held ((mutex tempo-clock))
    (with-slots (base-seconds base-beats bpm beat-dur) tempo-clock
      (let* ((in-beats (tempo-clock-beats tempo-clock)))
	(setf base-seconds (beats-to-secs tempo-clock in-beats)
	      base-beats in-beats
	      bpm new-bpm
	      beat-dur (/ 60.0 new-bpm))))
    (bt:condition-notify (condition-var tempo-clock))))

(defmethod tempo-clock-bpm ((tempo-clock tempo-clock) &optional new-bpm)
  (if new-bpm (tempo-clock-set-bpm tempo-clock new-bpm)
    (bpm tempo-clock)))

(defun tempo-clock-clear (tempo-clock)
  (bt:with-recursive-lock-held ((mutex tempo-clock))
    (with-slots (in-queue) tempo-clock
      (loop :until (pileup:heap-empty-p in-queue)
	    :do (pileup:heap-pop in-queue)))
    (bt:condition-notify (condition-var tempo-clock))))

(defmethod tempo-clock-quant ((tempo-clock tempo-clock) quant)
  (let* ((beats (secs-to-beats tempo-clock (+ .3 (sched-time tempo-clock)))))
    (+ beats (- quant (mod beats quant)))))


