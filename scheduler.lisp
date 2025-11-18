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



#+(or ecl (and lispworks (not mswindows)))
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

#+(and lispworks mswindows)
(defun unix-time ()
  (multiple-value-bind (seconds tenths-of-micro)
      (sys::internal-get-universal-time)
    (+ (- seconds 2208988800) ;; 2208988800 is the time between the start of 1970 and 1900
       (* tenths-of-micro 1.0d-7))))

#-windows
(cffi:defcstruct sched-param
    (priority :int))

#+darwin
(cffi:defcenum sched-policy
    (:sched_other 1)
  (:sched_rr 2)
  (:sched_fifo 4))

#+(or linux freebsd)
(cffi:defcenum sched-policy
    (:sched_other 0)
  (:sched_fifo 1)
  (:sched_rr 2))

#-windows
(defun set-thread-realtime-priority ()
  "This function is made high priority to calling thread, and sched-policy set SCHED_RR."
  (cffi:with-foreign-objects ((param '(:pointer (:struct sched-param))))
    (let* ((max-priority (cffi:foreign-funcall "sched_get_priority_max" :int (cffi:foreign-enum-value 'sched-policy :sched_rr)
									:int)))
      (cffi:with-foreign-slots ((priority dummy) param (:struct sched-param))
	(setf priority max-priority)))
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

;; ================================================================================
;; threading util

#+ecl
(defmacro with-recursive-lock-held ((lock) &body body)
  `(if (eql (bt:current-thread) (mp:lock-owner ,lock))
       (progn
	 ,@body)
     (bt:with-lock-held (,lock)
       ,@body)))

#-ecl
(defmacro with-recursive-lock-held ((lock) &body body)
  `(bt:with-recursive-lock-held (,lock)
     ,@body))

#+ecl
(defun condition-wait (condition-variable lock &key timeout)
  (let* ((success (bt:condition-wait condition-variable lock :timeout timeout)))
    (when (not success)
      (bt:acquire-lock lock))
    success))

#-ecl
(setf (symbol-function 'condition-wait) #'bt:condition-wait)


;; ================================================================================
;; Monotonic Time

#-darwin
(defun monotonic-time ()
     (multiple-value-bind (secs subsecs)
	       (org.shirakumo.precise-time:get-monotonic-time)
       (+ secs (* subsecs (/ 1.0d0 org.shirakumo.precise-time:monotonic-time-units-per-second)))))


#+darwin
(defun monotonic-time ()
    (cffi:with-foreign-objects ((tb :uint32 2))
      (if (= 0 (cffi:foreign-funcall "mach_timebase_info" :pointer tb :int))
	  (let* ((numer (cffi:mem-aref tb :uint32 0))
		 (denom (cffi:mem-aref tb :uint32 1)))
	    (* (/ (* (cffi:foreign-funcall "mach_absolute_time" :uint64) numer) denom) 1d-9))
	(error "Failed to get time scale for monotonic time."))))



;; ================================================================================

(defstruct sched-event timestamp task)

(defclass scheduler ()
  ((name
    :initarg :name
    :initform nil
    :reader sched-name)
   (server
    :initarg :server
    :initform nil
    :accessor server)
   (mutex
    :reader mutex)
   (condition-var
    :initform (bt:make-condition-variable)
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
   (timestamp
    :initarg :timestamp
    :initform #'monotonic-time
    :reader timestamp
    :documentation
    "This Function is get current scheduler time. That must based on seconds.")))

(defmethod initialize-instance :after ((self scheduler) &key)
  ;;; pilep:heap include lock. so scheduler use that lock.
  (with-slots (mutex in-queue) self
    #-(or ecl lispworks) (setf mutex (slot-value in-queue 'pileup::lock))
    #+ecl (setf mutex (bt:make-lock))
    #+lispworks (setf mutex (bt:make-recursive-lock))))

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
                                        (condition-wait (condition-var scheduler) (mutex scheduler) :timeout timeout)))
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
  "Insert task and time-info to scheduler queue. server of scheduler have ahead of time value(default to 0.2).
 '(- time (sched-ahead *s*)) is actual time it runs to f."
  (with-recursive-lock-held ((mutex scheduler))
    (pileup:heap-insert (make-sched-event :timestamp (- time (sched-ahead (server scheduler)))
                                          :task (lambda () (apply f args)))
                        (in-queue scheduler))
    (bt:condition-notify (condition-var scheduler)))
  (values))

(defun sched-clear (scheduler)
  "Clear to scheduler queue."
  (with-recursive-lock-held ((mutex scheduler))
    (let ((queue (in-queue scheduler)))
      (loop :while (not (pileup:heap-empty-p queue))
	    :do (pileup:heap-pop queue)))
    (bt:condition-notify (condition-var scheduler)))
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
  ((bpm :initarg :bpm :initform 60.0d0)
   (base-seconds)
   (base-beats :initarg :base-beats :initform 0 :reader base-beats)
   (beat-dur)
   (sync-thread :initform nil :accessor sync-thread)
   (sync-thread-run :initform t :accessor sync-thread-run)
   (sync-lock :initform (bt:make-lock) :reader sync-lock)
   (sync-cond :initform (bt:make-condition-variable) :reader sync-cond)))


(defmethod initialize-instance :after ((self tempo-clock) &key)
  (with-slots (bpm base-beats beat-dur base-seconds) self
    (setf beat-dur (/ 60.0d0 bpm)
	  base-seconds (funcall (timestamp self)))))


(defun sync-with-unix-time (server tempo-clock)
  (let ((min-diff most-positive-fixnum)
	(num-of-tries 5)
	(new-offset (timing-offset server)))
    (dotimes (i num-of-tries)
      (let* ((before-time (funcall (timestamp tempo-clock)))
	     (unix-time (unix-time))
	     (after-time (funcall (timestamp tempo-clock)))
	     (diff (- after-time before-time)))
	(when (< diff min-diff)
	  (setf min-diff diff)
	  (setf new-offset (- unix-time (+ before-time (/ diff 2)))))))
    (setf (timing-offset server) new-offset)))


(defmethod resync-thread-run ((tempo-clock tempo-clock))
  (sync-with-unix-time (server tempo-clock) tempo-clock)
  (setf (sync-thread tempo-clock)
    (bt:make-thread
     (lambda ()
       (bt:with-lock-held ((sync-lock tempo-clock))
	 (loop while (sync-thread-run tempo-clock)
	       do (bt:condition-wait (sync-cond tempo-clock) (sync-lock tempo-clock)
				     :timeout 20)
		  (sync-with-unix-time (server tempo-clock) tempo-clock))))
     :name (format nil "~@[~a~] resync time thread" (sched-name tempo-clock)))))


(defmethod beats-to-secs ((tempo-clock tempo-clock) beats)
  (with-slots (base-beats beat-dur base-seconds) tempo-clock
    (+ (* (- beats base-beats) beat-dur) base-seconds)))

(defmethod secs-to-beats ((tempo-clock tempo-clock) secs)
  (with-slots (base-seconds bpm base-beats) tempo-clock
    (+ (* (- secs base-seconds) (/ bpm 60.0)) base-beats)))


(defmethod tempo-clock-run ((tempo-clock tempo-clock))
  (when (eql (sched-status tempo-clock) :stop)
    (resync-thread-run tempo-clock)
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
                                                           (sched-ahead (server tempo-clock)))
							(sched-time tempo-clock))))
                                        (unless (plusp timeout)
                                          (return))
                                        (condition-wait (condition-var tempo-clock) (mutex tempo-clock) :timeout timeout)))
                            (loop :while (and (not (pileup:heap-empty-p (in-queue tempo-clock)))
                                              (>= (sched-time tempo-clock)
                                                  (- (beats-to-secs tempo-clock (sched-event-timestamp (pileup:heap-top (in-queue tempo-clock))))
                                                     (sched-ahead (server tempo-clock)))))
                                  :do (when (eql 'ensure-scheduler-stop-quit ;; it's magic code. it seems chagne..
                                                 (funcall (sched-event-task (pileup:heap-pop (in-queue tempo-clock)))))
                                        (setf run-p nil)
                                        (return)))))
                      (error (c) (format t "~&Error \"~a\" in TempoClock thread~%" c)
                        (finish-output)
                        (run)))))
           (set-thread-realtime-priority)
           (bt:with-lock-held ((mutex tempo-clock))
             (setf (sched-status tempo-clock) :running)
             (run))))
       :name (format nil "~@[~a ~]TempoClock thread" (sched-name tempo-clock))))
    :running))

(defmethod tempo-clock-beats ((tempo-clock tempo-clock))
  (secs-to-beats tempo-clock (sched-time tempo-clock)))


(defmethod tempo-clock-add ((tempo-clock tempo-clock) beats f &rest args)
  (with-recursive-lock-held ((mutex tempo-clock))
    (pileup:heap-insert (make-sched-event :timestamp beats
					  :task (lambda () (apply f args)))
			(in-queue tempo-clock))
    (bt:condition-notify (condition-var tempo-clock)))
  (values))


(defmethod tempo-clock-stop ((tempo-clock tempo-clock))
  (when (sync-thread tempo-clock)
    (bt:with-lock-held ((sync-lock tempo-clock))
      (setf (sync-thread-run tempo-clock) nil)
      (bt:condition-notify (sync-cond tempo-clock)))
    (bt:join-thread (sync-thread tempo-clock)))
  (with-slots (beat-dur) tempo-clock
    (when (eql (sched-status tempo-clock) :running)
      (tempo-clock-add tempo-clock (+ (* (sched-ahead *s*) .5 (reciprocal beat-dur))
				      (tempo-clock-beats tempo-clock))
		       (lambda () 'ensure-scheduler-stop-quit))
      (bt:join-thread (sched-thread tempo-clock))
      (tempo-clock-clear tempo-clock)
      (setf (sched-status tempo-clock) :stop))))


(defmethod tempo-clock-set-bpm ((tempo-clock tempo-clock) new-bpm)
  (with-recursive-lock-held ((mutex tempo-clock))
    (with-slots (base-seconds base-beats bpm beat-dur) tempo-clock
      (let* ((in-beats (tempo-clock-beats tempo-clock)))
	(setf base-seconds (beats-to-secs tempo-clock in-beats)
	      base-beats in-beats
	      bpm new-bpm
	      beat-dur (/ 60.0d0 new-bpm))))
    (bt:condition-notify (condition-var tempo-clock))))

(defmethod tempo-clock-bpm ((tempo-clock tempo-clock) &optional new-bpm)
  (if new-bpm (tempo-clock-set-bpm tempo-clock new-bpm)
    (with-slots (bpm) tempo-clock
      bpm)))

(defmethod tempo-clock-clear ((tempo-clock tempo-clock))
  (with-recursive-lock-held ((mutex tempo-clock))
    (with-slots (in-queue) tempo-clock
      (loop :until (pileup:heap-empty-p in-queue)
	    :do (pileup:heap-pop in-queue)))
    (bt:condition-notify (condition-var tempo-clock))))

(defmethod tempo-clock-quant ((tempo-clock tempo-clock) quant)
  (let* ((beats (secs-to-beats tempo-clock (+ (sched-ahead (server tempo-clock)) (sched-time tempo-clock)))))
    (+ beats (- quant (mod beats quant)))))
