;;; simple utillity and wrap ccl & sbcl's function.

(in-package #:sc)

(defun join-thread (thread)
  #+ccl (bt:join-thread thread)
  #+sbcl (sb-thread:join-thread thread :default nil))

(defun thread-wait (f)
  #+ccl (ccl:process-wait "wait.." f)
  #+sbcl (or (apply f nil)
	     (loop
	       (when (apply f nil)
		 (return))
	       (sb-unix:nanosleep 0 10000000))))

(defun thread-wait-with-timeout (f timeout)
  #+ccl (ccl:process-wait-with-timeout "wait.." timeout f)
  #+sbcl (let ((target-time (+ (get-internal-real-time) (* 10 timeout))))
	   (cond ((apply f nil) t)
		 (t (loop
		      (alexandria:when-let ((val (apply f nil)))
			(return-from thread-wait-with-timeout val))
		      (when (> (get-internal-real-time) target-time)
			(return))
		      (sb-unix:nanosleep 0 10000000))))))


;;; [1 2 3] == (list 1 2 3)
(let ((rpar (get-macro-character #\))))
  (set-macro-character #\] rpar)
  (set-macro-character #\[ (lambda (stream char1)
			     (declare (ignore char1))
			     (apply (lambda (&rest rest) (cons 'list rest))
				    (read-delimited-list #\] stream t)))))
