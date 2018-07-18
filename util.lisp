;;; simple utillity and wrap ccl & sbcl's function.

(in-package #:sc)
(named-readtables:in-readtable :sc)


;;; [1 2 3] == (list 1 2 3)
(let ((rpar (get-macro-character #\))))
  (set-macro-character #\] rpar)
  (set-macro-character #\[ (lambda (stream char1)
			     (declare (ignore char1))
			     (apply (lambda (&rest rest) (cons 'list rest))
				    (read-delimited-list #\] stream t)))))

(defun thread-wait (f)
  #+ccl (ccl:process-wait "wait.." f)
  #+(or sbcl ecl) (or (apply f nil)
		      (loop
			(when (apply f nil)
			  (return))
			(sleep .01))))

(defun thread-wait-with-timeout (f timeout)
  #+ccl (ccl:process-wait-with-timeout "wait.." timeout f)
  #+(or sbcl ecl) (let ((target-time (+ (get-internal-real-time) (* 10 timeout))))
		    (cond ((apply f nil) t)
			  (t (loop
			       (alexandria:when-let ((val (apply f nil)))
				 (return-from thread-wait-with-timeout val))
			       (when (> (get-internal-real-time) target-time)
				 (return))
			       (sleep .01))))))

(defun full-pathname (path)
  "returning absoulte full-pathname of path"
  (namestring (uiop:truename* path)))

(defmethod cat ((sequence string) &rest sequences)
  (apply #'concatenate 'string sequence sequences))

(defmethod cat ((sequence list) &rest sequences)
  (apply #'append sequence sequences))


#-windows
(defun sc-program-run (program options)
  (let* ((command 
	   (format nil "\"~a\" ~{~a ~}"
		   program options)))
    (uiop:run-program command
		      :output :interactive)))

#+windows
(defun sc-program-run (program options)
  (let* ((command 
	   (format nil "\"\"~a\" ~{~a ~}\""
		   program options)))
    (uiop:run-program command
		      :output :interactive)))


(defun as-keyword (object)
  (alexandria:make-keyword
   (etypecase object
     (symbol object)
     (string (string-upcase object)))))

