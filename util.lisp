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
  #+ccl (namestring (ccl:full-pathname path))
  #-ccl
  (labels ((absolute-dir (dir)
	     (if (eql (car dir) :absolute) (if (find :home dir)
					       (append
						(pathname-directory (user-homedir-pathname))
						(cdr (member :home dir)))
					       dir)
		 (let* ((default-dir
			  (pathname-directory (truename ""))))
		   (when (find :up dir)
		     (setf dir (cdr dir))
		     (setf default-dir (butlast default-dir)))
		   (append default-dir (cdr dir))))))
    (namestring (make-pathname :directory (absolute-dir (pathname-directory path)) :name (pathname-name path) :type (pathname-type path)))))

(defmethod cat ((sequence string) &rest sequences)
  (apply #'concatenate 'string sequence sequences))

(defmethod cat ((sequence list) &rest sequences)
  (apply #'append sequence sequences))

(defun run-program (command &key output wait)
  #+ccl (ccl:run-program "/bin/sh" (list "-c" command) :output output :wait wait)
  #+sbcl (sb-ext:run-program "/bin/sh" (list "-c" command) :output (if (eql output t) *standard-output* output) :wait wait)
  #+clisp (ext:run-program "/bin/sh" :arguments (list "-c" command) :output (if (eql output t) :terminal output) :wait wait)
  #+abcl (progn
	   wait
	   (ext:run-shell-command (format nil "/bin/sh -c \"~a\"" command) :output (if (eql output t) *standard-output* output)))
  #+ecl (let ((*standard-output* ext:+process-standard-output+)
	      (*standard-input* ext:+process-standard-input+)
	      (*error-output* ext:+process-error-output+))
	  output wait
	  (ext:system (format nil "/bin/sh -c \"~a\"" command))))

(defun as-keyword (object)
  (alexandria:make-keyword
   (etypecase object
     (symbol object)
     (string (string-upcase object)))))

