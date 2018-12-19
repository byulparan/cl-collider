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

(defun thread-wait-with-timeout (f timeout-msec)
  #+ccl (ccl:process-wait-with-timeout "wait.." timeout-msec f)
  #+(or sbcl ecl) (let ((dead-time (+ (/ (get-internal-real-time) internal-time-units-per-second)
				      (* 1e-3 timeout-msec))))
		    (cond ((apply f nil) t)
			  (t (loop
			       (alexandria:when-let ((val (apply f nil)))
				 (return-from thread-wait-with-timeout val))
			       (when (> (/ (get-internal-real-time) internal-time-units-per-second)
					dead-time)
				 (return))
			       (sleep .01))))))

(defun full-pathname (path)
  "returning absoulte full-pathname of path"
  (let* ((directory (uiop:truename* (uiop:pathname-directory-pathname path)))
	 (filename (pathname-name path))
	 (filetype (pathname-type path)))
    (when directory
      (format nil "~a~@[~a~]~@[.~a~]" (namestring directory) filename filetype))))

(defmethod cat ((sequence string) &rest sequences)
  (apply #'concatenate 'string sequence sequences))

(defmethod cat ((sequence list) &rest sequences)
  (apply #'append sequence sequences))


(defun sc-program-run (program options)
  (simple-inferiors:run program options
  		                :output t :copier :line))


#+windows
(defun find-port (sc-thread port)
  "in windows, scsynth program should be bind port before send message in CL"
  (labels ((netstat ()
	     (let* ((result 
		      (with-output-to-string (s)
			(uiop:run-program "netstat -an" :output s :external-format :default))))
	       (with-input-from-string (s result)
		 (loop for line = (read-line s nil nil)
		       while line
		       when (and (search "UDP" line) (search (write-to-string port) line))
			 collect line)))))
    (let* ((find-p (thread-wait-with-timeout
		    (lambda () (or (not (bt:thread-alive-p sc-thread)) (netstat)))
		    15000)))
      (when find-p
	(bt:thread-alive-p sc-thread)))))


(defun as-keyword (object)
  (alexandria:make-keyword
   (etypecase object
     (symbol object)
     (string (string-upcase object)))))

(defun nth-wrap (n list)
  "Get the Nth value of LIST, wrapping around if the value is bigger or smaller than the list length."
  (nth (mod n (length list)) list))

(defun blend-nth (n list)
  "Get the Nth value of LIST, linearly interpolating between the adjacent values if N is not an integer."
  (if (= n (round n))
      (nth n list)
      (let* ((floor (floor n))
             (ceiling (ceiling n))
             (fl-diff (- n floor)))
        (+ (* (nth floor list) (- 1 fl-diff))
           (* (nth ceiling list) fl-diff)))))

(defun linear-resample (list new-size)
  "Using linear interpolation, resample the values of LIST to a new list of size NEW-SIZE."
  (let* ((old-size (length list))
         ;; this.size - 1 / (newSize - 1).max(1);
         (factor (/ (1- old-size) (max (1- new-size) 1))))
    (if (= old-size new-size)
        list
        (loop :for i :from 0 :below new-size
           :collect (blend-nth (* i factor) list)))))
