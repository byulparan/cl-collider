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
  #+lispworks (mp:process-wait "wait.." f)
  #+(or sbcl ecl) (or (apply f nil)
		      (loop
			(when (apply f nil)
			  (return))
			(sleep .01))))

(defun thread-wait-with-timeout (f timeout-msec)
  #+ccl (ccl:process-wait-with-timeout "wait.." timeout-msec f)
  #+lispworks (mp:process-wait-with-timeout "wait.." (* timeout-msec 0.001) f)
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
  "Get the absolute pathname of PATH."
  (uiop:native-namestring (namestring (uiop:ensure-pathname path :want-non-wild t))))

(defun file-exists-p (filename)
  "True if FILENAME names a file that exists. This function is needed to ensure characters like ? are not interpreted as Common Lisp pathname wildcards."
  #-windows (probe-file (uiop:ensure-pathname filename :want-non-wild t))
  #+windows (probe-file filename))

(defmethod cat ((sequence string) &rest sequences)
  (apply #'concatenate 'string sequence sequences))

(defmethod cat ((sequence list) &rest sequences)
  (apply #'append sequence sequences))

(defun sc-program-run (program options)
  #+(or ecl lispworks)
  (uiop:run-program (format nil "~{~s ~}" (cons program options))
		    :output :interactive)
  #-(or ecl lispworks)
  (simple-inferiors:run program options
			:output t :error t :copier :line))

(defun as-keyword (object)
  (alexandria:make-keyword
   (etypecase object
     (symbol object)
     (string (string-upcase object)))))

(defun nth-wrap (n list)
  "Get the Nth value of LIST, wrapping around if the value is bigger or smaller than the list length."
  (elt list (mod n (length list))))

(defun blend-nth (n list)
  "Get the Nth value of LIST, linearly interpolating between the adjacent values if N is not an integer."
  (if (= n (round n))
      (elt list n)
      (let* ((floor (floor n))
             (ceiling (ceiling n))
             (fl-diff (- n floor)))
        (+ (* (elt list floor) (- 1 fl-diff))
           (* (elt list ceiling) fl-diff)))))

(defun linear-resample (sequence length)
  "Using linear interpolation, resample the values of SEQUENCE to a new list or vector of length LENGTH."
  (let ((old-length (length sequence)))
    (if (= old-length length)
        sequence
	(let ((factor (/ (1- old-length) (max (1- length) 1)))
	      (res (etypecase sequence
		     (list (make-list length))
		     (vector (make-array (list length))))))
	  (dotimes (i length res)
	    (setf (elt res i) (blend-nth (* i factor) sequence)))))))

;; conditionally load swank extensions
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (alexandria:featurep :swank)
    (load (asdf:system-relative-pathname :cl-collider "swank-extensions.lisp"))))

;; conditionally load slynk extensions
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (alexandria:featurep :slynk)
    (load (asdf:system-relative-pathname :cl-collider "slynk-extensions.lisp"))))

(defun write-mono-fl32-wav (stream sr sequence)
  "write sequence data to wave file."
  (write-sequence (flexi-streams:string-to-octets "RIFF") stream)
  (write-sequence (nreverse (osc::encode-int32 (+ 36 (* 4 (length sequence))))) stream)
  (write-sequence (flexi-streams:string-to-octets "WAVE") stream)
  (write-sequence (flexi-streams:string-to-octets "fmt ") stream)
  (write-sequence (nreverse (osc::encode-int32 16)) stream)
  (write-byte 3 stream) (write-byte 0 stream)
  (write-byte 1 stream) (write-byte 0 stream)
  (write-sequence (nreverse (osc::encode-int32 sr)) stream)
  (write-sequence (nreverse (osc::encode-int32 (* 4 sr))) stream)
  (write-byte 4 stream) (write-byte 0 stream)
  (write-byte 32 stream) (write-byte 0 stream)
  (write-sequence (flexi-streams:string-to-octets "data") stream)
  (write-sequence (nreverse (osc::encode-int32 (* 4 (length sequence)))) stream)
  (dotimes (i (length sequence))
    (write-sequence (nreverse (osc::encode-float32 (float (elt sequence i) 1.0))) stream)))
