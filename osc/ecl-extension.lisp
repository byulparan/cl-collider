(in-package :sc-osc)

(defun single-float-bits (x)
  (declare (type single-float x))
  (assert (= (float-radix x) 2))
  (cond ((zerop x) (if (eql x 0.0f0) 0 #x-80000000))
	((= ext:single-float-positive-infinity x) 2139095040)
	(t
	 (multiple-value-bind (lisp-significand lisp-exponent lisp-sign)
	     (integer-decode-float x)
	   (assert (plusp lisp-significand))
	   ;; Calculate IEEE-style fields from Common-Lisp-style fields.
	   
	   ;; KLUDGE: This code was written from my foggy memory of what IEEE
	   ;; format looks like, augmented by some experiments with
	   ;; the existing implementation of SINGLE-FLOAT-BITS, and what
	   ;; I found floating around on the net at
	   ;;   <http://www.scri.fsu.edu/~jac/MAD3401/Backgrnd/ieee.html>,
	   ;;   <http://rodin.cs.uh.edu/~johnson2/ieee.html>,
	   ;; and
	   ;;   <http://www.ttu.ee/sidu/cas/IEEE_Floating.htm>.
	   ;; And beyond the probable sheer flakiness of the code, all the bare
	   ;; numbers floating around here are sort of ugly, too. -- WHN 19990711
	   (let* ((significand lisp-significand)
		  (exponent (+ lisp-exponent 23 127))
		  (unsigned-result
		    (if (plusp exponent)    ; if not obviously denormalized
			(do ()
			    (nil)
			  (cond (;; special termination case, denormalized
				 ;; float number
				 (zerop exponent)
				 ;; Denormalized numbers have exponent one
				 ;; greater than the exponent field.
				 (return (ash significand -1)))
				(;; ordinary termination case
				 (>= significand (expt 2 23))
				 (assert (< 0 significand (expt 2 24)))
				 ;; Exponent 0 is reserved for
				 ;; denormalized numbers, and 255 is
				 ;; reserved for specials like NaN.
				 (assert (< 0 exponent 255))
				 (return (logior (ash exponent 23)
						 (logand significand
							 (1- (ash 1 23))))))

				(t
				 ;; Shift as necessary to set bit 24 of
				 ;; significand.
				 (setf significand (ash significand 1)
				       exponent (1- exponent)))))
		      (do ()
			  ((zerop exponent)
			   ;; Denormalized numbers have exponent one
			   ;; greater than the exponent field.
			   (ash significand -1))
			(unless (zerop (logand significand 1))
			  (warn "denormalized SINGLE-FLOAT-BITS ~S losing bits"
				x))
			(setf significand (ash significand -1)
			      exponent (1+ exponent))))))
	     (ecase lisp-sign
	       (1 unsigned-result)
	       (-1 (logior unsigned-result (- (expt 2 31))))))))))

(defun make-single-float (bits)
  (cond
    ;; IEEE float special cases
    ((zerop bits) 0.0)
    ((= 2139095040 bits) ext:single-float-positive-infinity)
    ((= bits #x-80000000) -0.0)
    (t (let* ((sign (ecase (ldb (byte 1 31) bits)
                      (0  1.0)
                      (1 -1.0)))
              (iexpt (ldb (byte 8 23) bits))
              (expt (if (zerop iexpt) ; denormalized
                        -126
                        (- iexpt 127)))
              (mant (* (logior (ldb (byte 23 0) bits)
                               (if (zerop iexpt)
                                   0
                                   (ash 1 23)))
                       (expt 0.5 23))))
         (* sign (expt 2.0 expt) mant)))))

(defun encode-float32 (f)
  (encode-int32 (single-float-bits f)))

(defun decode-float32 (s)
  (make-single-float (decode-int32 s)))



