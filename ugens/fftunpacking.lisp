
(in-package #:sc)


(defclass pv-chain-ugen (width-first-ugen) ())

(defun unpack-1-fft (chain bufsize binindex &optional (whichmeasure 0))
  (multinew (lambda (cls &rest inputs) (apply #'ugen-new "Unpack1FFT" :demand cls #'identity :bipolar
					      inputs))
	    'ugen chain bufsize binindex whichmeasure))

(defun unpack-fft (chain bufsize &optional (frombin 0) tobin)
  (labels ((range (start end)
	     (loop for i from start to end collect i)))
    (let ((upperlimit (divide bufsize 2.0)))
      (setf tobin (if (not tobin) upperlimit (min tobin upperlimit)))
      (alexandria:flatten 
       (flop
	(list (unpack-1-fft chain bufsize (range frombin tobin) 0)
	      (unpack-1-fft chain bufsize (range frombin tobin) 1)))))))

(defun pack-fft (chain bufsize magsphases &key (frombin 0) tobin (zeroothers 0))
  (setf tobin (if tobin tobin (divide bufsize 2.0)))
  (apply 'multinew (lambda (cls &rest inputs) (apply #'ugen-new "PackFFT" :control cls #'identity :bipolar inputs))
	 'pv-chain-ugen chain bufsize frombin tobin zeroothers (length magsphases) (alexandria:ensure-list magsphases)))

(defmethod pvcalc ((pv-ugen pv-chain-ugen) frames func &key (frombin 0) tobin (zeroothers 0))
  (let (origmagsphases magsphases)
    (setf origmagsphases (flop (clump (unpack-fft pv-ugen frames frombin tobin) 2)))
    (setf magsphases (funcall func (nth 0 origmagsphases) (nth 1 origmagsphases)))
    (setf magsphases (case (length magsphases)
		       (1 (append magsphases (let ((orig (nth 1 origmagsphases)))
					       (if (listp orig) orig (list orig)))))
		       (2 magsphases)
		       (t (list magsphases (nth 1 origmagsphases)))))
    (setf magsphases (alexandria:flatten (flop magsphases)))
    (pack-fft pv-ugen frames magsphases :frombin frombin :tobin tobin :zeroothers zeroothers)))

(defmethod pvcalc2 ((pv-ugen pv-chain-ugen) chain2 frames func &key (frombin 0) tobin (zeroothers 0))
  (let* ((origmagsphases (flop (clump (unpack-fft pv-ugen frames frombin tobin) 2)))
	 (origmagsphases2 (flop (clump (unpack-fft chain2 frames frombin tobin) 2)))
	 (magsphases (funcall func (nth 0 origmagsphases)
			      (nth 1 origmagsphases)
			      (nth 0 origmagsphases2)
			      (nth 1 origmagsphases2))))
    (setf magsphases (case (length magsphases)
		       (1 (append magsphases (let ((orig (nth 1 origmagsphases)))
					       (if (listp orig) orig (list orig)))))
		       (2 magsphases)
		       (t (list magsphases (nth 1 origmagsphases)))))
    (setf magsphases (alexandria:flatten (flop magsphases)))
    (pack-fft pv-ugen frames magsphases :frombin frombin :tobin tobin :zeroothers zeroothers)))





(defmethod pv-collect ((pv-ugen pv-chain-ugen) frames func &key (frombin 0) tobin (zeroothers 0))
  (let ((magsphases (clump (unpack-fft pv-ugen frames frombin tobin) 2))
	(ret nil))
    (setf magsphases
	  (alexandria:flatten
	   (loop repeat (length magsphases)
		 for index from 0
		 for mp = (nth index magsphases)
		 collect
		 (progn
		   (setf ret (alexandria:ensure-list (funcall func (nth 0 mp) (nth 1 mp) index)))
		   (setf ret (if (= 1 (length ret)) (concatenate 'list ret (alexandria:ensure-list (nth 1 mp))) ret))))))
    (pack-fft pv-ugen frames magsphases :frombin frombin :tobin tobin :zeroothers zeroothers)))


(defmethod pv-collect2 ((pv-ugen pv-chain-ugen) frames func &key (frombin 0) tobin (zeroothers 0))
  (let ((magsphases (clump (unpack-fft pv-ugen frames frombin tobin) 2))
	(ret nil))
    (setf magsphases
	  (alexandria:flatten
	   (loop repeat (length magsphases)
		 for index from 0
		 for mp = (nth index magsphases)
		 collect
		 (progn
		   (setf ret (alexandria:ensure-list (funcall func (nth 0 mp) (nth 1 mp) (+ frombin index))))
		   (setf ret (if (= 1 (length ret)) (concatenate 'list ret (alexandria:ensure-list (nth 1 mp))) ret))))))
    (pack-fft pv-ugen frames magsphases :frombin frombin :tobin tobin :zeroothers zeroothers)))
