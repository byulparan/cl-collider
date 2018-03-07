;;;
;;; This is extension of cl-osc(in Quicklisp).
;;; SuperCollider use some non-standard OSC functions, so support it.
;;;

(in-package :sc-osc)

(defun cat (&rest catatac)
  "original function in cl-osc that return just vector type. but datagram socket is use (vector (unsigned 8))."
  (apply #'concatenate '(vector (unsigned-byte 8)) catatac))

;;; encode osc ------------------------------------------------------------

(defun encode-float64 (f)
  (osc::encode-int64 (ieee-floats:encode-float64 f)))

(defun encode-int16 (i)
  (subseq (osc::encode-int32 i) 2))

(defun sc-encode-blob (blob)
  (let* ((bl (length blob))
	 (mod-length (mod bl 4))
	 (padding-length (if (zerop mod-length) 0 (- 4 mod-length))))
    (osc::cat (osc::encode-int32 bl) blob
	      (osc::pad padding-length))))

(defmethod sc-encode-address ((address string))
  (cat (map 'vector #'char-code address) 
       (osc::string-padding address)))

(defmethod sc-encode-address ((address integer))
  (osc::encode-int32 address))

(defun sc-make-type-tags (datas)
  (labels ((rec (data)
	     (typecase data
	       (integer #\i)
	       (double-float #\d)
	       (float #\f)
	       (simple-string #\s)
	       (t #\b))))
    (let ((result nil))
      (dolist (i datas result)
	(if (listp i) (setf result (append result (list #\[) (sc-make-type-tags i) (list #\])))
	    (setf result (append result (list (rec i)))))))))

(defun sc-encode-typetags (data)
  (let ((lump (make-array 0 :adjustable t
			  :element-type '(unsigned-byte 8)
			  :fill-pointer t)))
      (macrolet ((write-to-vector (char)
		   `(vector-push-extend
		     (char-code ,char) lump)))
	(write-to-vector #\,)
	(dolist (x (sc-make-type-tags data))
	  (write-to-vector x)))
      (cat lump
	   (osc::pad (osc::padding-length (length lump))))))     

(defconstant +2^32+ (expt 2 32))

(defun sc-encode-timetag (utime)
  (labels ((make-timetag (unix-time-in-seconds)
	     (multiple-value-bind (secs subsecs)
		 (floor unix-time-in-seconds)
	       (+ (ash (+ secs osc::+unix-epoch+) 32) (round (* subsecs +2^32+))))))
    (cond
      ;; a 1bit timetag will be interpreted as 'imediately' 
      ((equalp utime :now)
       #(0 0 0 0 0 0 0 1)) 
      ((numberp utime) (osc::encode-int64 (make-timetag utime)))
      (t (error "the time or subsecond given is not an integer or float")))))

(defun sc-encode-data (data)
  (let ((lump (make-array 0 :adjustable t :fill-pointer t :element-type '(unsigned-byte 8))))
    (macrolet ((enc (f)
                 `(setf lump (cat lump (,f x)))))
      (dolist (x (alexandria:flatten data)) 
        (typecase x
          (integer (enc osc::encode-int32))
	  (double-float (enc encode-float64))
          (float (enc osc::encode-float32)) 
          (simple-string (enc osc::encode-string))
	  (t (enc sc-encode-blob))))
      lump)))

(defun encode-message (address &rest data)
  (concatenate '(vector (unsigned-byte 8))
	       (sc-encode-address address)
	       (sc-encode-typetags data)
	       (sc-encode-data data)))

(defun encode-bundle (data &optional timetag)
  (flet ((encode-bundle-elt (data)
	   (let ((message (apply #'encode-message data)))
	     (cat (osc::encode-int32 (length message)) message))))
    (cat '(35 98 117 110 100 108 101 0)	
	 (if timetag
	     (sc-encode-timetag timetag)
           (sc-encode-timetag :now))
	 (if (listp (car data))
	     (apply #'cat (mapcar #'encode-bundle-elt data))
	   (encode-bundle-elt data)))))

;;; decode osc ------------------------------------------------------------

(defun decode-float64 (s)
  (ieee-floats:decode-float64 (osc::decode-uint64 s)))

(defun sc-decode-timetag (timetag)
  (if (equalp timetag #(0 0 0 0 0 0 0 1))
      1
      (osc::decode-uint64 timetag)))

(defun sc-decode-taged-data (data)
  (let ((div (position 0 data)))
    (let ((tags (subseq data 1 div)) 
	  (acc (subseq data (osc::padded-length div)))
	  (result '()))
      (map 'vector
	   #'(lambda (x)
	       (cond
		((eq x (char-code #\i)) 
		 (push (osc::decode-int32 (subseq acc 0 4)) 
		       result)
		 (setf acc (subseq acc 4)))
		((eq x (char-code #\f))
		 (push (osc::decode-float32 (subseq acc 0 4)) 
		       result)
		 (setf acc (subseq acc 4)))
		((eq x (char-code #\s))
		 (let ((pointer (osc::padded-length (position 0 acc))))
		   (push (osc::decode-string 
			  (subseq acc 0 pointer))
			 result)
		   (setf acc (subseq acc pointer))))
		((eq x (char-code #\b)) 
		 (let* ((size (osc::decode-int32 (subseq acc 0 4)))
                        (end (osc::padded-length (+ 4 size))))
                   (push (osc::decode-blob (subseq acc 0 end)) 
                         result)
                   (setf acc (subseq acc end))))
		((eq x (char-code #\d))
		 (push (decode-float64 (subseq acc 0 8)) result)
		 (setf acc (subseq acc 8)))
		(t (error "unrecognised typetag ~a(~a)" (code-char x) x))))
	   tags)
      (nreverse result))))

(defun decode-message (message)
  (declare (type (vector *) message))
  (let ((x (position (char-code #\,) message)))
    (if (eq x NIL)
        (format t "message contains no data.. ")
	(cons (osc::decode-address (subseq message 0 x))
	      (sc-decode-taged-data (subseq message x))))))

(defun decode-bundle (data)
  (let ((contents '()))
    (if (equalp 35 (elt data 0))
	(let ((timetag (subseq data 8 16)) 
	      (i 16)
	      (bundle-length (length data)))
	  (loop while (< i bundle-length)
	     do (let ((mark (+ i 4))
		      (size (osc::decode-int32
			     (subseq data i (+ i 4)))))
		  (if (eq size 0)
		      (setf bundle-length 0)
		      (push (decode-bundle
			     (subseq data mark (+ mark size)))
			    contents))
		  (incf i (+ 4 size))))
	  (push timetag contents))
	(decode-message data))))

