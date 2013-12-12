;;;
;;; This is extension of cl-osc.(in Quicklisp)
;;; It support more types for encode to data(especially Float64),
;;; and time function.
;;; I was find this code in google. but I can't remember that(sorry..).
;;;

(in-package :osc)

(defun cat (&rest catatac)
  "original function in cl-osc that return just vector type. but datagram socket is use (vector (unsigned 8))."
  (apply #'concatenate '(vector (unsigned-byte 8)) catatac))

(defun make-type-tags (datas)
  (labels ((rec (data)
	     (typecase data
	       (integer #\i)
	       (double-float #\d)
	       (float #\f)
	       (simple-string #\s)
	       (t #\b))))
    (let ((result nil))
      (dolist (i datas result)
	(if (listp i) (setf result (append result (list #\[) (make-type-tags i) (list #\])))
	    (setf result (append result (list (rec i)))))))))

(defun encode-typetags (data)
  (let ((lump (make-array 0 :adjustable t  :element-type '(unsigned-byte 8)
			      :fill-pointer t)))
      (macrolet ((write-to-vector (char)
		   `(vector-push-extend
		     (char-code ,char) lump)))
	(write-to-vector #\,)
	(dolist (x (make-type-tags data))
	  (write-to-vector x)))
      (cat lump
	   (pad (padding-length (length lump))))))     



(defun encode-message (address &rest data)
  (concatenate '(vector (unsigned-byte 8))
	       (encode-address-ext address)
	       (encode-typetags data)
	       (encode-data data)))

(defmethod encode-address-ext ((address string))
  (cat (map 'vector #'char-code address) 
       (string-padding address)))

(defmethod encode-address-ext ((address integer))
  (encode-int32 address))

(defun encode-data (data)
  (let ((lump (make-array 0 :adjustable t :fill-pointer t :element-type '(unsigned-byte 8))))
    (macrolet ((enc (f)
                 `(setf lump (cat lump (,f x)))))
      (dolist (x (alexandria:flatten data)) 
        (typecase x
          (integer (enc encode-int32))
	  (double-float (enc encode-float64))
          (float (enc encode-float32)) 
          (simple-string (enc encode-string))
	  (t (enc encode-blob))))
      lump)))

(defun decode-taged-data (data)
  (let ((div (position 0 data)))
    (let ((tags (subseq data 1 div)) 
	  (acc (subseq data (padded-length div)))
	  (result '()))
      (map 'vector
	   #'(lambda (x)
	       (cond
		((eq x (char-code #\i)) 
		 (push (decode-int32 (subseq acc 0 4)) 
		       result)
		 (setf acc (subseq acc 4)))
		((eq x (char-code #\f))
		 (push (decode-float32 (subseq acc 0 4)) 
		       result)
		 (setf acc (subseq acc 4)))
		((eq x (char-code #\s))
		 (let ((pointer (padded-length (position 0 acc))))
		   (push (decode-string 
			  (subseq acc 0 pointer))
			 result)
		   (setf acc (subseq acc pointer))))
		((eq x (char-code #\b)) 
		 (let* ((size (decode-int32 (subseq acc 0 4)))
                        (end (padded-length (+ 4 size))))
                   (push (decode-blob (subseq acc 0 end)) 
                         result)
                   (setf acc (subseq acc end))))
		((eq x (char-code #\d))
		 (push (decode-float64 (subseq acc 0 8)) result)
		 (setf acc (subseq acc 8)))
		(t (error "unrecognised typetag ~a(~a)" (code-char x) x))))
	   tags)
      (nreverse result))))





(defun encode-float64 (f)
  (encode-int64 (ieee-floats:encode-float64 f)))


(defun decode-float64 (s)
  (ieee-floats:decode-float64 (decode-uint64 s)))

(defun encode-int64 (i)
  (declare (type integer i))
  (let ((buf (make-sequence
	      '(vector (unsigned-byte 8)) 8)))
    (macrolet ((set-byte (n)
		 `(setf (elt buf ,n)
			(logand #xff (ash i ,(* 8 (- n 7)))))))
      (set-byte 0)
      (set-byte 1)
      (set-byte 2)
      (set-byte 3)
      (set-byte 4)
      (set-byte 5)
      (set-byte 6)
      (set-byte 7))
    buf))

(defun decode-uint64 (s)
  "8 byte -> 64 bit unsigned int"
  (let ((i (+ (ash (elt s 0) 56)
	      (ash (elt s 1) 48)
	      (ash (elt s 2) 40)
	      (ash (elt s 3) 32)
	      (ash (elt s 4) 24)
	      (ash (elt s 5) 16)
	      (ash (elt s 6) 8)
	      (elt s 7))))
    i))


(defun encode-int16 (i)
  (subseq (encode-int32 i) 2))



(defun encode-blob (blob)
  (let* ((bl (length blob))
	 (mod-length (mod bl 4))
	 (padding-length (if (zerop mod-length) 0 (- 4 mod-length))))
    (osc::cat (osc::encode-int32 bl) blob
	      (osc::pad padding-length))))      




(defconstant +unix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))
(defconstant +2^32+ (expt 2 32))
(defconstant +2^32/million+ (/ +2^32+ (expt 10 6)))
(defconstant +usecs+ (expt 10 6))

(defun encode-timetag (utime)
  "encodes an osc timetag from a universal-time and 32bit 'sub-second' part.
   for an 'instantaneous' timetag use (encode-timetag :now) 
   for a timetag with the current time use (encode-timetag :time)"
  (cond
    ;; a 1bit timetag will be interpreted as 'imediately' 
    ((equalp utime :now)
     #(0 0 0 0 0 0 0 1)) 
    ((numberp utime) (encode-int64 (make-timetag utime)))
    (t (error "the time or subsecond given is not an integer or float"))))

(defun decode-timetag (timetag)
  (if (equalp timetag #(0 0 0 0 0 0 0 1))
      1
      (decode-uint64 timetag)))

(defun make-timetag (unix-time-in-seconds)
  (multiple-value-bind (secs subsecs)
      (floor unix-time-in-seconds)
    (secs+usecs->timetag (+ +unix-epoch+ secs) (subsecs->microseconds subsecs))))

(defun secs+usecs->timetag (secs usecs)
  (setf secs (ash secs 32))   ; Make seconds the top
  (let ((usec-offset
	     (round (* usecs +2^32/MILLION+))))	; Fractional part.
	(+ secs usec-offset)))

(defun subsecs->microseconds (subsecs)
  (round (* subsecs +usecs+)))

(defun microseconds->subsecs (usecs)
  (declare (type (integer 0 1000000) usecs))
  (coerce (/ usecs  +usecs+) 'double-float))
