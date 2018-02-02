(in-package #:sc)


(defgeneric optimize-sub (ugen))
(defgeneric optimize-add-neg (ugen))
(defgeneric optimize-to-madd (ugen))
(defgeneric optimize-to-sum3 (ugen))
(defgeneric optimize-to-sum4 (ugen))

;;; unary-operator
(defclass unary-operator (pure-ugen)
  ())

(defmethod optimize-graph ((ugen unary-operator))
  (perform-dead-code-elimination ugen))

(defmethod new1 ((ugen unary-operator) &rest inputs)
  (setf (special-index ugen) (first inputs)
	(inputs ugen) (cdr inputs)
	(rate ugen) (rate (car (inputs ugen))))
  (add-to-synth ugen))

(defmacro def-unary-op (name basic-function &key special-index)
  (alexandria:with-gensyms (op-new)
    `(defun ,name (in1)
       (let ((,op-new (lambda (cls &rest args)
			(if (numberp (second args)) (apply ,basic-function (cdr args))
			    (apply #'ugen-new "UnaryOpUGen" nil cls #'identity :bipolar args)))))
	 (multinew ,op-new 'unary-operator ,special-index in1)))))



(def-unary-op neg (lambda (a) (* -1 a))
  :special-index 0)

(def-unary-op reciprocal (lambda (a) (/ 1.0 a))
  :special-index 16)

(def-unary-op abs~ #'abs
  :special-index 5)

(def-unary-op floor~ #'floor
  :special-index 9)

(def-unary-op ceil~ (lambda (in) (multiple-value-bind (val ignore)
				     (ceiling in)
				   (declare (ignore ignore))
				   val))
  :special-index 8)

(def-unary-op frac (lambda (a) (second (multiple-value-list (round a))))
  :special-index 10)

(def-unary-op sign (lambda (a) (cond ((> a 0) 1.0)
				     ((< a 0) -1.0)
				     (t 0.0)))
  :special-index 11)

(def-unary-op squared (lambda (a) (* a a))
  :special-index 12)

(def-unary-op cubed (lambda (a) (* a a a))
  :special-index 13)

(def-unary-op sqrt~ #'sqrt
  :special-index 14)

(def-unary-op exp~ #'exp
  :special-index 15)

(def-unary-op midicps (lambda (note) (* 440 (expt 2 (/ (- note 69) 12.0))))
  :special-index 17)

(def-unary-op cpsmidi (lambda (note) (* 440 (expt 2 (/ (- note 69) 12.0))))
  :special-index 18)

(def-unary-op midiratio (lambda (note) (expt 2 (* note 0.083333333333)))
  :special-index 19)

(def-unary-op dbamp (lambda (db) (expt 10.0 (* db .05)))
  :special-index 21)

(def-unary-op ampdb (lambda (amp) (* (log amp 10.0d0) 20))
  :special-index 22)

(def-unary-op log~ (lambda (number) (log number))
  :special-index 25)

(def-unary-op log2~ (lambda (number) (log number 2))
  :special-index 26)

(def-unary-op log10~ (lambda (number) (log number 10))
  :special-index 27)

(def-unary-op sin~ #'sin
  :special-index 28)

(def-unary-op cos~ #'cos
  :special-index 29)

(def-unary-op tan~ #'tan
  :special-index 30)

(def-unary-op tanh~ #'tanh
  :special-index 36)

(def-unary-op distort (lambda (x) (/ x (+ 1.0d0 (abs x))))
  :special-index 42)

(def-unary-op softclip (lambda (x) (let ((abs-x (abs x)))
                                     (if (<= abs-x 0.5) x (/ (- abs-x 0.25) x))))
  :special-index 43)

;;; binary-operator

(defclass binary-operator (pure-ugen) ())

(defmethod new1 ((ugen binary-operator) &rest inputs)
  (setf (special-index ugen) (first inputs)
	(inputs ugen) (cdr inputs)
	(rate ugen) (if (apply #'> (mapcar #'rate-number (inputs ugen))) (rate (first (inputs ugen)))
			(rate (second (inputs ugen)))))
  (add-to-synth ugen))

(defmacro def-binary-op (name basic-function (&key special-index) &body condition)
  (alexandria:with-gensyms (op-new cls sp-index)
    `(defun ,name (in1 in2)
       (let ((,op-new (lambda (,cls ,sp-index in1 in2)
			(if (every #'numberp (list in1 in2)) (apply ,basic-function (list in1 in2))
			    (cond ,@condition
				  (t (apply #'ugen-new "BinaryOpUGen" nil ,cls #'identity :bipolar
					    (list ,sp-index in1 in2))))))))
	 (multinew ,op-new 'binary-operator ,special-index in1 in2)))))

(def-binary-op add '+
    (:special-index 0)
  ((equalp 0 in1) in2)
  ((equalp 0 in2) in1))

(def-binary-op minus '-
    (:special-index 1)
  ((equalp 0 in1) (neg in2))
  ((equalp 0 in2) in1))

(def-binary-op mul '*
    (:special-index 2)
  ((equalp 0 in1) 0.0)
  ((equalp 0 in2) 0.0)
  ((equalp 1 in1) in2)
  ((equalp -1 in1) (neg in2))
  ((equalp 1 in2) in1)
  ((equalp -1 in2) (neg in1)))

(def-binary-op divide (lambda (in1 in2) (/ in1 in2 1.0))
    (:special-index 4)
  ((equalp 1.0 in2) in1)
  ((equalp -1.0 in2) (neg in1)))


(defun optimize-add (ugen)
  (let ((optimized-ugen (optimize-to-sum3 ugen)))
    (unless optimized-ugen (setf optimized-ugen (optimize-to-sum4 ugen)))
    (unless optimized-ugen (setf optimized-ugen (optimize-to-madd ugen)))
    (unless optimized-ugen (setf optimized-ugen (optimize-add-neg ugen)))
    (when optimized-ugen
      (replace-ugen (synthdef ugen) ugen optimized-ugen)
      (optimize-graph optimized-ugen))))


(defmethod optimize-sub (ugen)
  (destructuring-bind (a b) (inputs ugen)
    (let (replacement)
      (when (and (typep b 'unary-operator) (= (special-index b) 0))
	(if (= (length (descendants b)) 1) (alexandria:removef (children (synthdef ugen)) b)
	    (alexandria:removef (descendants b) ugen))
	(setf replacement (make-instance 'binary-operator :synthdef (synthdef ugen)
							  :name "BinaryOpUGen"
							  :inputs (list a (nth 0 (inputs b)))
							  :rate (rate ugen)))
	(replace-ugen (synthdef ugen) ugen replacement)
	(optimize-graph replacement)))))

(defmethod optimize-add-neg (ugen)
  (let ((comp (lambda (ugen) (and (typep ugen 'unary-operator) (= (special-index ugen) 0)))))
    (destructuring-bind (a b) (inputs ugen)
      (destructuring-bind (x y)
	  (cond ((funcall comp b) (list b a))
		((funcall comp a) (list a b))
		(t (list nil nil)))
	(when (and x y)
	  (if (= (length (descendants x)) 1) (alexandria:removef (children (synthdef ugen)) x)
	      (alexandria:removef (descendants x) ugen))
	  (minus a (nth 0 (inputs b))))))))


(defmethod optimize-graph ((ugen binary-operator))
  (when (perform-dead-code-elimination ugen)
    (return-from optimize-graph))
  (when (= (special-index ugen) 0)
    (return-from optimize-graph (optimize-add ugen)))
  (when (= (special-index ugen) 1)
    (optimize-sub ugen)))


(defun +~ (&rest args)
  (reduce 'add args))

(defun -~ (&rest args)
  (reduce 'minus args))

(defun *~ (&rest args)
  (reduce 'mul args))

(defun /~ (&rest args)
  (reduce 'divide args))

;; (defun my/~ (&rest args)
;;   "(/ (in bus) number) 가 생각과 다르게 나와서 (* (in bus) (/ 1.0 number)) 로 우회."
;;   (labels ((div (a b)
;; 	     (mul a (reciprocal b))))
;;     (reduce #'div args)))

(def-binary-op mod~ #'mod
    (:special-index 5))

(def-binary-op round~ (lambda (a b) (let* ((round (round a b))
					   (lo (* round b))
					   (hi (* (+ 1 round) b)))
				      (if (< (abs (- lo a))
					     (abs (- hi a)))
					  lo hi)))
    (:special-index 19))

(def-binary-op trunc (lambda (a b) (* b (truncate a b)))
    (:special-index 21))

(def-binary-op == #'=
    (:special-index 6))

(def-binary-op <~ #'<
    (:special-index 8))

(def-binary-op >~ #'>
    (:special-index 9))

(def-binary-op <=~ #'<=
    (:special-index 10))

(def-binary-op >=~ #'>=
    (:special-index 11))

(def-binary-op fold2 (lambda (a b)
                       (fold.ir a (*~ b -1) b))
    (:special-index 44))

(def-binary-op min~ #'min
    (:special-index 12))

(def-binary-op max~ #'max
    (:special-index 13))

(def-binary-op expt~ #'expt
    (:special-index 25))

(def-binary-op <! #'(lambda (in1 in2) (declare (ignore in2)) in1)
    (:special-index 46))

(def-binary-op logand~ #'logand
    (:special-index 14))

(def-binary-op logior~ #'logior
    (:special-index 15))


(def-binary-op << #'(lambda (in1 in2) (ash in1 in2))
    (:special-index 26))

(def-binary-op >> #'(lambda (in1 in2) (ash in1 (- in2)))
    (:special-index 27))

(defun ash~ (in1 in2)
  (if (plusp in2) (<< in1 in2)
      (>> in1 (- in2))))
;;;
;;;
;;; 

(defclass MulAdd (ugen) ())

(defun can-be-mul-add (in mul add)
  (if (eql (rate in) :audio) t
      (if (and (eql (rate in) :control)
	       (or (eql (rate mul) :control) (eql (rate mul) :scalar))
	       (or (eql (rate add) :control) (eql (rate add) :scalar)))
	  t nil)))

(defmethod new1 ((ugen muladd) &rest inputs)
  (setf (inputs ugen) inputs)
  (setf (rate ugen) (apply #'max (mapcar #'rate-number inputs)))
  (setf (rate ugen) (case (rate ugen) (2 :audio) (1 :control) (0 :scalar)))
  (destructuring-bind (in mul add) inputs
    (when (equalp mul 0.0) (return-from new1 add))
    (let ((minus (if (equalp mul -1.0) t nil))
	  (nomul (if (equalp mul 1.0) t nil))
	  (noadd (if (equalp add 0.0) t nil)))
      (when (and nomul noadd) (return-from new1 in))
      (when (and minus noadd) (return-from new1 (neg in)))
      (when noadd (return-from new1 (mul in mul)))
      (when minus (return-from new1 (minus add in)))
      (when nomul (return-from new1 (add in add)))
      (when (can-be-mul-add in mul add)
	(return-from new1 (add-to-synth ugen)))
      (when (can-be-mul-add mul in add)
	(setf (inputs ugen) (list mul in add))
	(return-from new1 (add-to-synth ugen)))
      (add (mul in mul) add))))

(defun mul-add (in &optional (mul 1.0) (add 0.0))
  (multinew (lambda (cls &rest inputs) 
	      (apply #'ugen-new "MulAdd" nil cls #'identity :bipolar inputs))
	    'muladd in mul add))

(defmethod optimize-to-madd (ugen)
  (let ((comp (lambda (ugen) (and (typep ugen 'binary-operator)
				  (= (special-index ugen) 2)
				  (= (length (descendants ugen)) 1)))))
    (destructuring-bind (a b) (inputs ugen)
      (destructuring-bind (x y)
	  (cond ((funcall comp a) (list a b))
		((funcall comp b) (list b a))
		(t (list nil nil)))
	(when (and x y)
	  (destructuring-bind (x0 x1)
	      (cond ((can-be-mul-add (nth 0 (inputs x)) (nth 1 (inputs x)) y) (inputs x))
		    ((can-be-mul-add (nth 1 (inputs x)) (nth 0 (inputs x)) y) (reverse (inputs x)))
		    (t (return-from optimize-to-madd nil)))
	    (alexandria:removef (children (synthdef ugen)) x)
	    (mul-add x0 x1 y)))))))

;;; Sum3
(defclass sum3 (ugen) ())

(defmethod new1 ((ugen sum3) &rest inputs)
  (destructuring-bind (in0 in1 in2) inputs
    (when (equalp in2 0.0) (return-from new1 (add in0 in1)))
    (when (equalp in1 0.0) (return-from new1 (add in0 in2)))
    (when (equalp in0 0.0) (return-from new1 (add in1 in2)))
    (let* ((arg-list (list in0 in1 in2))
	   (rate (rate arg-list)))
      (setf arg-list (sort arg-list #'>= :key #'rate-number))
      (setf (inputs ugen) arg-list)
      (setf (rate ugen) rate)
      (add-to-synth ugen))))

(defun sum3 (in0 in1 in2)
  (multinew
   (lambda (cls &rest inputs) (apply #'ugen-new "Sum3" nil cls #'identity :bipolar inputs))
   'sum3 in0 in1 in2))

(defmethod optimize-to-sum3 (ugen)
  (let ((comp (lambda (ugen) (and (typep ugen 'binary-operator)
				  (= (special-index ugen) 0)
				  (= (length (descendants ugen)) 1)))))
    (destructuring-bind (a b) (inputs ugen)
      (destructuring-bind (x y)
	  (cond ((funcall comp a) (list a b))
		((funcall comp b) (list b a))
		(t (list nil nil)))
	(when (and x y)
	  (alexandria:removef (children (synthdef ugen)) x)
	  (sum3 (nth 0 (inputs x)) (nth 1 (inputs x)) y))))))


;;; Sum3
(defclass sum4 (ugen) ())

(defmethod new1 ((ugen sum4) &rest inputs)
  (destructuring-bind (in0 in1 in2 in3) inputs
    (when (equalp in0 0.0) (return-from new1 (sum3 in1 in2 in3)))
    (when (equalp in1 0.0) (return-from new1 (sum3 in0 in2 in3)))
    (when (equalp in2 0.0) (return-from new1 (sum3 in0 in1 in3)))
    (when (equalp in3 0.0) (return-from new1 (sum3 in0 in1 in2)))
    (let* ((arg-list (list in0 in1 in2 in3))
	   (rate (rate arg-list)))
      (setf arg-list (sort arg-list #'>= :key #'rate-number))
      (setf (inputs ugen) arg-list)
      (setf (rate ugen) rate)
      (add-to-synth ugen))))

(defun sum4 (in0 in1 in2 in3)
  (multinew
   (lambda (cls &rest inputs) (apply #'ugen-new "Sum4" nil cls #'identity :bipolar inputs))
   'sum4 in0 in1 in2 in3))

(defmethod optimize-to-sum4 (ugen)
  (let ((comp (lambda (ugen) (and (typep ugen 'sum3)
				  (= (length (descendants ugen)) 1)))))
    (destructuring-bind (a b) (inputs ugen)
      (destructuring-bind (x y)
	  (cond ((funcall comp a) (list a b))
		((funcall comp b) (list b a))
		(t (list nil nil)))
	(when (and x y)
	  (alexandria:removef (children (synthdef ugen)) x)
	  (sum4 (nth 0 (inputs x)) (nth 1 (inputs x)) (nth 2 (inputs x)) y))))))


;;; operations ugen
;;; UGen 연산자 -------------------------------------------------------------

(defun if~ (condition true false)
  (let ((mul (mul condition (minus true false))))
    (add mul false)))

(defun madd (ugen &optional (mul 1.0) (add 0.0))
  (mul-add ugen mul add))

(defmethod _range (cls (ugen ugen) &optional (lo 0.0) (hi 1.0))
  (declare (ignore cls))
  (let ((mul) (add))
    (if (eql :bipolar (signal-range ugen)) (setf mul (mul (minus hi lo) 0.5)
						 add (add mul lo))
	(setf mul (minus hi lo) add lo))
    (madd ugen mul add)))

(defun range (ugen &optional (lo 0.0) (hi 1.0))
  (multinew #'_range nil ugen lo hi))

(defmethod _exp-range (cls (ugen ugen) &optional (lo 1.0) (hi 2.0))
  (declare (ignore cls))
  (let ((range (signal-range ugen)))
    (lin-exp ugen (if (eq range :bipolar) -1 0) 1 lo hi)))

(defun exp-range (ugen &optional (lo 1.0) (hi 2.0))
  (multinew #'_exp-range nil ugen lo hi))

(defun unipolar (ugen &optional (mul 1))
  (range ugen 0 mul))

(defun bipolar (ugen &optional (mul 1))
  (range ugen (neg mul) mul))

(defgeneric clip (in &optional lo hi))

(defun clip~ (in &optional lo hi)
  (clip.ir in lo hi))

(defun prune (ugen min max type)
  (let ((op (lambda (cls ugen min max type)
	      (declare (ignore cls))
	      (ecase type
		(:minmax (clip ugen min max))
		(:min (max~ ugen min))
		(:max (min~ ugen max))))))
    (multinew op nil ugen min max type)))

(defun sum (array)
  (let ((sum (first array)))
    (dolist (arr (cdr array) sum)
      (setf sum (+~ sum arr)))))

(defmethod unbubble ((list list))
  (cond ((zerop (length list)) nil)
	((= 1 (length list)) (car list))
	(t list)))

(defun bubble (list)
  (list list))

(defun flop (lists)
  (labels ((this-nth (n list)
	     (nth (mod n (length list)) list)))
    (let* ((lists (mapcar #'alexandria:ensure-list lists))
	   (len (apply #'max (mapcar #'length lists))))
      (loop repeat len
	    for i from 0
	    collect (mapcar (lambda (list) (this-nth i list)) lists)))))

(defun clump (lists n)
  (loop for i from 0 below (length lists) by n
	collect (let ((result nil))
		  (dotimes (k n result)
		    (let ((val (nth (+ i k) lists)))
		      (setf result (append result (if val (list val) val))))))))

(defun mix (array)
  (let* ((reduce-col (clump (alexandria:ensure-list array) 4))
	 (mixed-col (loop for a in reduce-col
			  collect (cond ((= (length a) 4) (apply #'sum4 a))
					((= (length a) 3) (apply #'sum3 a))
					(t (sum a))))))
    (cond ((< (length mixed-col) 3) (sum mixed-col))
	  ((= (length mixed-col) 3) (apply #'sum3 mixed-col))
	  (t (mix mixed-col)))))

(defun mean (array)
  (if (every #'numberp array) (alexandria:mean array)
      (/~ (sum array) (length array))))

(defun product (list &optional f)
  (let ((product 1))
    (if f (loop for elem in list for i from 0
		do (setf product (*~ product (funcall f elem i))))
	(loop for elem in list
	      do (setf product (*~ product elem))))
    product))

(defun dup (object &optional (n 2))
  (duplicate object n))

(defmethod duplicate (self (n integer))
  (if (not (listp self)) (make-list n :initial-element self)
      (loop for i from 0 below n
	 collect (copy-list self))))

(defmethod duplicate ((self function) (n integer))
  (loop for i from 0 below n collect (funcall self i)))

(defmethod duplicate ((self function) (n list))
  (loop for i in n collect (funcall self i)))

