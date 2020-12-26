(in-package #:sc)

(defugen (done "Done") (src)
  ((:kr (multinew new 'ugen src))))

(defugen (free-self "FreeSelf")
    (in)
  ((:kr (multinew new 'ugen in))))

(defugen (pause-self "PauseSelf")
    (src)
  ((:kr (multinew new 'ugen src))))

(defugen (free-self-when-done "FreeSelfWhenDone")
    (src)
  ((:kr (multinew new 'ugen src))))

(defugen (pause-self-when-done "PauseSelfWhenDone")
    (src)
  ((:kr (multinew new 'ugen src))))

(defugen (puase "Pause")
    (gate id)
  ((:kr (multinew new 'ugen gate id))))

(defugen (free "Free") (trig id)
  ((:kr (multinew new 'ugen trig id))))



(defgeneric make-env-array-from-env (env))

(defun process-env (env-ugen)
  (let ((inputs (inputs env-ugen)))
    (let ((env-array (alexandria:last-elt inputs)))
      (setf (inputs env-ugen)
	(append (butlast inputs) (map 'list #'identity env-array)))))
  env-ugen)

(defugen (env-gen "EnvGen")
    (envelope &key (gate 1) (level-scale 1) (level-bias 0) (time-scale 1) (act :no-action))
  ((:ar (unbubble (mapcar #'process-env (multinew new 'ugen gate level-scale level-bias time-scale (act act)
						  (make-env-array-from-env envelope)))))
   (:kr (unbubble (mapcar #'process-env (multinew new 'ugen gate level-scale level-bias time-scale (act act)
						  (make-env-array-from-env envelope)))))))

(defugen (linen "Linen")
    (&optional (gate 1.0) (attack-time 0.01) (sus-level 1.0) (release-time 1.0) &key (act :no-action))
  ((:kr (multinew new 'ugen gate attack-time sus-level release-time (act act)))))

;;;
;;; Env Class
;;; 

(defclass env ()
  ((levels :initarg :levels :accessor levels)
   (times :initarg :times :accessor times)
   (curve-number :initarg :curve-number :accessor curve-number)
   (curve-value :initarg :curve-value :accessor curve-value)
   (release-node :initarg :release-node :accessor release-node)
   (loop-node :initarg :loop-node :accessor loop-node)))

(defmethod initialize-instance :after ((self env) &key)
  (with-slots (levels times curve-number release-node loop-node) self
    (assert (= (1- (length levels)) (length times)))
    (assert (>= (length times) (length curve-number)))))

(defparameter +env-shape-table+
  (let ((table (make-hash-table)))
    (labels ((sethash (&rest hash)
	       (loop repeat (/ (length hash) 2)
		    for i = 0 then (+ i 2)
		    do (setf (gethash (nth i hash) table) (nth (+ i 1) hash)))
	       table)) 
      (sethash :step 0
	       :lin 1
	       :linear 1
	       :exp 2
	       :exponential 2
	       :sin 3
	       :sine 3
	       :wel 4
	       :welch 4
	       :sqr 6
	       :cub 7
	       :cubed 7
	       :hold 8))))

(defmethod env-shape-number ((self symbol))
  (let ((value (gethash self +env-shape-table+)))
    (if value value (error (format nil "~s is not a valid type of envelope curve." self)))))

(defmethod env-shape-number ((self ugen))
  5)

(defmethod env-shape-number ((self number))
  5)

(defun env-shape-numbers (curves)
  (loop for curve in curves collect (env-shape-number curve)))


(defmethod make-env-array-from-env ((env list))
  env)

(defmethod make-env-array-from-env ((env env))
  (with-slots (levels times curve-number curve-value release-node loop-node) env
    (let* ((env-segs (mapcar #'sc::lst-operation
			     (mapcar #'alexandria:ensure-list (list levels times curve-number curve-value)))))
      (flet ((mk-deep-lst (x)
	       (if (every #'atom x) (list x) x)))
	(let ((deep-lst (mapcar #'mk-deep-lst env-segs)))
	  (destructuring-bind (lvs tms cns cvs)
	      deep-lst
	    (flet ((wrap (idx lst)
		     (nth (mod idx (length lst)) lst)))
	      (let ((env-arrays (loop for i from 0 below (apply #'max (mapcar #'length deep-lst))
				      collect (let* ((env-array (make-array 3)))
						(setf (aref env-array 0) (car (wrap i lvs))
						      (aref env-array 1) (length (wrap i tms))
						      (aref env-array 2)
						      (alexandria:flatten
							     (lst-operation
							      (list
							       (cdr (wrap i lvs))
							       (wrap i tms)
							       (wrap i cns)
							       (wrap i cvs)))))
						env-array))))
		(setf env-arrays (lst-operation (list release-node loop-node env-arrays)))
		(mapcar #'(lambda (e)
			    (let ((env-array (third e)))
			      (make-array (+ 4 (length (aref env-array 2)))
				:initial-contents (append (list (aref env-array 0)
								(aref env-array 1)
								(first e)
								(second e))
							  (aref env-array 2)))))
			env-arrays)))))))))

(defun env (levels times &optional (curve :lin) (release-node -99) (loop-node -99))
  (let* ((curves (alexandria:ensure-list curve))
	 (curve-value
	   (loop for curve in curves
		 collect
		 (etypecase curve (symbol 0) (number curve) (ugen curve)))))
    (make-instance 'env :levels levels :times times :curve-number (env-shape-numbers curves)
			:curve-value curve-value
			:release-node release-node :loop-node loop-node)))

(defun triangle (&optional (dur 1.0) (level 1.0))
  (let ((dur (sc::*~ dur .5)))
    (env (list 0 level 0) (list dur dur))))

(defun sine (&optional (dur 1.0) (level 1.0))
  (let ((dur (sc::*~ dur .5)))
    (env (list 0 level 0) (list dur dur) :sine)))

(defun perc (&optional (attack 0.01) (release 1.0) (level 1.0) (curve -4.0))
  (env (list 0 level 0) (list attack release) curve))

(defun linen (&optional (attack 0.01) (sus-time 1.0) (release 1.0) (level 1.0) (curve :lin))
  (env (list 0 level level 0) (list attack sus-time release) curve))

(defun cutoff (&optional (release 0.1) (level 1.0) (curve :lin))
  (let* ((curve-no (env-shape-number curve))
	 (release-level (if (= 2 curve-no) (dbamp -100) 0)))
    (env (list level release-level) (list release) curve 0)))

(defun dadsr (&optional (delay-time 0.1) (attack-time 0.01) (decay-time 0.3)
	      (sustain-level 0.5) (release-time 1.0) (peak-level 1.0) (curve -4.0) (bias 0.0))
  (env (sc::add (list 0 0 peak-level (sc::mul peak-level sustain-level) 0) bias)
       (list delay-time attack-time decay-time release-time)
       curve
       3))

(defun adsr (&optional (attack-time 0.01) (decay-time 0.3) (sustain-level .5)
	     (release-time 1.0) (peak-level 1.0) (curve -4.0) (bias 0.0))
  (env (sc::add (list 0 peak-level (sc::mul peak-level sustain-level) 0) bias)
       (list attack-time decay-time release-time)
       curve
       2))

(defun asr (&optional (attack-time 0.01) (sustain-level 1.0) (release-time 1.0) (curve -4.0))
  (env (list 0 sustain-level 0)
       (list attack-time release-time)
       curve
       1))

;;; Interpolation formulas adapted from Overtone

(defun step-interpolation (pos y1 y2)
  (if (zerop pos)
      y1
      y2))

(defun hold-interpolation (pos y1 y2)
  (if (< pos 1)
      y1
      y2))

(defun linear-interpolation (pos y1 y2)
  (+ y1
     (* (- y2 y1)
	pos)))

;;; duplicate
;; (defun exponential-interpolation (pos y1 y2)
;;   (let ((y1 (1+ y1))
;; 	(y2 (1+ y2)))
;;     (1-
;;      (* y1
;; 	(expt (/ y2 y1)
;; 	      pos)))))

(defun exponential-interpolation (pos y1 y2)
  (let ((y1 (if (zerop y1) 0.0001 y1))
	(y2 (if (zerop y2) 0.0001 y2)))
    (assert (or (and (plusp y1) (plusp y2))
		(and (< y1 0) (< y2 0)))
	    (y1 y2)
	    "~S and ~S should have the same sign." y1 y2)
    (* y1
       (expt (/ y2 y1)
	     pos))))

(defun sine-interpolation (pos y1 y2)
  (+ y1
     (* (- y2 y1)
	(+ (* -1 (cos (* pi pos)) 0.5) 0.5))))

(defun welch-interpolation (pos y1 y2)
  (if (< y1 y2)
      (+ y1
	 (* (- y2 y1)
	    (sin (* pi 0.5 pos))))
      (- y2
	 (* (- y2 y1)
	    (sin (* pi 0.5 (- 1 pos)))))))

(defun curve-interpolation (pos y1 y2 &optional (curvature 0))
  (if (< (abs curvature) 0.0001)
      (+ (* pos (- y2 y1))
	 y1)
      (let ((denominator (- 1.0 (exp curvature)))
	    (numerator (- 1.0 (exp (* pos curvature)))))
	(+ y1
	   (* (- y2 y1) (/ numerator denominator))))))

(defun squared-interpolation (pos y1 y2)
  (let* ((y1-s (sqrt y1))
	 (y2-s (sqrt y2))
	 (yp (+ y1-s (* pos (- y2-s y1-s)))))
    (* yp yp)))

(defun cubed-interpolation (pos y1 y2)
  (let* ((y1-c (expt y1 0.3333333))
	 (y2-c (expt y2 0.3333333))
	 (yp (+ y1-c (* pos (- y2-c y1-c)))))
    (* yp yp yp)))

(defun interpolation (curve pos y1 y2 &optional curve-val)
  (if (= 5 curve)
      (curve-interpolation pos y1 y2 curve-val)
      (let ((fun (case curve
		   (0 #'step-interpolation)
		   (1 #'linear-interpolation)
		   (2 #'exponential-interpolation)
		   (3 #'sine-interpolation)
		   (4 #'welch-interpolation)
		   (6 #'squared-interpolation)
		   (7 #'cubed-interpolation)
		   (8 #'hold-interpolation))))
	(funcall fun pos y1 y2))))

(defun times-and-curves-as-list (times curve-numbers curve-values)
  (let* ((times-list (loop :for i :in (append (list 0) times)
			  :summing i :into ir
			  :collect ir))
	 (curve-number-list (if (= 1 (length curve-numbers))
			       (make-list (1- (length times-list))
					  :initial-element (car curve-numbers))
			       curve-numbers))
	 (curve-value-list (if (= 1 (length curve-values))
			      (make-list (1- (length times-list))
					 :initial-element (car curve-values))
			      curve-values)))
    (values times-list curve-number-list curve-value-list)))

(defmethod env-at ((env env) time)
  "Return the value of the envelope ENV at TIME."
  (with-accessors ((levels levels) (times times)
		   (curve-number curve-number)
		   (curve-value curve-value))
      env
    (multiple-value-bind (times-list curve-number-list curve-value-list)
	(times-and-curves-as-list times curve-number curve-value)
      (assert (<= time (car (last times-list))) (time)
	      "~S must be less or equal the total duration of the envelope." time)
      (loop :for (time-a time-b) :on times-list
	    :for (level-a level-b) :on levels
	    :for curve-n :in curve-number-list
	    :for curve-v :in curve-value-list
	    :until (>= time-b time)
	    :finally (return (interpolation curve-n
					    (/ (- time time-a)
					       (- time-b time-a))
					    level-a
					    level-b
					    curve-v))))))

(defmethod env-as-signal ((env env) (frames integer))
  "Return a list of length FRAMES created by sampling ENV at FRAMES number of intervals."
  (with-accessors ((levels levels) (times times)
		   (curve-number curve-number)
		   (curve-value curve-value))
      env
    (multiple-value-bind (times-list curve-number-list curve-value-list)
	(times-and-curves-as-list times curve-number curve-value)
      (loop :with max-time := (car (last times-list))
	    :with step := (/ max-time
			     (1- frames))
	    :for frame :from 0 :below frames
	    :for pointer := 0 :then (if (> x x2)
					(incf pointer)
					pointer)
	    :for x1 := (elt times-list pointer)
	    :for y1 := (elt levels pointer)
	    :for x2 := (elt times-list (1+ pointer))
	    :for y2 := (elt levels (1+ pointer))
	    :for x := (* frame step)
	    :for pos := (/ (- x x1) (- x2 x1))
	    :for curve := (elt curve-number-list pointer)
	    :for curve-val := (when (= 5 curve) (elt curve-value-list pointer))
	    :if (> x x2) :do (decf frame)
	      :else :collect (interpolation curve pos y1 y2 curve-val)))))
