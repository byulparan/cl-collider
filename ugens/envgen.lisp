
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



(defgeneric make-list-from-env (env))

(defugen (env-gen "EnvGen")
    (envelope &key (gate 1) (level-scale 1) (level-bias 0) (time-scale 1) (act :no-action))
  ((:ar (apply 'multinew new 'ugen (append (list gate level-scale level-bias time-scale (act act))
				 (make-list-from-env envelope))))
   (:kr (apply 'multinew new 'ugen (append (list gate level-scale level-bias time-scale (act act))
				 (make-list-from-env envelope))))))

(defugen (linen "Linen")
    (&optional (gate 1.0) &key (attack-time 0.01) (sus-level 1.0) (release-time 1.0) (act :no-action))
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
  (with-slots (levels times release-node loop-node) self
    (assert (every (lambda (obj) (or (typep obj 'number) (typep obj 'ugen))) levels))
    (assert (every (lambda (obj) (or (typep obj 'number) (typep obj 'ugen))) times))
    (assert (typep release-node 'number))
    (assert (typep loop-node 'number))
    (assert (= (1- (length levels)) (length times)))))

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
	       :cubed 7))))

(defmethod env-shape-number ((self symbol))
  (let ((value (gethash self +env-shape-table+)))
    (if value value (error (format nil "~s is Not Curves Value" self)))))

(defmethod env-shape-number ((self ugen))
  5)

(defmethod env-shape-number ((self number))
  5)

(defun env-shape-numbers (curves)
  (loop for curve in curves collect (env-shape-number curve)))

(defmethod make-list-from-env ((env env))
  (with-slots (levels times curve-number curve-value release-node loop-node) env
    (let ((segments (alexandria:flatten
		     (list (car levels)
			   (length times)
			   release-node
			   loop-node
			   (loop repeat (max (1- (length levels)) (length times))
			      for i from 0 collect (list (nth (+ i 1) levels)
							 (nth i times)
							 (nth (mod i (length curve-number))
							      curve-number)
							 (nth (mod i (length curve-value))
							      curve-value)))))))
      segments)))

(defmethod make-list-from-env ((env list))
  env)

(defun env (levels times &optional (curve :lin) (release-node -99) (loop-node -99))
  (let* ((curves (su:mklist curve))
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



