
(in-package #:sc)

(defugen (demand "Demand") (trig reset demand-ugens)
  ((:ar (let ((demand-ugens (alexandria:ensure-list demand-ugens)))
	  (apply #'multinew new 'multiout-ugen (length demand-ugens)  trig reset demand-ugens)))
   (:kr (let ((demand-ugens (alexandria:ensure-list demand-ugens)))
	  (apply #'multinew new 'multiout-ugen (length demand-ugens)  trig reset demand-ugens)) ))
  :check-fn #'check-same-rate-as-first-input)


(defun check-duty (ugen)
  (when (eql (rate (nth 0 (inputs ugen))) :demand)
      (unless (find (rate (nth 1 (inputs ugen))) (list :demand :scalar (rate ugen)))
        (error (format nil "Reset input cannot be ~a rate (input: ~a rate: ~a)."
		       (rate ugen) (nth 1 (inputs ugen)) (rate (nth 1 (inputs ugen))))))))

(defugen (duty "Duty") (&optional (dur 1.0) &key (reset 0.0) (level 1.0) (act :no-action))
  ((:ar (multinew new 'ugen dur reset (act act) level))
   (:kr (multinew new 'ugen dur reset (act act) level)))
  :check-fn #'check-duty)

(defugen (t-duty "TDuty") (&optional (dur 1.0) &key (reset 0.0) (level 1.0) (act :no-action) (gap-first 0))
  ((:ar (multinew new 'ugen dur reset (act act) level gap-first))
   (:kr (multinew new 'ugen dur reset (act act) level gap-first)))
  :check-fn #'check-duty)


(defugen (demand-envgen "DemandEnvGen")
    (level dur &key (shape 1) (curve 0) (gate 1.0) (reset 1.0)
	   (level-scale 1.0) (level-bias 0.0) (time-scale 1.0) (act :no-action))
  ((:kr (multinew new 'ugen level dur shape curve gate reset level-scale level-bias time-scale (act act)))
   (:ar (progn
	  (when (or (eql (rate gate) :audio)
		    (eql (rate reset) :audio))
	    (when (not (eql (rate gate) :audio))
	      (setf gate (k2a.ar gate)))
	    (when (not (eql (rate reset) :audio))
	      (setf reset (k2a.ar reset))))
	  (multinew new 'ugen level dur shape curve gate reset level-scale level-bias time-scale (act act))))))


(defclass dugen (ugen)
  ())
;;; todo 



(defmacro def-dugen (name args &body body)
  `(progn (defun ,(car name) ,args
	    (let ((new (lambda (cls &rest inputs) (apply #'ugen-new ,(second name) :demand cls
							 #'identity :bipolar inputs))))
	      ,@body))
	  (export ',(car name))))

(def-dugen (d-series "Dseries") (&optional (start 1) (step 1) (length +inf+))
  (multinew new 'dugen length start step))

(def-dugen (d-geom "Dgeom") (&optional (start 1) (grow 2) (length +inf+))
  (multinew new 'dugen length start grow))

(def-dugen (d-bufrd "Dbufrd") (&optional (bufnum 0) (phase 0) (loop 1.0))
  (multinew new 'dugen bufnum phase loop))

(def-dugen (d-bufwr "Dbufwr") (&optional (input 0.0) (bufnum 0) (phase 0.0) (loop 1.0))
  (multinew new 'dugen bufnum phase input loop))

(def-dugen (list-dugen "ListDUGen") (list &optional (repeats 1))
  (apply 'multinew new 'dugen repeats list))

(def-dugen (d-seq "Dseq") (list &optional (repeats 1))
  (apply 'multinew new 'dugen repeats list))

(def-dugen (d-ser "Dser") (list &optional (repeats 1))
  (apply 'multinew new 'dugen repeats list))

(def-dugen (d-shuf "Dshuf") (list &optional (repeats 1))
  (apply 'multinew new 'dugen repeats list))

(def-dugen (d-rand "Drand") (list &optional (repeats 1))
  (apply 'multinew new 'dugen repeats list))

(def-dugen (d-xrand "Dxrand") (list &optional (repeats 1))
  (apply 'multinew new 'dugen repeats list))

(def-dugen (d-switch1 "Dswitch1") (list index)
  (apply 'multinew new 'dugen index list))

(def-dugen (d-switch "Dswitch") (list index)
  (apply 'multinew new 'dugen index list))

(def-dugen (d-white "Dwhite") (&optional (lo 0.0) (hi 1.0) (length +inf+))
  (multinew new 'dugen length lo hi))

(def-dugen (d-iwhite "Diwhite") (&optional (lo 0.0) (hi 1.0) (length +inf+))
  (multinew new 'dugen length lo hi))

(def-dugen (d-brown "Dbrown") (&optional (lo 0.0) (hi 1.0) (step 0.01) (length +inf+))
  (multinew new 'dugen length lo hi step))

(def-dugen (d-ibrown "Dibrown") (&optional (lo 0.0) (hi 1.0) (step 0.01) (length +inf+))
  (multinew new 'dugen length lo hi step))

(def-dugen (d-stutter "Dstutter") (n in)
  (multinew new 'dugen n in))

(def-dugen (d-once "Donce") (in)
  (multinew new 'dugen in))

(def-dugen (d-reset "Dreset") (in &optional (reset 0.0))
  (multinew new 'dugen in reset))

(def-dugen (d-poll "Dpoll") (in label &optional (run 1) (trigid -1))
  (multinew new 'dugen in label run trigid))

