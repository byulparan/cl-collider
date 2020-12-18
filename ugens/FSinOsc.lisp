(in-package #:sc)

(defugen (f-sin-osc "FSinOsc")
    (&optional (freq 440.0) (iphase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen freq iphase) mul add))
   (:kr
    (madd (multinew new 'ugen freq iphase) mul add))))

;;;
;; (defun assert-array-ref (array-ref)
;;   (labels ((max-depth (lst &optional (rank 0)) 
;; 	     (let ((lst (remove-if-not #'consp lst)))
;; 	       (cond ((null lst) (1+ rank))
;; 		     (t (max (max-depth (car lst) (+ rank 1))
;; 			     (max-depth (cdr lst) rank)))))))
;;     (let ((depth (apply #'max (mapcar #'max-depth array-ref))))
;;       (assert (> 3 depth) nil
;; 	      "too deep depth harm or amp or ring list. max depth is 2, but your max depth is ~a." depth))))

(defun flop-argument (lst)
  (assert (every #'numberp (alexandria:flatten lst)) nil
	  "Klang/Klank do not support UGens as arguments. Use Dyn-Klang/Dyn-Klank instead.")
  (unless lst (setf lst (list 1.0)))
  (flop lst))

(defclass klang (ugen) ())

(defmethod new1 ((ugen klang) &rest inputs)
  (destructuring-bind (freqscale freqoffset array-ref) inputs
    (setf (inputs ugen)
	  (append (list freqscale freqoffset)
		  (alexandria:flatten (loop for arr across array-ref collect (if (null arr) 1.0 arr)))))
    (add-to-synth ugen)))

(defugen (klang "Klang") (specificationsArrayRef &optional (freq-scale 1.0) (freq-offset 0.0))
  ((:ar ;(assert-array-ref specificationsArrayRef)
	(let ((len (length specificationsArrayRef)))
	  (unless (= 3 len)
	    (alexandria:appendf specificationsArrayRef (make-list (- 3 len)))))
	(multinew new 'klang freq-scale freq-offset 
		  (unbubble (mapcar #'(lambda (lst) (make-array (length lst) :initial-contents lst))
				    (lst-operation (mapcar #'flop-argument specificationsArrayRef))))))))

(defclass klank (ugen) ())

(defmethod new1 ((ugen klank) &rest inputs)
  (destructuring-bind (input freqscale freqoffset decayscale array-ref) inputs
    (setf (inputs ugen)
	  (append (list input freqscale freqoffset decayscale)
		  (alexandria:flatten (loop for arr across array-ref collect (if (null arr) 1.0 arr)))))
    (add-to-synth ugen)))

(defugen (klank "Klank") (specificationsArrayRef input &optional (freq-scale 1.0) (freq-offset 0.0) (decay-scale 1.0))
  ((:ar ;(assert-array-ref specificationsArrayRef)
	(let ((len (length specificationsArrayRef)))
	  (unless (= 3 len)
	    (alexandria:appendf specificationsArrayRef (make-list (- 3 len)))))
	(multinew new 'klank  input freq-scale freq-offset decay-scale
		  (unbubble (mapcar #'(lambda (lst) (make-array (length lst) :initial-contents lst))
				    (lst-operation (mapcar #'flop-argument specificationsArrayRef))))))))


(defclass dyn-klank (ugen) ())

(defmethod new1 ((ugen dyn-klank) &rest inputs)
  (destructuring-bind (array-ref input freq-scale freq-offset decay-scale) inputs
    (let ((array-ref (map 'list #'identity array-ref)))
      (ecase (rate ugen)
	(:audio (sum (ringz.ar input (alexandria:if-let ((spec (nth 0 array-ref))) spec
				       (add (mul (list 440.0) freq-scale) freq-offset))
			       (alexandria:if-let ((spec (nth 2 array-ref))) spec
				 (mul (list 1.0) decay-scale))
			       (alexandria:if-let ((spec (nth 1 array-ref))) spec (list 1.0)))))
	(:control (sum (ringz.kr input (alexandria:if-let ((spec (nth 0 array-ref))) spec
					 (add (mul (list 440.0) freq-scale) freq-offset))
				 (alexandria:if-let ((spec (nth 2 array-ref))) spec
				   (mul (list 1.0) decay-scale))
				 (alexandria:if-let ((spec (nth 1 array-ref))) spec (list 1.0)))))))))

(defugen (dyn-klank "DynKlank")
    (array-ref input &optional (freq-scale 1.0) (freq-offset 0.0) (decay-scale 1.0))
  ((:ar ;;(assert-array-ref array-ref)
	(multinew new 'dyn-klank (make-array (length array-ref) :initial-contents array-ref)
		  input freq-scale freq-offset decay-scale))
   (:kr ;;(assert-array-ref array-ref)
	(multinew new 'dyn-klank (make-array (length array-ref) :initial-contents array-ref)
		  input freq-scale freq-offset decay-scale))))

(defclass dyn-klang (ugen) ())

(defmethod new1 ((ugen dyn-klang) &rest inputs)
  (destructuring-bind (array-ref freq-scale freq-offset) inputs
    (let ((array-ref (map 'list #'identity array-ref)))
      (ecase (rate ugen)
	(:audio (sum (sin-osc.ar (alexandria:if-let ((spec (nth 0 array-ref))) spec
				   (add (mul (list 440.0) freq-scale) freq-offset))
				 (alexandria:if-let ((spec (nth 2 array-ref))) spec (list 0.0))
				 (alexandria:if-let ((spec (nth 1 array-ref))) spec (list 1.0)))))
	(:control (sum (sin-osc.kr (alexandria:if-let ((spec (nth 0 array-ref))) spec
				     (add (mul (list 440.0) freq-scale) freq-offset))
				   (alexandria:if-let ((spec (nth 2 array-ref))) spec (list 0.0))
				   (alexandria:if-let ((spec (nth 1 array-ref))) spec (list 1.0)))))))))

(defugen (dyn-klang "DynKlang")
    (array-ref &optional (freq-scale 1.0) (freq-offset 0.0))
  ((:ar ;;(assert-array-ref array-ref)
	(multinew new 'dyn-klang (make-array (length array-ref) :initial-contents array-ref)
		  freq-scale freq-offset))
   (:kr ;;(assert-array-ref array-ref)
	(multinew new 'dyn-klang (make-array (length array-ref) :initial-contents array-ref)
		  freq-scale freq-offset))))


(defugen (blip "Blip")
    (&optional (freq 440.0) (num-harm 220.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen freq num-harm) mul add))
   (:kr
    (madd (multinew new 'ugen freq num-harm) mul add))))

(defugen (saw "Saw")
    (&optional (freq 440.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq) mul add))
   (:kr (madd (multinew new 'ugen freq) mul add))))

(defugen (pulse "Pulse")
    (&optional (freq 440.0) (width 0.5) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq width) mul add))
   (:kr (madd (multinew new 'ugen freq width) mul add))))

