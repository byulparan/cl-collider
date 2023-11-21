(in-package #:sc)

(defugen (f-sin-osc "FSinOsc")
    (&optional (freq 440.0) (iphase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen freq iphase) mul add))
   (:kr
    (madd (multinew new 'ugen freq iphase) mul add))))


(defun flop-argument (lst type)
  (unless lst (setf lst (list (ecase type
				(:amps 1.0)
				(:times 1.0)
				(:phases 0.0)))))
  (flop lst))

(defclass klang (ugen) ())

(defmethod new1 ((ugen klang) &rest inputs)
  (destructuring-bind (freqscale freqoffset array-ref) inputs
    (setf (inputs ugen)
	  (append (list freqscale freqoffset)
		  (alexandria:flatten (loop for arr across array-ref collect (if (null arr) 1.0 arr)))))
    (add-to-synth ugen)))

(defugen (klang "Klang") (specificationsArrayRef &optional (freq-scale 1.0) (freq-offset 0.0))
  ((:ar (let ((len (length specificationsArrayRef)))
	  (unless (= 3 len)
	    (alexandria:appendf specificationsArrayRef (make-list (- 3 len)))))
	(multinew new 'klang freq-scale freq-offset 
		  (unbubble (mapcar #'(lambda (lst) (make-array (length lst) :initial-contents lst))
				    (lst-operation (mapcar #'flop-argument specificationsArrayRef
							   (list :freqs :amps :phases)))))))))

(defclass klank (ugen) ())

(defmethod new1 ((ugen klank) &rest inputs)
  (destructuring-bind (input freqscale freqoffset decayscale array-ref) inputs
    (setf (inputs ugen)
	  (append (list input freqscale freqoffset decayscale)
		  (alexandria:flatten (loop for arr across array-ref collect (if (null arr) 1.0 arr)))))
    (add-to-synth ugen)))

(defugen (klank "Klank") (specificationsArrayRef input &optional (freq-scale 1.0) (freq-offset 0.0) (decay-scale 1.0))
  ((:ar (let ((len (length specificationsArrayRef)))
	  (unless (= 3 len)
	    (alexandria:appendf specificationsArrayRef (make-list (- 3 len)))))
	(multinew new 'klank input freq-scale freq-offset decay-scale
		  (unbubble (mapcar #'(lambda (lst) (make-array (length lst) :initial-contents lst))
				    (lst-operation (mapcar #'flop-argument specificationsArrayRef
							   (list :freqs :amps :times)))))))))


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
  ((:ar	(multinew new 'dyn-klank (make-array (length array-ref) :initial-contents array-ref)
		  input freq-scale freq-offset decay-scale))
   (:kr	(multinew new 'dyn-klank (make-array (length array-ref) :initial-contents array-ref)
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
  ((:ar	(multinew new 'dyn-klang (make-array (length array-ref) :initial-contents array-ref)
		  freq-scale freq-offset))
   (:kr	(multinew new 'dyn-klang (make-array (length array-ref) :initial-contents array-ref)
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

