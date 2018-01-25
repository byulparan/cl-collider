
;;; ...from [Osc.sc FSinOsc.sc]

(in-package #:sc)

(defugen (osc "Osc")
    (bufnum &optional (freq 440.0) (phase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen bufnum freq phase) mul add))
   (:kr
    (madd (multinew new 'pure-ugen bufnum freq phase) mul add))))

(defugen (sin-osc "SinOsc")
    (&optional (freq 440.0) (phase 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen freq phase) mul add))
   (:kr (madd (multinew new 'pure-ugen freq phase) mul add))))

(defugen (sin-osc-fb "SinOscFB")
    (&optional (freq 440.0) (feedback 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen freq feedback) mul add))
   (:kr
    (madd (multinew new 'pure-ugen freq feedback) mul add))))

(defugen (osc-n "OscN")
    (bufnum &optional (freq 440.0) (phase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen bufnum freq phase) mul add))
   (:kr
    (madd (multinew new 'pure-ugen bufnum freq phase) mul add))))

(defugen (v-osc "VOsc")
    (bufpos &optional (freq 440.0) (phase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen bufpos freq phase) mul add))
   (:kr
    (madd (multinew new 'pure-ugen bufpos freq phase) mul add))))

(defugen (c-osc "COsc")
    (bufnum &optional (freq 440.0) (beats 0.5) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen bufnum freq beats) mul add))
   (:kr
    (madd (multinew new 'pure-ugen bufnum freq beats) mul add))))

(defugen (formant "Formant")
    (&optional (fund-freq 440.0) (form-freq 1760.0) (bw-freq 880.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen fund-freq form-freq bw-freq) mul add))))

(defugen (lf-saw "LFSaw")
    (&optional (freq 440.0) (iphase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen freq iphase) mul add))
   (:kr
    (madd (multinew new 'pure-ugen freq iphase) mul add))))

(defugen (lf-par "LFPar")
    (&optional (freq 440.0) (iphase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen freq iphase) mul add))
   (:kr
    (madd (multinew new 'pure-ugen freq iphase) mul add))))

(defugen (lf-cub "LFCub")
    (&optional (freq 440.0) (iphase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen freq iphase) mul add))
   (:kr
    (madd (multinew new 'pure-ugen freq iphase) mul add))))

(defugen (lf-tri "LFTri")
    (&optional (freq 440.0) (iphase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen freq iphase) mul add))
   (:kr
    (madd (multinew new 'pure-ugen freq iphase) mul add))))



(defclass lf-gauss (ugen)
  ())

(defugen (lf-gauss "LFGauss")
    (&optional (duration 1) (width 0.1) &key (iphase 0.0) (loop 1) (action 0))
  ((:ar
    (multinew new 'lf-gauss duration width iphase loop action))
   (:kr
    (multinew new 'lf-gauss duration width iphase loop action))))

(defmethod minval ((ugen lf-gauss))
  (let ((width (nth 1 (inputs ugen))))
    (exp~ (divide 1.0 (mul -2.0 (squared (mul 1.0 width)))))))

(defmethod _range (cls (ugen lf-gauss) &optional (lo 0.0) (hi 1.0))
  (declare (ignore cls))
  (lin-lin ugen (minval ugen) 1.0 lo hi))




(defugen (lf-pulse "LFPulse")
    (&optional (freq 440.0) (iphase 0.0) (width 0.5) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen freq iphase width) mul add))
   (:kr
    (madd (multinew new 'pure-ugen freq iphase width) mul add)))
  :signal-range :unipolar)

(defugen (var-saw "VarSaw")
    (&optional (freq 440.0) (iphase 0.0) (width 0.5) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen freq iphase width) mul add))
   (:kr
    (madd (multinew new 'pure-ugen freq iphase width) mul add))))


(defugen (sync-saw "SyncSaw")
    (&optional (sync-freq 440.0) (saw-freq 440.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen sync-freq saw-freq) mul add))
   (:kr
    (madd (multinew new 'pure-ugen sync-freq saw-freq) mul add))))


(defugen (index "Index")
    (bufnum &optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen bufnum in) mul add))
   (:kr
    (madd (multinew new 'pure-ugen  bufnum in) mul add))))

(defugen (wrap-index "WrapIndex")
    (bufnum &optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen  bufnum in) mul add))
   (:kr
    (madd (multinew new 'pure-ugen bufnum in) mul add))))

(defugen (index-in-between "IndexInBetween")
    (bufnum &optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen bufnum in) mul add))
   (:kr
    (madd (multinew new 'pure-ugen  bufnum in) mul add))))

(defugen (detect-index "DetectIndex")
    (bufnum &optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen bufnum in) mul add))
   (:kr
    (madd (multinew new 'pure-ugen bufnum in) mul add))))

(defugen (shaper "Shaper")
    (bufnum &optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen bufnum in) mul add))
   (:kr
    (madd (multinew new 'pure-ugen bufnum in) mul add))))

(defugen (index-l "IndexL")
    (bufnum &optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen bufnum in) mul add))
   (:kr
    (madd (multinew new 'pure-ugen bufnum in) mul add))))



(defugen (degree-to-key "DegreeToKey")
    (bufnum &optional (in 0.0) (octave 12.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen bufnum in octave) mul add))
   (:kr
    (madd (multinew new 'pure-ugen bufnum in octave) mul add))))




(defugen (select "Select") (which array)
  ((:ar (multinew-list new 'pure-ugen (cons which array)))
   (:kr (multinew-list new 'pure-ugen (cons which array))))
  :check-fn (lambda (ugen)
	      (when (eql (rate ugen) :audio)
		(loop for i from 1 below (length (inputs ugen))
		      do (unless (eql (rate (nth i (inputs ugen))) :audio)
			   (error (format nil "input was not audio rate : ~a" (nth i (inputs ugen)))))))))

(defugen (select-x "SelectX") (which array &optional (wrap 1.0))
  ((:ar (progn
	  new wrap
	  (x-fade2.ar (select.ar (round~ which 2) array)
		      (select.ar (add (trunc which 2) 1) array)
		      (fold2 (minus (mul which 2) 1) 1))))
   (:kr (progn
	  new wrap
	  (x-fade2.kr (select.kr (round~ which 2) array)
		      (select.kr (add (trunc which 2) 1) array)
		      (fold2 (minus (mul which 2) 1) 1))))))

(defugen (lin-select-x "LinSelectX") (which array &optional (wrap 1.0))
  ((:ar (progn
	  new wrap
	  (lin-x-fade2.ar (select.ar (round~ which 2) array)
			  (select.ar (add (trunc which 2) 1) array)
			  (fold2 (minus (mul which 2) 1) 1))))
   (:kr (progn new wrap
	       (lin-x-fade2.kr (select.kr (round~ which 2) array)
			       (select.kr (add (trunc which 2) 1) array)
			       (fold2 (minus (mul which 2) 1) 1))))))

(defugen (select-x-focus "SelectXFocus") (which array &optional (focus 1.0))
  ((:ar (progn new
	       (mix (loop for input in array for i from 0
			  collect (mul (max~ (minus 1 (mul (abs~ (minus which i)) focus)) 0) input)))))
   (:kr (progn new
	       (mix (loop for input in array for i from 0
			  collect (mul (max~ (minus 1 (mul (abs~ (minus which i)) focus)) 0) input)))))))


(defugen (vibrato "Vibrato") (&optional (freq 440.0) (frate 6) (depth 0.02) (delay 0.0) (onset 0.0)
					(rate-variation 0.04) (depth-variation 0.1) (iphase 0.0))
  ((:ar
    (multinew new 'pure-ugen freq frate depth delay onset rate-variation depth-variation iphase))
   (:kr
    (multinew new 'pure-ugen freq frate depth delay onset rate-variation depth-variation iphase))))


(defugen (t-choose "TChoose") (trig array)
  ((:ar (progn new (select.ar (ti-rand.kr 0 (1- (length array)) trig) array)))
   (:kr (progn new (select.kr (ti-rand.kr 0 (1- (length array)) trig) array)))))

(defugen (tw-choose "TWChoose") (trig array weights &optional (normalize 0))
  ((:ar (progn new (select.ar (tw-index.ar trig weights normalize) array)))
   (:kr (progn new (select.kr (tw-index.kr trig weights normalize) array)))))


;;; ...in FSinOsc.sc

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

