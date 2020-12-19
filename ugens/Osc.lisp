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


(defugen (impulse "Impulse")
    (&optional (freq 440.0) (phase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen freq phase) mul add))
   (:kr
    (madd (multinew new 'pure-ugen freq phase) mul add)))
  :signal-range :unipolar)


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



;; 
;; from SCClassLibrary/backwards_compatibility/PMOsc.sc
;; 
(defun pm-osc.ar (car-freq mod-freq &optional (pm-index 0.0) (mod-phase 0.0) (mul 1.0) (add 0.0))
  (sin-osc.ar car-freq (sin-osc.ar mod-freq mod-phase pm-index) mul add))

(defun pm-osc.kr (car-freq mod-freq &optional (pm-index 0.0) (mod-phase 0.0) (mul 1.0) (add 0.0))
  (sin-osc.kr car-freq (sin-osc.kr mod-freq mod-phase pm-index) mul add))

(export 'pm-osc.ar)
(export 'pm-osc.kr)
