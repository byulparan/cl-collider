
(in-package #:sc)

(defugen (rand-seed "RandSeed") (&optional (trig 0.0) (seed 56789))
  ((:ar (progn (multinew new 'width-first-ugen trig seed) 0.0))
   (:kr (progn (multinew new 'width-first-ugen trig seed) 0.0))
   (:ir (progn (multinew new 'width-first-ugen trig seed) 0.0))))

(defugen (rand-id "RandID") (&optional (id 0))
  ((:kr (progn (multinew new 'width-first-ugen id) 0.0))
   (:ir (progn (multinew new 'width-first-ugen id) 0.0))))

(defugen (rand "Rand")
    (&optional (lo 0.0) (hi 1.0))
  ((:ir (multinew new  'ugen lo hi))))

(defugen (i-rand "IRand")
    (&optional (lo 0) (hi 127))
  ((:ir (multinew new  'ugen lo hi))))

(defugen (t-rand "TRand")
    (&optional (lo 0.0) (hi 1.0) (trig 0.0))
  ((:ar (multinew new  'ugen lo hi trig))
   (:kr (multinew new  'ugen lo hi trig))))

(defugen (ti-rand "TIRand")
    (&optional (lo 0.0) (hi 127) (trig 0.0))
  ((:kr (multinew new  'ugen lo hi trig))
   (:ar (multinew new  'ugen lo hi trig))))

(defugen (lin-rand "LinRand")
    (&optional (lo 0.0) (hi 1.0) (minmax 0.0))
  ((:ir (multinew new  'ugen lo hi minmax))))

(defugen (n-rand "NRand")
    (&optional (lo 0.0) (hi 1.0) (n 0.0))
  ((:ir (multinew new  'ugen lo hi n))))

(defugen (exp-rand "ExpRand")
    (&optional (lo 0.01) (hi 1.0))
  ((:ir (multinew new  'ugen lo hi))))

(defugen (t-exp-rand "TExpRand")
    (&optional (lo 0.01) (hi 1.0) (trig 0.0))
  ((:ar (multinew new  'ugen lo hi trig))
   (:kr (multinew new  'ugen lo hi trig))))

(defugen (coin-gate "CoinGate")
    (prob in)
  ((:ar (multinew new  'ugen prob in))
   (:kr (multinew new  'ugen prob in))))

(defugen (tw-index "TWindex")
    (in array &optional (normalize 0.0))
  ((:ar (apply 'multinew new 'ugen (cons in (cons normalize array))))
   (:kr (apply 'multinew new 'ugen (cons in (cons normalize array))))))

(defugen (white-noise "WhiteNoise")
    (&optional (mul 1.0) (add 0.0))
  ((:ar
    (if (listp mul) (madd
		     (dup (lambda (i) (declare (ignore i)) (multinew new 'ugen)) (length mul)) mul add)
	(madd (multinew new 'ugen) mul add)))
   (:kr
    (if (listp mul) (madd (dup (lambda (i) (declare (ignore i)) (multinew new 'ugen)) (length mul)) mul add)
	(madd (multinew new 'ugen) mul add)))))

(defugen (brown-noise "BrownNoise")
    (&optional (mul 1.0) (add 0.0))
  ((:ar
    (if (listp mul) (madd (dup (lambda (i) (declare (ignore i)) (multinew new 'ugen)) (length mul)) mul add)
	(madd (multinew new 'ugen) mul add)))
   (:kr
    (if (listp mul) (madd (dup (lambda (i) (declare (ignore i)) (multinew new 'ugen)) (length mul)) mul add)
	(madd (multinew new 'ugen) mul add)))))

(defugen (pink-noise "PinkNoise")
    (&optional (mul 1.0) (add 0.0))
  ((:ar
    (if (listp mul) (madd (dup (lambda (i) (declare (ignore i)) (multinew new 'ugen)) (length mul)) mul add)
	(madd (multinew new 'ugen) mul add)))
   (:kr
    (if (listp mul) (madd (dup (lambda (i) (declare (ignore i)) (multinew new 'ugen)) (length mul)) mul add)
	(madd (multinew new 'ugen) mul add)))))

(defugen (clip-noise "ClipNoise")
    (&optional (mul 1.0) (add 0.0))
  ((:ar
    (if (listp mul) (madd (dup (lambda (i) (declare (ignore i)) (multinew new 'ugen)) (length mul)) mul add)
	(madd (multinew new 'ugen) mul add)))
   (:kr
    (if (listp mul) (madd (dup (lambda (i) (declare (ignore i)) (multinew new 'ugen)) (length mul)) mul add)
	(madd (multinew new 'ugen) mul add)))))

(defugen (gray-noise "GrayNoise")
    (&optional (mul 1.0) (add 0.0))
  ((:ar
    (if (listp mul) (madd (dup (lambda (i) (declare (ignore i)) (multinew new 'ugen)) (length mul)) mul add)
	(madd (multinew new 'ugen) mul add)))
   (:kr
    (if (listp mul) (madd (dup (lambda (i) (declare (ignore i)) (multinew new 'ugen)) (length mul)) mul add)
	(madd (multinew new 'ugen) mul add)))))

(defugen (crackle "Crackle")
    (&optional (chaos-param 1.5) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen chaos-param) mul add))
   (:kr
    (madd (multinew new 'ugen chaos-param) mul add))))

(defugen (logistic "Logistic")
    (&optional (chaos-param 3.0) (freq 1000.0) (init 0.5) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen chaos-param freq init) mul add))
   (:kr
    (madd (multinew new 'ugen chaos-param freq init) mul add))))

(defugen (lf-noise0 "LFNoise0")
    (&optional (freq 500.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq) mul add))
   (:kr (madd (multinew new 'ugen freq) mul add))))

(defugen (lf-noise1 "LFNoise1")
    (&optional (freq 500.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq) mul add))
   (:kr (madd (multinew new 'ugen freq) mul add))))

(defugen (lf-noise2 "LFNoise2")
    (&optional (freq 500.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq) mul add))
   (:kr (madd (multinew new 'ugen freq) mul add))))

(defugen (lf-clip-noise "LFClipNoise")
    (&optional (freq 500.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq) mul add))
   (:kr (madd (multinew new 'ugen freq) mul add))))

(defugen (lfd-noise0 "LFDNoise0")
    (&optional (freq 500.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq) mul add))
   (:kr (madd (multinew new 'ugen freq) mul add))))

(defugen (lfd-noise1 "LFDNoise1")
    (&optional (freq 500.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq) mul add))
   (:kr (madd (multinew new 'ugen freq) mul add))))

(defugen (lfd-noise3 "LFDNoise3")
    (&optional (freq 500.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq) mul add))
   (:kr (madd (multinew new 'ugen freq) mul add))))

(defugen (lfd-clip-noise "LFDClipNoise")
    (&optional (freq 500.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq) mul add))
   (:kr (madd (multinew new 'ugen freq) mul add))))

(defugen (hasher "Hasher")
    (&optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen in) mul add))
   (:kr (madd (multinew new 'ugen in) mul add))))

(defugen (mantissa-mask "MantissaMask")
    (&optional (in 0.0) (bits 3.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen in bits) mul add))
   (:kr (madd (multinew new 'ugen in bits) mul add))))

(defugen (dust "Dust") (&optional (density 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen density) mul add))
   (:kr (madd (multinew new 'ugen density) mul add)))
  :signal-range :unipolar)

(defugen (dust2 "Dust2")
    (&optional (density 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen density) mul add))
   (:kr (madd (multinew new 'ugen density) mul add))))
