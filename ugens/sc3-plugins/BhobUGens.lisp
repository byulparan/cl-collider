(in-package #:sc)

(defugen (nested-allpass-n "NestedAllpassN")
    (in &optional (maxdelay1 0.036) (delay1 0.036) (gain1 0.08) (maxdelay2 0.03) (delay2 0.03) (gain2 0.3) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in maxdelay1 delay1 gain1 maxdelay2 delay2 gain2) mul add))))

(defugen (nested-allpass-l "NestedAllpassL")
    (in &optional (maxdelay1 0.036) (delay1 0.036) (gain1 0.08) (maxdelay2 0.03) (delay2 0.03) (gain2 0.3) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in maxdelay1 delay1 gain1 maxdelay2 delay2 gain2) mul add))))

(defugen (nested-allpass-c "NestedAllpassC")
    (in &optional (maxdelay1 0.036) (delay1 0.036) (gain1 0.08) (maxdelay2 0.03) (delay2 0.03) (gain2 0.3) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in maxdelay1 delay1 gain1 maxdelay2 delay2 gain2) mul add))))

(defugen (double-nested-allpass-n "DoubleNestedAllpassN")
    (in &optional (maxdelay1 0.0047) (delay1 0.0047) (gain1 0.15) (maxdelay2 0.022) (delay2 0.022) (gain2 0.25)
	(maxdelay3 0.0083) (delay3 0.0083) (gain3 0.3) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3) mul add))))

(defugen (double-nested-allpass-l "DoubleNestedAllpassL")
    (in &optional (maxdelay1 0.0047) (delay1 0.0047) (gain1 0.15) (maxdelay2 0.022) (delay2 0.022) (gain2 0.25)
	(maxdelay3 0.0083) (delay3 0.0083) (gain3 0.3) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3) mul add))))

(defugen (double-nested-allpass-c "DoubleNestedAllpassC")
    (in &optional (maxdelay1 0.0047) (delay1 0.0047) (gain1 0.15) (maxdelay2 0.022) (delay2 0.022) (gain2 0.25)
	(maxdelay3 0.0083) (delay3 0.0083) (gain3 0.3) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in maxdelay1 delay1 gain1 maxdelay2 delay2 gain2 maxdelay3 delay3 gain3) mul add))))


(defugen (moog-ladder "MoogLadder")
    (in &optional (ffreq 440.0) (res 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in ffreq res) mul add))
   (:kr
    (madd (multinew new 'ugen in ffreq res) mul add))))


(defugen (rlpfd "RLPFD")
    (in &optional (ffreq 440.0) (res 0.0) (dist 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in ffreq res dist) mul add))
   (:kr
    (madd (multinew new 'ugen in ffreq res dist) mul add))))

(defugen (streson "Streson")
    (input &optional (delay-time 0.003) (res 0.9) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen input delay-time res) mul add))
   (:kr
    (madd (multinew new 'ugen input delay-time res) mul add))))

(defugen (nlfilt-n "NLFiltN")
    (input a b d c l &optional (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen input a b d c l) mul add))
   (:kr
    (madd (multinew new 'ugen input a b d c l) mul add))))

(defugen (nlfilt-l "NLFiltL")
    (input a b d c l &optional (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen input a b d c l) mul add))
   (:kr
    (madd (multinew new 'ugen input a b d c l) mul add))))

(defugen (nlfilt-c "NLFiltC")
    (input a b d c l &optional (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen input a b d c l) mul add))
   (:kr
    (madd (multinew new 'ugen input a b d c l) mul add))))

(defugen (gauss-trig "GaussTrig")
    (&optional (freq 440.0) (dev 0.3) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen freq dev) mul add))
   (:kr
    (madd (multinew new 'ugen freq dev) mul add)))
  :signal-range :unipolar)

(defugen (lf-brown-noise0 "LFBrownNoise0")
    (&optional (freq 20) (dev 1.0) (dist 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen freq dev dist) mul add))
   (:kr
    (madd (multinew new 'ugen freq dev dist) mul add))))

(defugen (lf-brown-noise1 "LFBrownNoise1")
    (&optional (freq 20) (dev 1.0) (dist 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen freq dev dist) mul add))
   (:kr
    (madd (multinew new 'ugen freq dev dist) mul add))))

(defugen (lf-brown-noise2 "LFBrownNoise2")
    (&optional (freq 20) (dev 1.0) (dist 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen freq dev dist) mul add))
   (:kr
    (madd (multinew new 'ugen freq dev dist) mul add))))

(defugen (t-brown-rand "TBrownRand")
    (&optional (lo 0.0) (hi 1.0) (dev 1.0) (dist 0.0) (trig 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen lo hi dev dist trig) mul add))
   (:kr
    (madd (multinew new 'ugen lo hi dev dist trig) mul add))))

(def-dugen (d-brown2 "Dbrown2") (lo hi step dist &optional (length +inf+))
  (multinew new 'dugen length lo hi step dist))

(def-dugen (d-gauss "Dgauss") (lo hi &optional (length +inf+))
  (multinew new 'dugen length lo hi))

(defugen (t-gauss-rand "TGaussRand")
    (&optional (lo 0.0) (hi 1.0) (trig 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen lo hi trig) mul add))
   (:kr
    (madd (multinew new 'ugen lo hi trig) mul add))))

(defugen (t-beta-rand "TBetaRand")
    (&optional (lo 0.0) (hi 1.0) (prob1 0.0) (prob2 0.0) (trig 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen lo hi prob1 prob2 trig) mul add))
   (:kr
    (madd (multinew new 'ugen lo hi prob1 prob2 trig) mul add))))


(defugen (gendy4 "Gendy4")
    (&optional (amp-dist 1) (dur-dist 1) (ad-param 1.0) (dd-param 1.0)
	       (min-freq 440.0) (max-freq 660.0) (amp-scale 0.5) (dur-scale 0.5)
	       (init-cps 12) knum (mul 1.0) (add 1.0))
  ((:ar (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps (if knum knum init-cps))
	      mul add))
   (:kr (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps (if knum knum init-cps))
	      mul add))))

(defugen (gendy5 "Gendy5")
    (&optional (amp-dist 1) (dur-dist 1) (ad-param 1.0) (dd-param 1.0)
	       (min-freq 440.0) (max-freq 660.0) (amp-scale 0.5) (dur-scale 0.5)
	       (init-cps 12) knum (mul 1.0) (add 1.0))
  ((:ar (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps (if knum knum init-cps))
	      mul add))
   (:kr (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps (if knum knum init-cps))
	      mul add))))

(defugen (tgrains2 "TGrains2")
    (chanls &optional (trigger 0) (bufnum 0) (rate 1) (center-pos 0)
	    (dur .1) (pan 0) (amp .1) (attk .5) (dec .5) (interp 4))
  ((:ar (progn (when (< chanls 2) (error "TGrains needs at least two channels."))
	       (multinew new 'multiout-ugen chanls trigger bufnum rate center-pos dur pan amp
			 attk dec interp)))))

(defugen (tgrains3 "TGrains3")
    (chanls &optional (trigger 0) (bufnum 0) (rate 1) (center-pos 0)
	    (dur .1) (pan 0.0) (amp .1) (attk .5) (dec .5) (window 1) (interp 4))
  ((:ar (progn (when (< chanls 2) (error "TGrains needs at least two channels."))
	       (multinew new 'multiout-ugen chanls trigger bufnum rate center-pos dur pan amp
			 attk dec window interp)))))
