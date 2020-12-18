(in-package #:sc)

(defugen (rlpfd "RLPFD")
    (in &optional (ffreq 440) (res 0) (dist 0) (mul 1) (add 0))
  ((:ar
    (madd (multinew new 'pure-ugen in ffreq res dist) mul add))
   (:kr
    (madd (multinew new 'pure-ugen in ffreq res dist) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (gendy4 "Gendy4")
    (&optional (amp-dist 1) (dur-dist 1) (ad-param 1.0) (dd-param 1.0)
	       (min-freq 440.0) (max-freq 660.0) (amp-scale 0.5) (dur-scale 0.5)
	       (init-cps 12) knum (mul 1.0) (add 1.0))
  ((:ar (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps knum)
	      mul add))
   (:kr (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps knum)
	      mul add))))

(defugen (gendy5 "Gendy5")
    (&optional (amp-dist 1) (dur-dist 1) (ad-param 1.0) (dd-param 1.0)
	       (min-freq 440.0) (max-freq 660.0) (amp-scale 0.5) (dur-scale 0.5)
	       (init-cps 12) knum (mul 1.0) (add 1.0))
  ((:ar (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps knum)
	      mul add))
   (:kr (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps knum)
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
