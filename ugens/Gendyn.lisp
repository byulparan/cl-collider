(in-package #:sc)

(defugen (gendy1 "Gendy1")
    (&optional (amp-dist 1) (dur-dist 1) (ad-param 1.0) (dd-param 1.0)
	       (min-freq 440.0) (max-freq 660.0) (amp-scale 0.5) (dur-scale 0.5)
	       (init-cps 12) knum (mul 1.0) (add 1.0))
  ((:ar (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps (if knum knum init-cps))
	      mul add))
   (:kr (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps (if knum knum init-cps))
	      mul add))))

(defugen (gendy2 "Gendy2")
    (&optional (amp-dist 1) (dur-dist 1) (ad-param 1.0) (dd-param 1.0)
	       (min-freq 440.0) (max-freq 660.0) (amp-scale 0.5) (dur-scale 0.5)
	       (init-cps 12) knum (a 1.17) (c 0.31) (mul 1.0) (add 1.0))
  ((:ar (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps (if knum knum init-cps) a c)
	      mul add))
   (:kr (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param min-freq
			max-freq amp-scale dur-scale init-cps (if knum knum init-cps) a c)
	      mul add))))

(defugen (gendy3 "Gendy3")
    (&optional (amp-dist 1) (dur-dist 1) (ad-param 1.0) (dd-param 1.0)
	       (freq 440.0) (amp-scale 0.5) (dur-scale 0.5)
	       (init-cps 12) knum (mul 1.0) (add 1.0))
  ((:ar (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param freq
			amp-scale dur-scale init-cps (if knum knum init-cps))
	      mul add))
   (:kr (madd (multinew new 'ugen amp-dist dur-dist ad-param dd-param freq
			amp-scale dur-scale init-cps (if knum knum init-cps))
	      mul add))))

