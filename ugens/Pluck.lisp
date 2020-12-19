
(in-package :sc)

(defugen (pluck "Pluck")
    (&optional (in 0.0) (trig 1.0) (max-deltime 0.2) (deltime 0.2) (decaytime 1.0)
	       (coef 0.5) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen in trig max-deltime deltime decaytime coef)
	      mul add))))
