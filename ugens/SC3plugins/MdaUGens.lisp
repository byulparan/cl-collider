(in-package #:sc)

(defugen (mda-piano "MdaPiano")
    (&optional (freq 440.0) &key (gate 1.0) (vel 100) (decay 0.8) (release 0.8)
	       (hard 0.8) (velhard 0.8) (muffle 0.8) (velmuff 0.8)
	       (velcurve 0.8) (stereo 0.2) (tune 0.5) (random 0.1)
	       (stretch 0.1) (sustain 0) (mul 1) (add 0))
  ((:ar
    (madd
     (multinew new 'multiout-ugen 2 freq gate vel decay release hard velhard muffle velmuff
	       velcurve stereo tune random stretch sustain)
     mul add))))
