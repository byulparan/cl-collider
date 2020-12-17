(in-package #:sc)

(defugen (pitch-shift "PitchShift") (&optional (in 0.0) (window-size 0.2) (pitch-ratio 1.0) (pitch-dispersion 0.0) (time-dispersion 0.0)
				    &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in window-size pitch-ratio pitch-dispersion time-dispersion)
	      mul add))))
