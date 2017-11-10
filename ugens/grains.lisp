(in-package :sc)

(defugen (grain-sin "GrainSin") (&optional (numchan 1) (trig 0) (dur 1) (freq 440)
					   (pan 0.0) (envbufnum -1) (max-grains 512)
					   &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen numchan trig dur freq pan envbufnum max-grains)
	      mul add))))

(defugen (grain-fm "GrainFM") (&optional (numchan 1) (trig 0) (dur 1) (car-freq 440)
					 (mod-freq 200) (index 1) (pan 0.0) (envbufnum -1) (max-grains 512)
					 &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen numchan trig dur car-freq mod-freq index pan envbufnum max-grains)
	      mul add))))


(defugen (grain-buf "GrainBuf") (&optional (numchan 1) (trig 0) (dur 1) sndbuf (rate 1) (pos 0) (interp 2)
					   (pan 0.0) (envbufnum -1) (max-grains 512)
					   &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen numchan trig dur sndbuf rate pos interp pan envbufnum max-grains)
	      mul add))))



(defugen (grain-in "GrainIn") (&optional (numchan 1) (trig 0) (dur 1) in
					 (pan 0.0) (envbufnum -1) (max-grains 512)
					 &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen numchan trig dur in pan envbufnum max-grains)
	      mul add))))

(defugen (warp1 "Warp1") (&optional (numchan 1) (bufnum 0) (pointer 0) (freq-scale 1) (window-size 0.2)
				    (envbufnum -1) (overlaps 8) (window-rand-ratio 0.0) (interp 1) 
				    &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen numchan bufnum pointer freq-scale window-size envbufnum overlaps
			window-rand-ratio interp) 
	      mul add))))

(defugen (pitch-shift "PitchShift") (&optional (in 0.0) (window-size 0.2) (pitch-ratio 1.0) (pitch-dispersion 0.0) (time-dispersion 0.0)
				    &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in window-size pitch-ratio pitch-dispersion time-dispersion)
	      mul add))))
