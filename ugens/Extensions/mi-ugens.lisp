(in-package :sc)


(defugen (mi-rings "MiRings")
    (&optional (in .0) (trig .0) (pitch 60.0) (struct .25) (bright .5)
	       &key (damp .7) (pos .25) (model .0) (poly 1.0) (intern-exciter .0) (easteregg .0) (bypass .0) (mul 1.0) (add .0))
  ((:ar (madd (multinew new 'multiout-ugen 2 in trig pitch struct bright damp pos model poly
			intern-exciter easteregg bypass) mul add))))



(defugen (mi-plaits "MiPlaits")
    (&optional (pitch 60) (engine 0) (harm .1) (timbre .5) (morph .5) (trig .0) &key (level .0) (fm-mod .0) (timb-mod .0) (morph-mod .0) (decay .5) (lpg-colour .5) (mul 1.0))
  ((:ar (madd (multinew new 'multiout-ugen 2 pitch engine harm timbre morph trig level fm-mod timb-mod morph-mod decay lpg-colour) mul))))



(defugen (mi-clouds "MiClouds")
    (input-array &optional (pitch .0) (pos .5) (size .25) (dens .4) (tex .5)
		 &key (drywet .5) (in-gain 1.0) (spread .5) (rvb .0) (fb .0) (freeze .0) (mode .0) (lofi .0) (trig .0) (mul 1.0) (add .0))
  ((:ar (madd (apply #'multinew new 'multiout-ugen 2 pitch pos size dens tex drywet in-gain spread rvb fb freeze mode lofi trig (alexandria:ensure-list input-array)) mul add))) 
  :check-fn (lambda (ugen)
	      (assert (eql :audio (rate (nth 14 (inputs ugen)))) nil "input-array is note audio rate: ~a ~a" (nth 14 (inputs ugen))  (rate (nth 14 (inputs ugen))) )))



(defugen (mi-braids "MiBraids")
    (&optional (pitch 60) (timbre .5) (color .5) (model .0) (trig .0) &key (resamp .0)
	       (decim .0) (bits .0) (ws .0) (mul 1.0))
  ((:ar
    (madd (multinew new 'ugen pitch timbre color model trig resamp decim bits ws) mul))))



(defugen (mi-elements "MiElements")
    (&optional (blow-in 0) (strike-in 0) (gate 0) (pitch 48) (strength .5) (contour .2)
	       &key (bow-level 0) (blow-level 0) (strike-level 0) (flow .5) (mallet .5)
	       (bow-timb .5) (blow-timb .5) (strike-timb .5) (geom .25) (bright .5) (damp .7)
	       (pos .2) (space .3) (model 0) (easteregg 0) (mul 1.0) (add .0))
  ((:ar (madd (multinew new 'multiout-ugen 2 blow-in strike-in gate pitch strength contour
			bow-level blow-level strike-level flow mallet bow-timb blow-timb strike-timb geom bright damp pos space model easteregg) mul add))))



(defugen (mi-grids "MiGrids")
    (&key (on-off 1) (bpm 120) (map-x .5) (map-y .5) (chaos .0) (bd-dens .25) (sd-dens .25) (hh-dens .25) (clock-trig .0) (reset-trig .0) (ext-clock .0) (mode .0) (swing .0) (config .0) (reso 2))
  ((:ar (multinew new 'multiout-ugen 8 on-off bpm map-x map-y chaos bd-dens sd-dens hh-dens clock-trig reset-trig ext-clock mode swing config reso))))



(defugen (mi-mu "MiMu")
    (in &optional (gain 1.0) (bypass .0) (mul 1.0) (add .0))
  ((:ar (madd (multinew new 'ugen in gain bypass) mul add))) 
  :check-fn #'check-same-rate-as-first-input)



(defugen (mi-omi "MiOmi")
    (audio-in &optional (gate .0) (pit 48) &key (contour .2) (detune .25) (level1 .5) (level2 .5) (ratio1 .5) (ratio2 .5) (fm1 .0) (fm2 .0) (fb .0) (xfb .0) (filter-mode .0) (cutoff .5) (reson .0) (strength .5) (env .5) (rotate .2) (space .5) (mul 1.0) (add .0))
  ((:ar (madd (multinew new 'multiout-ugen 2 audio-in gate pit contour detune level1 level2 ratio1 ratio2 fm1 fm2 fb xfb filter-mode cutoff reson strength env rotate space) mul add))))



(defugen (mi-ripples "MiRipples")
    (in &optional (cf .3) (reson .2) (drive 1.) (mul 1.0) (add .0))
  ((:ar (madd (multinew new 'ugen in cf reson drive) mul add)))
  :check-fn #'check-same-rate-as-first-input)



(defugen (mi-tides "MiTides")
    (&key (freq 1) (shape .5) (slope .5) (smooth .5) (shift .2) (trig .0) (clock .0) (output-mode 3) (ramp-mode 1) (ratio 9) (rate 1) (mul 1.0) (add .0))
  ((:ar (madd (multinew new 'multiout-ugen 4 freq shape slope smooth shift trig clock output-mode ramp-mode ratio rate) mul add))))



(defugen (mi-verb "MiVerb")
    (input-array &key (time .7) (drywet .5) (damp .5) (hp .05) (freeze .0) (diff .625) (mul 1.0) (add .0))
  ((:ar (madd (apply #'multinew new 'multiout-ugen 2 time drywet damp hp freeze diff (alexandria:ensure-list input-array)) mul add)))
  :check-fn (lambda (ugen)
	      (let* ((inputs (inputs ugen)))
		(dotimes (i (- (length inputs) 6))
		  (unless (eql :audio (rate (nth (+ i 6) inputs)))
		    (error "input is not audio rate: ~a ~a" (nth (+ i 6) inputs) (rate (nth (+ i 6) inputs))))))))



(defugen (mi-warps "MiWarps")
    (&optional (carrier .0) (modulator .0) &key (lev1 .5) (lev2 .5) (algo .0) (timb .0) (osc .0) (freq 110.0) (vgain 1.0) (easteregg .0))
  ((:ar (multinew new 'multiout-ugen 2 carrier modulator lev1 lev2 algo timb osc freq vgain easteregg))))
