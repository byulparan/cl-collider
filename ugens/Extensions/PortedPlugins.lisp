;;
;; https://github.com/madskjeldgaard/portedplugins
;; 

(in-package :sc)


(defugen (analog-bass-drum "AnalogBassDrum")
    (trig &key (infsustain 0.0) (accent 0.5) (freq 50) (tone 0.5) (decay 0.5) (attackfm 0.5) (selffm 0.25))
  ((:ar (multinew new 'ugen trig infsustain accent freq tone decay attackfm selffm))))


(defugen (analog-phaser-mod "AnalogPhaserMod")
    (input &key (skew 0) (modulation 0.5) (stages 8))
  ((:ar (multinew new 'ugen input skew modulation stages))
   (:kr (multinew new 'ugen input skew modulation stages))))


(defugen (analog-phaser "AnalogPhaser")
    (input lfoinput &key (skew 0) (feedback .25) (modulation .5) (stages 8))
  ((:ar (multinew new 'ugen input lfoinput skew feedback modulation stages))))


(defugen (analog-snare-drum "AnalogSnareDrum")
    (trig &key (infsustain 0) (accent .1) (freq 200) (tone .5) (decay .5) (snappy .5))
  ((:ar (multinew new 'ugen trig infsustain accent freq tone decay snappy))))


(defugen (analog-tape "AnalogTape")
    (input &key (bias .5) (saturation .5) (drive .5) (oversample 1) (mode 0))
  ((:ar (multinew new 'ugen input bias saturation drive oversample mode))))


(defugen (analog-vintage-distortion "AnalogVintageDistortion")
    (input &key (drivegain .5) (bias 0) (lowgain .1) (highgain .1) (shelvingfreq 600) (oversample 0))
  ((:ar (multinew new 'ugen input drivegain bias lowgain highgain shelvingfreq oversample))))


;; AnalogChew
;; AnalogLoss
;; AnalogDegrade

(defugen (analog-fold-osc "AnalogFoldOsc")
    (&optional (freq 100) (amp 1))
  ((:ar (multinew new 'ugen freq amp))))


(defugen (bl-osc "BLOsc")
    (&optional (freq 100) (pulsewidth .5) (waveform 0))
  ((:ar (multinew new 'ugen freq pulsewidth waveform))
   (:kr (multinew new 'ugen freq pulsewidth waveform))))


(defugen (chen "Chen")
    (&optional (speed .5) &key  (a .5) (b .3) (c .28))
  ((:ar (multinew new 'multiout-ugen 3 speed a b c))
   (:kr (multinew new 'multiout-ugen 3 speed a b c)))
  :check-fn (lambda (ugen)
	      (dolist (input (inputs ugen))
		(unless (find (rate input) (list :scalar :control))
		  (error "input rate sholud be scalar or control")))))


(defugen (d-compressor "DCompressor")
    (input &optional (sidechain-in 0) &key (sidechain 0) (ratio 4) (threshold -40) (attack .1) (release 100.1) (makeup .5) (automakeup 1))
  ((:ar (multinew new 'ugen input sidechain-in sidechain ratio threshold attack release makeup automakeup))))


(defugen (harmonic-osc "HarmonicOsc")
    (&optional (freq 100) (firstharmonic 1) amplitudes)
  ((:ar (apply #'multinew new 'ugen freq firstharmonic amplitudes))
   (:kr (apply #'multinew new 'ugen freq firstharmonic amplitudes))))


(defugen (lpg "LPG")
    (input controlinput &key (controloffset 0) (controlscale 1) (vca 1) (resonance 1.5) (lowpassmode 1) (linearity 1))
  ((:ar (multinew new 'ugen input controlinput controloffset controlscale vca resonance lowpassmode linearity)))
  :check-fn (lambda (ugen)
	      (let* ((inputs (inputs ugen)))
		(assert (and (eql (rate (first inputs)) :audio)
			     (find (rate (second inputs)) (list :control :audio))
			     (every #'identity (mapcar (lambda (input) (find (rate input) (list :control :scalar))) (cddr inputs))))))))


(defugen (lockhart-wavefolder "LockhartWavefolder")
    (input &optional (num-cells 4))
  ((:ar (multinew new 'ugen input num-cells))
   (:kr (multinew new 'ugen input num-cells))))


(defugen (neo-formant "NeoFormant")
    (&optional (formantfreq 100) (carrierfreq 200) (phaseshift .5))
  ((:ar (multinew new 'ugen formantfreq carrierfreq phaseshift))
   (:kr (multinew new 'ugen formantfreq carrierfreq phaseshift))))


(defugen (neo-var-saw-osc "NeoVarSawOsc")
    (&optional (freq 100) (pw .5) (waveshape .5))
  ((:ar (multinew new 'ugen freq pw waveshape))
   (:kr (multinew new 'ugen freq pw waveshape))))


(defugen (nonlinear-filter "NonlinearFilter")
    (input &optional (freq 500) (q .5) &key (gain 1) (shape 5) (saturation 3))
  ((:ar (multinew new 'ugen input freq q gain shape saturation))
   (:kr (multinew new 'ugen input freq q gain shape saturation))))


(defugen (osc-bank "OscBank")
  (&optional (freq 100) (gain 1) &key (saw8 .5) (square8 .5) (saw4 .5) (square4 .5) (saw2 .5) (square2 .5) (saw1 .5))
  ((:ar (multinew new 'ugen freq gain saw8 square8 saw4 square4 saw2 square2 saw1))
   (:kr (multinew new 'ugen freq gain saw8 square8 saw4 square4 saw2 square2 saw1))))


(defugen (phasor-modal "PhasorModal")
    (input &key (freq 100) (decay .25) (damp 1) (amp .5) (phase .0))
  ((:ar (multinew new 'ugen input freq decay damp amp phase))))


(defugen (resonator "Resonator")
    (input &key (freq 100) (position .001) (resolution 24) (structure .5) (brightness .5) (damping .5))
  ((:ar (multinew new 'ugen input freq position resolution structure brightness damping))))


(defugen (rongs "Rongs")
    (&key (trigger 0) (sustain 1) (f0 .01) (structure .5) (brightness .5) (damping .75) (accent .9) (stretch .5) (position .15) (loss .15) (mode-num 2) (cos-freq .25))
  ((:ar (multinew new 'ugen trigger sustain f0 structure brightness damping accent stretch position loss mode-num cos-freq)))
  :check-fn (lambda (ugen)
	      (destructuring-bind (trigger sustain f0 structure brightness damping accent stretch position loss mode-num cos-freq)
		  (mapcar #'rate (inputs ugen))
		(assert (and (eql mode-num :scalar)
			     (eql cos-freq :scalar)
			     (find trigger (list :control :scalar :audio))
			     (every #'identity (mapcar (lambda (in) (find in (list :control :scalar)))
						       (list sustain f0 structure brightness damping accent stretch position loss))))))))


(defugen (string-voice "StringVoice")
    (&key (trig 0) (infsustain 0) (freq 100) (accent .5) (structure .5) (brightness .5) (damping .5))
  ((:ar (multinew new 'ugen trig infsustain freq accent structure brightness damping))))


(defugen (vadim-filter "VadimFilter")
    (input &optional (freq 500) (resonance 1.0) (type 0))
  ((:ar (multinew new 'ugen input freq resonance type))))


(defugen (var-shape-osc "VarShapeOsc")
  (&optional (freq 100) (pw .5) (waveshape .5) (sync 1) (syncfreq 105))
  ((:ar (multinew new 'ugen freq pw waveshape sync syncfreq))
   (:kr (multinew new 'ugen freq pw waveshape sync syncfreq))))


(defugen (vosim-osc "VosimOsc")
    (&optional (freq 100) (form1freq 951) (form2freq 919) (shape 0))
  ((:ar (multinew new 'ugen freq form1freq form2freq shape))
   (:kr (multinew new 'ugen freq form1freq form2freq shape))))


(defugen (z-osc "ZOsc")
    (&optional (freq 100) (formantfreq 91) (shape .5) (mode .5))
  ((:ar (multinew new 'ugen freq formantfreq shape mode))
   (:kr (multinew new 'ugen freq formantfreq shape mode))))


;; VAOnePoleFilter

(defugen (va-diode-filter "VADiodeFilter")
    (input &optional (freq 500) (res .1) (overdrive 0))
  ((:ar (multinew new 'ugen input freq res overdrive))
   (:kr (multinew new 'ugen input freq res overdrive))))


(defugen (va-korg35 "VAKorg35")
    (input &optional (freq 500) (res .1) (overdrive 0) (type 0))
  ((:ar (multinew new 'ugen input freq res overdrive type))
   (:kr (multinew new 'ugen input freq res overdrive type))))


(defugen (va-ladder "VALadder")
  (input &optional (freq 500) (res .1) (overdrive 0) (type 0))
  ((:ar (multinew new 'ugen input freq res overdrive type))
   (:kr (multinew new 'ugen input freq res overdrive type))))


(defugen (va-sem12 "VASEM12")
    (input &optional (freq 500) (res .1) (transition 0))
  ((:ar (multinew new 'ugen input freq res transition))
   (:kr (multinew new 'ugen input freq res transition))))



