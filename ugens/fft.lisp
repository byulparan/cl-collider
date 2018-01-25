
(in-package #:sc)

(defmacro def-pv-chain-ugen (name args &body body)
  `(progn (defun ,(car name) ,args
	    (let ((new (lambda (cls &rest inputs) (apply #'ugen-new ,(second name) :control cls #'identity :bipolar
							 inputs))))
	      ,@body))
	  (export ',(car name) :sc)))


(defun fft (buffer &optional (in 0.0) (hop 0.5) (wintype 0) (active 1)
	    (winsize 0))
  (multinew (lambda (cls &rest inputs) (apply #'ugen-new "FFT" :control cls #'identity :bipolar inputs))
	    'pv-chain-ugen buffer in hop wintype active winsize))

(defugen (ifft "IFFT")
    (buffer &optional (wintype 0) (winsize 0))
  ((:ar
    (multinew new 'width-first-ugen buffer wintype winsize))
   (:kr
    (multinew new 'width-first-ugen buffer wintype winsize))))

(def-pv-chain-ugen (pv-mag-above "PV_MagAbove") (buffer &optional (threshold 0.0))
  (multinew new 'pv-chain-ugen buffer threshold))

(def-pv-chain-ugen (pv-mag-below "PV_MagBelow") (buffer &optional (threshold 0.0))
  (multinew new 'pv-chain-ugen buffer threshold))

(def-pv-chain-ugen (pv-mag-clip "PV_MagClip")
    (buffer &optional (threshold 0.0))
  (multinew new 'pv-chain-ugen buffer threshold))

(def-pv-chain-ugen (pv-local-max "PV_LocalMax")
    (buffer &optional (threshold 0.0))
  (multinew new 'pv-chain-ugen buffer threshold))


(def-pv-chain-ugen (pv-mag-smear "PV_MagSmear")
    (buffer &optional (bins 0.0))
  (multinew new 'pv-chain-ugen buffer bins))

(def-pv-chain-ugen (pv-bin-shift "PV_BinShift")
    (buffer &optional (stretch 1.0) (shift 0.0))
  (multinew new 'pv-chain-ugen buffer stretch shift))

(def-pv-chain-ugen (pv-mag-shift "PV_MagShift")
    (buffer &optional (stretch 1.0) (shift 0.0))
  (multinew new 'pv-chain-ugen buffer stretch shift))

(def-pv-chain-ugen (pv-mag-squared "PV_MagSquared")
    (buffer)
  (multinew new 'pv-chain-ugen buffer))

(def-pv-chain-ugen (pv-mag-noise "PV_MagNoise")
    (buffer)
  (multinew new 'pv-chain-ugen buffer))

(def-pv-chain-ugen (pv-phase-shift90 "PV_PhaseShift90")
    (buffer)
  (multinew new 'pv-chain-ugen buffer))

(def-pv-chain-ugen (pv-phase-shift270 "PV_PhaseShift270")
    (buffer)
  (multinew new 'pv-chain-ugen buffer))

(def-pv-chain-ugen (pv-conj "PV_Conj")
    (buffer)
  (multinew new 'pv-chain-ugen buffer))

(def-pv-chain-ugen (pv-phase-shift "PV_PhaseShift")
    (buffer shift)
  (multinew new 'pv-chain-ugen buffer shift))

(def-pv-chain-ugen (pv-brick-wall "PV_BrickWall")
    (buffer &optional (wipe 0.0))
  (multinew new 'pv-chain-ugen buffer wipe))

(def-pv-chain-ugen (pv-bin-wipe "PV_BinWipe")
    (buffer-a buffer-b &optional (wipe 0.0))
  (multinew new 'pv-chain-ugen buffer-a buffer-b wipe))

(def-pv-chain-ugen (pv-mag-mul "PV_MagMul")
    (buffer-a buffer-b)
  (multinew new 'pv-chain-ugen buffer-a buffer-b))

(def-pv-chain-ugen (pv-copy-phase "PV_CopyPhase")
    (buffer-a buffer-b)
  (multinew new 'pv-chain-ugen buffer-a buffer-b))

(def-pv-chain-ugen (pv-copy "PV_Copy")
    (buffer-a buffer-b)
  (multinew new 'pv-chain-ugen buffer-a buffer-b))

(def-pv-chain-ugen (pv-max "PV_Max")
    (buffer-a buffer-b)
  (multinew new 'pv-chain-ugen buffer-a buffer-b))

(def-pv-chain-ugen (pv-min "PV_Min")
    (buffer-a buffer-b)
  (multinew new 'pv-chain-ugen buffer-a buffer-b))

(def-pv-chain-ugen (pv-mul "PV_Mul")
    (buffer-a buffer-b)
  (multinew new 'pv-chain-ugen buffer-a buffer-b))

(def-pv-chain-ugen (pv-div "PV_Div")
    (buffer-a buffer-b)
  (multinew new 'pv-chain-ugen buffer-a buffer-b))

(def-pv-chain-ugen (pv-add "PV_Add")
    (buffer-a buffer-b)
  (multinew new 'pv-chain-ugen buffer-a buffer-b))

(def-pv-chain-ugen (pv-mag-div "PV_MagDiv")
    (buffer-a buffer-b &optional (zeroed 0.0001))
  (multinew new 'pv-chain-ugen buffer-a buffer-b zeroed))

(def-pv-chain-ugen (pv-rand-comb "PV_RandComb")
    (buffer &optional (wipe 0.0) (trig 0.0))
  (multinew new 'pv-chain-ugen buffer wipe trig))

(def-pv-chain-ugen (pv-rect-comb "PV_RectComb")
    (buffer &optional (num-teeth 0.0) (phase 0.0) (width 0.5))
  (multinew new 'pv-chain-ugen buffer num-teeth phase width))

(def-pv-chain-ugen (pv-rect-comb2 "PV_RectComb2")
    (buffer-a buffer-b &optional (num-teeth 0.0) (phase 0.0) (width 0.5))
  (multinew new 'pv-chain-ugen buffer-a buffer-b num-teeth phase width))

(def-pv-chain-ugen (pv-rand-wipe "PV_RandWipe")
    (buffer-a buffer-b &optional (wipe 0.0) (trig 0.0))
  (multinew new 'pv-chain-ugen buffer-a buffer-b wipe trig))

(def-pv-chain-ugen (pv-diffuser "PV_Diffuser")
    (buffer &optional (trig 0.0))
  (multinew new 'pv-chain-ugen buffer trig))

(def-pv-chain-ugen (pv-mag-freeze "PV_MagFreeze")
    (buffer &optional (freeze 0.0))
  (multinew new 'pv-chain-ugen buffer freeze))

(def-pv-chain-ugen (pv-bin-scramble "PV_BinScramble")
    (buffer &optional (wipe 0.0) (width 0.2) (trig 0.0))
  (multinew new 'pv-chain-ugen buffer wipe width trig))

(def-pv-chain-ugen (fft-trigger "FFTTrigger")
    (buffer &optional (hop 0.5) (polar 0.0))
  (multinew new 'pv-chain-ugen buffer hop polar))

(def-pv-chain-ugen (pv-conformal-map "PV_ConformalMap")
    (buffer &optional (areal 0.0) (aimag 0.0))
  (multinew new 'pv-chain-ugen buffer areal aimag))



(defugen (convolution "Convolution")
    (in kernel &optional (framesize 512) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in kernel framesize) mul add))))

(defugen (convolution2 "Convolution2")
    (in kernel &optional (trigger 0) (framesize 2048) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in kernel trigger framesize) mul add))))

(defugen (convolution2-l "Convolution2L")
    (in kernel &optional (trigger 0) (framesize 2048) (crossfade 1) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in kernel trigger framesize crossfade) mul add))))



(defugen (stereo-convolution2-l "StereoConvolution2L")
    (in kernel-l kernel-r &optional (trigger 0) (framesize 2048) (crossfade 1) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'multiout-ugen 2 in kernel-l kernel-r trigger framesize crossfade) mul add))))

(defugen (convolution3 "Convolution3")
    (in kernel &optional (trigger 0) (framesize 2048) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'ugen in kernel trigger framesize) mul add))
   (:kr
    (madd (multinew new 'ugen in kernel trigger framesize) mul add))))

(defugen (pv-jensen-andersen "PV_JensenAndersen")
    (buffer &optional (propsc .25) (prophfe .25) (prophfc .25) (propsf .25)
	    (threshold 1.0) (waittime .04))
  ((:ar
    (multinew new  'pv-chain-ugen buffer propsc prophfe prophfc propsf threshold waittime))))

(defugen (pv-hainsworth-foote "PV_HainsworthFoote")
    (buffer &optional (proph 0.0) (propf 0.0) (threshold 1.0) (waittime 0.04))
  ((:ar
    (multinew new  'pv-chain-ugen buffer proph propf threshold waittime))))


(defugen (running-sum "RunningSum")
    (in &optional (num-samp 40.0))
  ((:ar (multinew new 'ugen in num-samp))
   (:kr (multinew new 'ugen in num-samp))))

(defun running-sum-rms (in &optional (num-samp 40.0))
  (sqrt
   (mul (running-sum.ar (squared in) num-samp)
	(reciprocal num-samp))))
