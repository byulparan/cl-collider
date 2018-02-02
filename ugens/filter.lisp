;;; ...in Filter.sc
(in-package #:sc)

(defugen (resonz "Resonz") (&optional (in 0.0) (freq 440.0) (bwr 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq bwr) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq bwr) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (one-pole "OnePole")
    (&optional (in 0.0) (coef 0.5) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in coef) mul add))
   (:kr (madd (multinew new 'pure-ugen in coef) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (one-zero "OneZero")
    (&optional (in 0.0) (coef 0.5) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in coef) mul add))
   (:kr (madd (multinew new 'pure-ugen in coef) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (two-pole "TwoPole")
    (&optional (in 0.0) (freq 440.0) (radius 0.8) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq radius) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq radius) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (two-zero "TwoZero")
    (&optional (in 0.0) (freq 440.0) (radius 0.8) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq radius) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq radius) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (apf "APF")
    (&optional (in 0.0) (freq 440.0) (radius 0.8) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq radius) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq radius) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (integrator "Integrator")
    (&optional (in 0.0) (coef 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in coef) mul add))
   (:kr (madd (multinew new 'pure-ugen in coef) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (Decay "Decay")
    (&optional (in 0.0) (decay-time 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in decay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in decay-time) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (decay2 "Decay2")
    (&optional (in 0.0) (attack-time 0.01) (decay-time 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in attack-time decay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in attack-time decay-time) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (lag "Lag")
    (&optional (in 0.0) (lag-time 0.1) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in lag-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in lag-time) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (lag2 "Lag2")
    (&optional (in 0.0) (lag-time 0.1) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in lag-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in lag-time) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (lag3 "Lag3")
    (&optional (in 0.0) (lag-time 0.1) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in lag-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in lag-time) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (ramp "Ramp")
    (&optional (in 0.0) (lag-time 0.1) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in lag-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in lag-time) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (lag-ud "LagUD")
    (&optional (in 0.0) (lag-time-u 0.1) (lag-time-d 0.1) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in lag-time-u lag-time-d) mul add))
   (:kr (madd (multinew new 'pure-ugen in lag-time-u lag-time-d) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (lag-2ud "Lag2UD")
    (&optional (in 0.0) (lag-time-u 0.1) (lag-time-d 0.1) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in lag-time-u lag-time-d) mul add))
   (:kr (madd (multinew new 'pure-ugen in lag-time-u lag-time-d) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (lag-3ud "Lag3UD")
    (&optional (in 0.0) (lag-time-u 0.1) (lag-time-d 0.1) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in lag-time-u lag-time-d) mul add))
   (:kr (madd (multinew new 'pure-ugen in lag-time-u lag-time-d) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (leak-dc "LeakDC")
    (&optional (in 0.0) (coef 0.995) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in coef) mul add))
   (:kr (madd (multinew new 'pure-ugen in coef) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (rlpf "RLPF")
    (&optional (in 0.0) (freq 440.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (rhpf "RHPF")
    (&optional (in 0.0) (freq 440.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (lpf "LPF")
    (&optional (in 0.0) (freq 440.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (hpf "HPF")
    (&optional (in 0.0) (freq 440.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (bpf "BPF")
    (&optional (in 0.0) (freq 440.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (brf "BRF")
    (&optional (in 0.0) (freq 440.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (mid-eq "MidEQ")
    (&optional (in 0.0) (freq 440.0) (rq 1.0) (db 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq db) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq rq db) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (lpz-1 "LPZ1")
    (&optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in) mul add))
   (:kr (madd (multinew new 'pure-ugen in) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (hpz-1 "HPZ1")
    (&optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in) mul add))
   (:kr (madd (multinew new 'pure-ugen in) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (slope "Slope")
    (&optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in) mul add))
   (:kr (madd (multinew new 'pure-ugen in) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defugen (changed "Changed")
    (&optional (in 0.0) (threshold 0.0))
  ((:ar (progn new (>~ (abs~ (hpz-1.ar in)) threshold)))
   (:kr (progn new (>~ (abs~ (hpz-1.kr in)) threshold))))
  :check-fn #'check-same-rate-as-first-input)


(defugen (lpz-2 "LPZ2")
    (&optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in) mul add))
   (:kr (madd (multinew new 'pure-ugen in) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (hpz-2 "HPZ2")
    (&optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in) mul add))
   (:kr (madd (multinew new 'pure-ugen in) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (bpz-2 "BPZ2")
    (&optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in) mul add))
   (:kr (madd (multinew new 'pure-ugen in) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (brz-2 "BRZ2")
    (&optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in) mul add))
   (:kr (madd (multinew new 'pure-ugen in) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (median "Median")
    (&optional (length 3) (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen length in) mul add))
   (:kr (madd (multinew new 'pure-ugen length in) mul add)))
  :check-fn #'check-when-audio)

(defugen (slew "Slew")
    (&optional (in 0.0) (up 1.0) (dn 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in up dn) mul add))
   (:kr (madd (multinew new 'pure-ugen in up dn) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (fos "FOS")
    (&optional (in 0.0) (a0 0.0) (a1 0.0) (b1 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in a0 a1 b1) mul add))
   (:kr (madd (multinew new 'pure-ugen in a0 a1 b1) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (sos "SOS")
    (&optional (in 0.0) (a0 0.0) (a1 0.0) (a2 0.0) (b1 0.0) (b2 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in a0 a1 a2 b1 b2) mul add))
   (:kr (madd (multinew new 'pure-ugen in a0 a1 a2 b1 b2) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defugen (ringz "Ringz")
    (&optional (in 0.0) (freq 440.0) (decay-time 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq decay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq decay-time) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defugen (formlet "Formlet")
    (&optional (in 0.0) (freq 440.0) (attack 1.0) (decay 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq attack decay) mul add))
   (:kr (madd (multinew new 'pure-ugen in freq attack decay) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defun b-pass-sc (low-hi freq rq)
  (let* ((w0 (*~ 2 pi freq (sample-dur.ir)))
         (cos-w0 (cos~ w0))
         (i (funcall (if (eq :low low-hi) #'-~ #'+~) 1 cos-w0))
         (alpha (*~ (sin~ w0) 0.5 rq))
         (b0rz (/~ 1 (+~ 1 alpha)))
         (a0 (*~ i 0.5 b0rz))
         (a1 (*~ (if (eq :low low-hi) i (*~ -1 i)) b0rz))
         (b1 (*~ cos-w0 2 b0rz))
         (b2 (*~ (-~ 1 alpha) (*~ -1 b0rz))))
    (list a0 a1 a0 b1 b2)))

(defun b-pass4-new (low-hi new in freq rq)
  (declare (ignorable new))
  (let ((coefs (b-pass-sc low-hi freq (sqrt rq))))
    (apply #'multinew #'sos.ar (apply #'sos.ar in coefs) coefs)))

(defugen (b-lowpass "BLowPass")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-lowpass4 "BLowPass4")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew #'b-pass4-new :low new in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-hipass "BHiPass")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-hipass4 "BHiPass4")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew #'b-pass4-new :hi new in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-bandpass "BBandPass")
    (&optional (in 0.0) (freq 1200.0) (bw 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq bw) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-bandstop "BBandStop")
    (&optional (in 0.0) (freq 1200.0) (bw 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq bw) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-allpass "BAllPass")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-peak-eq "BPeakEQ")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (db 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq db) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-lowshelf "BLowShelf")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (db 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq db) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-hishelf "BHiShelf")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (db 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq db) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defugen (detect-silence "DetectSilence")
    (&optional (in 0.0) (amp 0.0001) &key (time 0.1) (act :no-action))
  ((:ar (multinew new 'ugen in amp time (act act)))
   (:kr (multinew new 'ugen in amp time (act act))))
  :check-fn #'check-same-rate-as-first-input)


(defugen (impulse "Impulse")
    (&optional (freq 440.0) (phase 0.0) (mul 1.0) (add 0.0))
  ((:ar
    (madd (multinew new 'pure-ugen freq phase) mul add))
   (:kr
    (madd (multinew new 'pure-ugen freq phase) mul add)))
  :signal-range :unipolar)

(defun var-lag-new (rate new in time curvature warp start)
  (let* ((start (if (not (eql :false start)) start in))
	 (curve (gethash warp +env-shape-table+))
	 (curvature (if curve 0 curvature))
	 (curve (if curve curve warp)))
    (if (/= curve 1)
	(let* ((env-arrays (make-env-array-from-env (env (list start in) (list time) warp))))
	  (loop for e in env-arrays
		do (setf (aref e 6) curve
			 (aref e 7) curvature))
	  (let ((trig (if (eql rate :audio) (+~ (changed.ar in) (impulse.ar 0))
			  (+~ (changed.kr in) (impulse.kr 0)))))
	    (unless (eql (rate time) :scalar)
	      (setf trig (+~ trig (changed.kr time))))
	    (if (eql rate :audio) (env-gen.ar env-arrays :gate trig)
		(env-gen.kr env-arrays :gate trig))))
	(funcall new 'pure-ugen in time start))))

(defugen (var-lag "VarLag")
    (&optional (in 0.0) (time .1) (curvature 0) (warp 5) start (mul 1.0) (add 0.0))
  ((:ar (madd (multinew #'var-lag-new :audio new in time curvature warp (if start start :false)) mul add))
   (:kr (madd (multinew #'var-lag-new :control new in time curvature warp (if start start :false)) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (moog-ff "MoogFF")
    (in &optional (freq 100) (gain 2) (reset 0) (mul 1) (add 0))
  ((:ar
    (madd (multinew new 'pure-ugen in freq gain reset) mul add))
   (:kr
    (madd (multinew new 'pure-ugen in freq gain reset) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (rlpfd "RLPFD")
    (in &optional (ffreq 440) (res 0) (dist 0) (mul 1) (add 0))
  ((:ar
    (madd (multinew new 'pure-ugen in ffreq res dist) mul add))
   (:kr
    (madd (multinew new 'pure-ugen in ffreq res dist) mul add)))
  :check-fn #'check-same-rate-as-first-input)
