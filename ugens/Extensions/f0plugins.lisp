;; 
;; https://github.com/redFrik/f0plugins
;; 

(in-package :sc)

;; AY8910
(defugen (ay8910 "AY8910")
    (&key (r0 0) (r1 0) (r2 0) (r3 0) (r4 0) (r5 0) (r6 0) (r7 0) (r8 0) (r9 0) (ra 0) (rb 0) (rc 0) (rd 0) (rate 1))
  ((:ar (multinew new 'multiout-ugen 3 r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 ra rb rc rd rate))))


(defun ay8910-square.ar (&optional (freq 100) (noise 15) (ctrl 0) (vol 15) (env-rate 200) (env-shape 14) (rate 1))
  (let* ((r0 (mod~ freq 256))
	 (r1 (clip.kr (/~ freq 256) 0 15))
	 (r6 (clip.kr noise 0 31))
	 (control (clip.kr ctrl 0 7))
	 (enable0 (>~ (mod~ control 2) 0))
	 (enable1 (>~ (mod~ (floor~ (/~ control 2)) 2) 0))
	 (enable2 (>~ (mod~ (floor~ (/~ control 4)) 4) 0))
	 (r7 (+~ enable0 (*~ enable1 8)))
	 (r8 (+~ (clip.kr vol 0 15) (*~ enable2 16)))
	 (rb (mod~ env-rate 256))
	 (rc (clip.kr (/~ env-rate 256) 0 255))
	 (rd (clip.kr env-shape 0 15)))
    (ay8910.ar :r0 r0 :r1 r1 :r2 0 :r3 0 :r4 0 :r5 0 :r6 r6
		:r7 r7 :r8 r8 :r9 0 :ra 0 :rb rb :rc rc :rd rd :rate rate)))

(export 'ay8910-square.ar)


;; Astrocade
(defugen (astrocade "Astrocade")
  (&key (reg0 0) (reg1 127) (reg2 0) (reg3 0) (reg4 0) (reg5 0) (reg6 15) (reg7 0))
  ((:ar (multinew new 'ugen reg0 reg1 reg2 reg3 reg4 reg5 reg6 reg7))))


;; Atari2600
(defugen (atari2600 "Atari2600")
  (&optional (audc0 1) (audc1 2) (audf0 3) (audf1 4) (audv0 5) (audv1 5) (rate 1))
  ((:ar (multinew new 'ugen audc0 audc1 audf0 audf1 audv0 audv1 rate))))

;; Beep
(defugen (beep "BeepU") (&optional (freq 3250) (vol 1))
  ((:ar (multinew new 'ugen freq vol))))


;; Dbjorklund
(def-dugen (d-bjorklund "Dbjorklund") (&optional (k 4) (n 8) (offset 0) (length +inf+))
  (multinew new 'dugen length k n offset))

(def-dugen (d-bjorklund2 "Dbjorklund2") (&optional (k 4) (n 8) (offset 0) (length +inf+))
  (progn
    (when (and (numberp k) (<= k 0))
      (error "k must be >= 1"))
    (multinew new 'dugen length k n offset)))



;; Dsieve


;; MZPokey
(defugen (mz-pokey "MZPokey")
    (&optional (audf1 0) (audc1 0) &key (audf2 0) (audc2 0) (audf3 0) (audc3 0) (audf4 0) (audc4 0) (audctl 0))
  ((:ar (multinew new 'ugen audf1 audc1 audf2 audc2 audf3 audc3 audf4 audc4 audctl))))


;; Nes2
(defugen (nes2 "Nes2") (&optional (trig 0) &key (a0 0) (a1 0) (a2 0) (a3 0) (b0 0) (b1 0) (b2 0) (b3 0) (c0 0) (c2 0) (c3 0) (d0 0) (d2 0) (d3 0) (e0 0) (e1 0) (e2 0) (e3 0) (smask 0))
  ((:ar (multinew new 'ugen trig a0 a1 a2 a3 b0 b1 b2 b3 c0 c2 c3 d0 d2 d3 e0 e1 e2 e3 smask))))

(defun nes2-square.ar (&optional (trig 0) (dutycycle 0) (loopenv 0) (envdecay 0) (vol 10) (sweep 0) (sweeplen 0) (sweepdir 0) (sweepshi 0) (freq 100) (vbl 0))
  (let* ((a0 (*~ (clip.kr (round~ dutycycle) 0 3) 64))
	 (a0 (logior~ a0 (*~ (clip.kr (round~ loopenv) 0 1) 32)))
	 (a0 (logior~ a0 (*~ (clip.kr (round~ envdecay) 0 1) 16)))
	 (a0 (logior~ a0 (clip.kr (round~ vol) 0 15)))
	 (a1 (*~ (clip.kr (round~ sweep) 0 1) 128))
	 (a1 (logior~ a1 (*~ (clip.kr (round~ sweeplen) 0 7) 16)))
	 (a1 (logior~ a1 (*~ (clip.kr (round~ sweepdir) 0 1) 8)))
	 (a1 (logior~ a1 (clip.kr (round~ sweepshi) 0 7)))
	 (a2 (mod~ (round~ (max~ freq 0)) 256))
	 (a3 (clip.kr (floor~ (/~ freq 256)) 0 7))
	 (a3 (logior~ a3 (*~ (clip.kr (round~ vbl) 0 31) 8))))
    (nes2.ar trig :a0 a0 :a1 a1 :a2 a2 :a3 a3 :smask 1)))

(defun nes2-triangle.ar (&optional (trig 0) (start 0) (counter 10) (freq 100) (vbl 0))
  (let* ((c0 (*~ (clip.kr (round~ start) 0 1) 128))
	 (c0 (logior~ c0 (clip.kr (round~ counter) 0 127)))
	 (c2 (mod~ (round~ (max~ freq 0)) 256))
	 (c3 (clip.kr (floor~ (/~ freq 256)) 0 7))
	 (c3 (logior~ c3 (*~ (clip.kr (round~ vbl) 0 31) 8))))
    (nes2.ar trig :c0 c0 :c2 c2 :c3 c3 :smask 4)))

(defun nes2-noise.ar (&optional (trig 0) (loopenv 0) (envdecay 0) (vol 10) (short 0) (freq 10) (vbl 0))
  (let* ((d0 (*~ (clip.kr (round~ loopenv) 0 1) 32))
	 (d0 (logior~ d0 (*~ (clip.kr (round~ envdecay) 0 1) 16)))
	 (d0 (logior~ d0 (clip.kr (round~ vol) 0 15)))
	 (d2 (*~ (clip.kr (round~ short) 0 1) 128))
	 (d2 (logior~ d2 (clip.kr (round~ freq) 0 15)))
	 (d3 (*~ (clip.kr (round~ vbl) 0 31) 8)))
    (nes2.ar trig :d0 d0 :d2 d2 :d3 d3 :smask 8)))

(defun nes2-dmc.ar (&optional (trig) (loop 0) (freq 1))
  (let* ((e0 (*~ (clip.kr (round~ loop) 0 1) 64))
	 (e0 (logior~ e0 (clip.kr (round~ freq) 0 7)))
	 (e1 0)
	 (e2 0)
	 (e3 0))
    (nes2.ar trig :e0 e0 :e1 e1 :e2 e2 :e3 e3 :smask 16)))

(export '(nes2-square.ar nes2-triangle.ar nes2-noise.ar nes2-dmc.ar))

;; Pokey
(defugen (pokey "Pokey") 
    (&optional (audf1 0) (audc1 0) (audf2 0) (audc2 0) (audf3 0) (audc3 0) (audf4 0) (audc4 0) (audctl 0))
  ((:ar (multinew new 'ugen audf1 audc1 audf2 audc2 audf3 audc3 audf4 audc4 audctl))))

(defun pokey-square.ar (&optional (freq1 0) (tone1 0) (vol1 0) (freq2 0) (tone2 0) (vol2 0) (freq3 0) (tone3 0) (vol3 0) (freq4 0) (tone4 0) (vol4 0) (ctrl 0))
  (pokey.ar (clip.kr freq1 0 255)
	    (logior~ (*~ (clip.kr (round~ tone1) 0 7) 32) (clip.kr (round~ vol1) 0 15))
	    (clip.kr freq2 0 255)
	    (logior~ (*~ (clip.kr (round~ tone2) 0 7) 32) (clip.kr (round~ vol2) 0 15))
	    (clip.kr freq3 0 255)
	    (logior~ (*~ (clip.kr (round~ tone3) 0 7) 32) (clip.kr (round~ vol3) 0 15))
	    (clip.kr freq4 0 255)
	    (logior~ (*~ (clip.kr (round~ tone4) 0 7) 32) (clip.kr (round~ vol4) 0 15))
	    (+~ (*~ (round~ (-~ (mod~ (round~ ctrl) 64) 1) 2) 4)
		(mod~ (round~ ctrl) 2))))

(export '(pokey-square.ar))


;; RedDPCM
(defugen (red-dpcm-encode "RedDPCMencode")
    (&optional (in 0.0) (round 0.0))
  ((:ar (multinew new 'pure-ugen in round))
   (:kr (multinew new 'pure-ugen in round)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (red-dpcm-decode "RedDPCMdecode")
    (&optional (in 0.0))
  ((:ar (multinew new 'pure-ugen in))
   (:kr (multinew new 'pure-ugen in)))
  :check-fn #'check-same-rate-as-first-input)


;; RedLbyl
(defugen (red-lbyl "RedLbyl")
    (&optional (in 0.0) (thresh 0.5) (samples 2))
  ((:ar (multinew new 'pure-ugen in thresh samples))
   (:kr (multinew new 'pure-ugen in thresh samples)))
  :check-fn #'check-same-rate-as-first-input)

;; RedNoise
(defugen (red-noise "RedNoise")
    (&optional (clock 0) (mul 1) (add 0))
  ((:ar (madd  (multinew new 'ugen clock) mul add))
   (:kr (madd (multinew new 'ugen clock) mul add))))

;; RedPhasor
(defugen (red-phasor "RedPhasor")
    (&optional (trig 0.0) (rate 1) (start 0) (end 1) &key (loop 0) (loop-start 0) (loop-end 1))
  ((:ar (multinew new 'ugen trig rate start end loop loop-start loop-end))
   (:kr (multinew new 'ugen trig rate start end loop loop-start loop-end))))

(defugen (red-phasor2 "RedPhasor2")
    (&optional (trig 0.0) (rate 1) (start 0) (end 1) &key (loop 0) (loop-start 0) (loop-end 1))
  ((:ar (multinew new 'ugen trig rate start end loop loop-start loop-end))
   (:kr (multinew new 'ugen trig rate start end loop loop-start loop-end))))

;; SID6581f
(defugen (sid6581f "SID6581f")
    (&optional (freq-lo0 0) (freq-hi0 0) (pw-lo0 0) (pw-hi0 0) (ctrl0 0) (atk-dcy0 0) (sus-rel0 0)
	  (freq-lo1 0) (freq-hi1 0) (pw-lo1 0) (pw-hi1 0) (ctrl1 0) (atk-dcy1 0) (sus-rel1 0)
	  (freq-lo2 0) (freq-hi2 0) (pw-lo2 0) (pw-hi2 0) (ctrl2 0) (atk-dcy2 0) (sus-rel2 0)
	  (fc-lo 0) (fc-hi 0) (res-filt 0) (mode-vol 0) (rate 1))
  ((:ar (multinew new 'ugen freq-lo0 freq-hi0 pw-lo0 pw-hi0 ctrl0 atk-dcy0 sus-rel0
		  freq-lo1 freq-hi1 pw-lo1 pw-hi1 ctrl1 atk-dcy1 sus-rel1
		  freq-lo2 freq-hi2 pw-lo2 pw-hi2 ctrl2 atk-dcy2 sus-rel2
		  fc-lo fc-hi res-filt mode-vol rate))))


;; SN76489
(defugen (sn76489 "SN76489")
  (&optional (tone0 512) (tone1 0) (tone2 0) (noise 0) (vol0 15) (vol1 0) (vol2 0) (vol3 0) (rate 1))
  ((:ar (multinew new 'ugen tone0 tone1 tone2 noise vol0 vol1 vol2 vol3 rate))))


;; Slub
(defugen (slub "Slub")
  (&optional (trig 0.0) (spike 4.04))
  ((:ar (multinew new 'ugen trig spike))
   (:kr (multinew new 'ugen trig spike))))


;; Tbjorklund
(defugen (t-bjorklund "Tbjorklund")
  (&optional (rate 8) (width 0.5) (k 4) (n 8) (offset 0))
  ((:ar (multinew new 'ugen rate width k n offset))
   (:kr (multinew new 'ugen rate width k n offset)))
  :signal-range :unipolar)

;; Tsieve

;; WavesetRepeater
(defugen (waveset-repeater "WavesetRepeater")
  (in &optional (repeats 8) (rate 1) (numzc 1) (memlen 0.1) (interpolation 2))
  ((:ar (multinew new 'ugen in repeats rate numzc memlen interpolation))
   (:kr (multinew new 'ugen in repeats rate numzc memlen interpolation))))


