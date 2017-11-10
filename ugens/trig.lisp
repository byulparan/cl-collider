
(in-package #:sc)

(defugen (trig-1 "Trig1") (&optional (in 0.0) (dur 0.1))
  ((:ar (multinew new 'ugen in dur))
   (:kr (multinew new 'ugen in dur)))
  :signal-range :unipolar)

(defugen (trig "Trig") (&optional (in 0.0) (dur 0.1))
  ((:ar (multinew new 'ugen in dur))
   (:kr (multinew new 'ugen in dur)))
  :signal-range :unipolar)

(defugen (send-trig "SendTrig")
    (&optional (in 0.0) (id 0) (value 0.0))
  ((:ar (progn (multinew new 'nooutput-ugen in id value) 0.0))
   (:kr (progn (multinew new 'nooutput-ugen in id value) 0.0)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (send-reply "SendReply")
    (&optional (trig 0.0) (cmd "/reply") values (reply-id -1))
  ((:kr
    (let ((values (alexandria:ensure-list values)))
      (unless (find-if #'listp values) (setf values (bubble values)))
      (dolist (args (flop (list trig cmd values reply-id)))
	(destructuring-bind (trig cmd value rep-id) args
	  (let ((arg (list trig rep-id (length cmd))))
	    (apply #'multinew new 'nooutput-ugen
		   (append arg (map 'list #'char-code cmd) (alexandria:ensure-list value))))))
      0.0))
   (:ar
    (let ((values (alexandria:ensure-list values)))
      (unless (find-if #'listp values) (setf values (bubble values)))
      (dolist (args (flop (list trig cmd values reply-id)))
	(destructuring-bind (trig cmd value rep-id) args
	  (let ((arg (list trig rep-id (length cmd))))
	    (apply #'multinew new 'nooutput-ugen
		   (append arg (map 'list #'char-code cmd) (alexandria:ensure-list value))))))
      0.0))))


(defugen (t-delay "TDelay") (&optional (in 0.0) (dur 0.1))
  ((:ar (multinew new 'ugen in dur))
   (:kr (multinew new 'ugen in dur)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (latch "Latch") (&optional (in 0.0) (trig 0.0))
  ((:ar (multinew new 'ugen in trig))
   (:kr (multinew new 'ugen in trig))))

(defugen (gate "Gate") (&optional (in 0.0) (trig 0.0))
  ((:ar (multinew new 'ugen in trig))
   (:kr (multinew new 'ugen in trig))))

(defugen (pulse-count "PulseCount")
    (&optional (trig 0.0) (reset 0.0))
  ((:ar (multinew new 'ugen trig reset))
   (:kr (multinew new 'ugen trig reset)))
  :check-fn #'check-same-rate-as-first-input)


(defun peak-check-fn (ugen)
  (if (and (eql (rate ugen) :control) (eql (rate (nth 0 (inputs ugen))) :audio))
      (error "valid inputs")
      (check-same-rate-as-first-input ugen)))

(defugen (peak "Peak") (&optional (in 0.0) (trig 0.0))
  ((:ar (multinew new 'ugen in trig))
   (:kr (multinew new 'ugen in trig)))
  :check-fn #'peak-check-fn)

(defugen (running-min "RunningMin")
    (&optional (in 0.0) (trig 0.0))
  ((:ar (multinew new 'ugen in trig))
   (:kr (multinew new 'ugen in trig)))
  :check-fn #'peak-check-fn)

(defugen (running-max "RunningMax")
    (&optional (in 0.0) (trig 0.0))
  ((:ar (multinew new 'ugen in trig))
   (:kr (multinew new 'ugen in trig)))
  :check-fn #'peak-check-fn)



(defugen (stepper "Stepper")
    (&optional (trig 0.0) (reset 0) (min 0) (max 7) (step 1) reset-val)
  ((:ar (multinew new 'ugen trig reset min max step (if reset-val reset-val min)))
   (:kr (multinew new 'ugen trig reset min max step (if reset-val reset-val min))))
  :check-fn #'check-same-rate-as-first-input)

(defugen (pulse-divier "PulseDivider") (&optional (trig 0.0) (div 2.0) (start 0.0))
  ((:ar (multinew new 'ugen trig div start))
   (:kr (multinew new 'ugen trig div start))))

(defugen (set-reset-ff "SetResetFF")
    (&optional (trig 0.0) (div 2.0) (start 0.0))
  ((:ar (multinew new 'ugen trig div start))
   (:kr (multinew new 'ugen trig div start)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (toggle-ff "ToggleFF")  (&optional (trig 0.0))
  ((:ar (multinew new 'ugen trig))
   (:kr (multinew new 'ugen trig))))

(defugen (zero-crossing "ZeroCrossing") (&optional (in 0.0))
  ((:ar (multinew new 'ugen in))
   (:kr (multinew new 'ugen in)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (timer "Timer") (&optional (trig 0.0))
  ((:ar (multinew new 'ugen trig))
   (:kr (multinew new 'ugen trig)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (sweep "Sweep")
    (&optional (trig 0.0) (srate 1.0))
  ((:ar (multinew new 'ugen trig srate))
   (:kr (multinew new 'ugen trig srate))))

(defugen (phasor "Phasor")
    (&optional (trig 0.0) (srate 1.0) (start 0.0) (end 1.0) (reset-pos 0.0))
  ((:ar (multinew new 'ugen trig srate start end reset-pos))
   (:kr (multinew new 'ugen trig srate start end reset-pos))))

(defugen (peak-follower "PeakFollower") (&optional (in 0.0) (decay 0.999))
  ((:ar (multinew new 'ugen in decay))
   (:kr (multinew new 'ugen in decay))))


(defugen (pitch "Pitch") 
    (&optional (in 0.0) (init-freq 440.0) (min-freq 60.0) (max-freq 4000.0)
	       (exec-freq 100.0) (max-bins-per-octave 16) (median 1)
	       (amp-threshold 0.01) (peak-threshold 0.5) (down-sample 1) (clar 0.0))
  ((:kr (multinew new 'multiout-ugen 2 in init-freq min-freq max-freq exec-freq max-bins-per-octave
		  median amp-threshold peak-threshold down-sample clar))))

(defugen (coyote "Coyote")
    (&optional (in 0.0) (track-fall 0.2) (slow-lag 0.2) (fast-lag 0.01) (fast-mul 0.5) (thresh 0.05) (min-dur 0.1))
  ((:kr (multinew new 'ugen in track-fall slow-lag fast-lag fast-mul thresh min-dur))))

(defugen (in-range "InRange")
    (&optional (in 0.0) (lo 0.0) (hi 1.0))
  ((:ar (multinew new 'ugen in lo hi))
   (:kr (multinew new 'ugen in lo hi))))

(defugen (fold "Fold")
    (&optional (in 0.0) (lo 0.0) (hi 1.0))
  ((:ir (let* ((x (-~ in lo))
               (range (-~ hi lo))
               (range2 (+~ range range))
               (c (-~ x (*~ range2 (floor~ (/~ x range2)))))
               (c (if~ (>=~ c range) (-~ range2 c) c)))
          (+~ c lo)))
   (:ar (multinew new 'ugen in lo hi))
   (:kr (multinew new 'ugen in lo hi))))

(defmethod fold (in &optional (lo 0.0) (hi 1.0))
  (let ((rate (rate (list in lo hi))))
    (funcall (case rate
               (:scalar #'fold.ir)
               (:audio #'fold.ar)
               (:control #'fold.kr))
             in lo hi)))

(defugen (clip "Clip") (&optional (in 0.0) (lo 0.0) (hi 1.0))
  ((:ir (max~ (min~ in hi) lo))
   (:ar (multinew new 'ugen in lo hi))
   (:kr (multinew new 'ugen in lo hi))))

(defmethod clip (in &optional (lo 0.0) (hi 1.0))
  (let ((rate (rate (list in lo hi))))
    (funcall (case rate
               (:scalar #'clip.ir)
               (:audio #'clip.ar)
               (:control #'clip.kr))
             in lo hi)))

(defugen (wrap "Wrap")
    (&optional (in 0.0) (lo 0.0) (hi 1.0))
  ((:ir (+~ (mod~ (-~ in lo) (-~ hi lo)) lo))
   (:ar (multinew new 'ugen in lo hi))
   (:kr (multinew new 'ugen in lo hi))))

(defmethod wrap (in &optional (lo 0.0) (hi 1.0))
  (let ((rate (rate (list in lo hi))))
    (funcall (case rate
               (:scalar #'wrap.ir)
               (:audio #'wrap.ar)
               (:control #'wrap.kr))
             in lo hi)))

(defugen (Schmidt "Schmidt")
    (&optional (in 0.0) (lo 0.0) (hi 1.0))
  ((:ar (multinew new 'ugen in lo hi))
   (:kr (multinew new 'ugen in lo hi))))

(defugen (most-change "MostChange")
    (&optional (a 0.0) (b 0.0))
  ((:ar (multinew new 'ugen a b))
   (:kr (multinew new 'ugen a b))))

(defugen (least-change "LeastChange")
    (&optional (a 0.0) (b 0.0))
  ((:ar (multinew new 'ugen a b))
   (:kr (multinew new 'ugen a b))))

(defugen (last-value "LastValue")
    (&optional (in 0.0) (diff 0.01))
  ((:ar (multinew new 'ugen in diff))
   (:kr (multinew new 'ugen in diff))))



(defclass send-peak-rms (ugen)
  ())

(defmethod num-outputs ((ugen send-peak-rms))
  0)

(defugen (send-peak-rms "SendPeakRMS")
    (sig &optional (reply-rate 20.0) (peak-lag 3) (cmd-name "/reply") (reply-id -1))
  ((:ar (progn (apply new 'send-peak-rms reply-rate peak-lag reply-id (length (alexandria:ensure-list sig))
		      (append (alexandria:ensure-list sig) (list (length cmd-name)) (map 'list #'char-code cmd-name)))
	       0))
   (:kr (progn (apply new 'send-peak-rms reply-rate peak-lag reply-id (length (alexandria:ensure-list sig))
		      (append (alexandria:ensure-list sig) (list (length cmd-name)) (map 'list #'char-code cmd-name)))
	       0))))

