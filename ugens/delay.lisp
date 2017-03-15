
(in-package #:sc)

(defun as-audio-rate-input (input)
  "Return an audio rate version of INPUT if it is not already audio rate."
  (let ((op (lambda (cls in)
	      (declare (ignore cls))
	      (if (eql :audio (rate in)) in
		  (k2a.ar in)))))
    (multinew op nil input)))

(defugen (Delay-1 "Delay1") (&optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in) mul add))
   (:kr (madd (multinew new 'pure-ugen in) mul add))))

(defugen (Delay-2 "Delay2") (&optional (in 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in) mul add))
   (:kr (madd (multinew new 'pure-ugen in) mul add))))


(defugen (delay-n "DelayN")
    (&optional (in 0.0) (max-delay-time 0.2) (delay-time 0.2) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen (as-audio-rate-input in) max-delay-time delay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in max-delay-time delay-time) mul add))))

(defugen (delay-l "DelayL")
    (&optional (in 0.0) (max-delay-time 0.2) (delay-time 0.2) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen (as-audio-rate-input in) max-delay-time delay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in max-delay-time delay-time) mul add))))

(defugen (delay-c "DelayC")
    (&optional (in 0.0) (max-delay-time 0.2) (delay-time 0.2) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen (as-audio-rate-input in) max-delay-time delay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in max-delay-time delay-time) mul add))))

(defugen (comb-n "CombN")
    (&optional (in 0.0) (max-delay-time 0.2) (delay-time 0.2) (decay-time 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen (as-audio-rate-input in) max-delay-time delay-time decay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in max-delay-time delay-time decay-time) mul add))))

(defugen (comb-l "CombL")
    (&optional (in 0.0) (max-delay-time 0.2) (delay-time 0.2) (decay-time 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen (as-audio-rate-input in) max-delay-time delay-time decay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in max-delay-time delay-time decay-time) mul add))))

(defugen (comb-c "CombC")
    (&optional (in 0.0) (max-delay-time 0.2) (delay-time 0.2) (decay-time 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen (as-audio-rate-input in) max-delay-time delay-time decay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in max-delay-time delay-time decay-time) mul add))))

(defugen (allpass-n "AllpassN")
    (&optional (in 0.0) (max-delay-time 0.2) (delay-time 0.2) (decay-time 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen (as-audio-rate-input in) max-delay-time delay-time decay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in max-delay-time delay-time decay-time) mul add))))

(defugen (allpass-l "AllpassL")
    (&optional (in 0.0) (max-delay-time 0.2) (delay-time 0.2) (decay-time 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen (as-audio-rate-input in) max-delay-time delay-time decay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in max-delay-time delay-time decay-time) mul add))))

(defugen (allpass-c "AllpassC")
    (&optional (in 0.0) (max-delay-time 0.2) (delay-time 0.2) (decay-time 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen (as-audio-rate-input in) max-delay-time delay-time decay-time) mul add))
   (:kr (madd (multinew new 'pure-ugen in max-delay-time delay-time decay-time) mul add))))

(defugen (buf-delay-n "BufDelayN")
    (&optional (buf 0) (in 0.0) (delay 0.2) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen buf (as-audio-rate-input in) delay) mul add))
   (:kr (madd (multinew new 'pure-ugen buf in delay) mul add))))

(defugen (buf-delay-l "BufDelayL")
    (&optional (buf 0) (in 0.0) (delay 0.2) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen buf (as-audio-rate-input in) delay) mul add))
   (:kr (madd (multinew new 'pure-ugen buf in delay) mul add))))

(defugen (buf-delay-c "BufDelayC")
    (&optional (buf 0) (in 0.0) (delay 0.2) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen buf (as-audio-rate-input in) delay) mul add))
   (:kr (madd (multinew new 'pure-ugen buf in delay) mul add))))

(defugen (buf-comb-n "BufCombN")
    (&optional (buf 0) (in 0.0) (delay 0.2) (decay 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen buf (as-audio-rate-input in) delay decay) mul add))))

(defugen (buf-comb-l "BufCombL")
    (&optional (buf 0) (in 0.0) (delay 0.2) (decay 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen buf (as-audio-rate-input in) delay decay) mul add))))

(defugen (buf-comb-c "BufCombC")
    (&optional (buf 0) (in 0.0) (delay 0.2) (decay 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen buf (as-audio-rate-input in) delay decay) mul add))))

(defugen (buf-allpass-n "BufAllpassN")
    (&optional (buf 0) (in 0.0) (delay 0.2) (decay 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen buf (as-audio-rate-input in) delay decay) mul add))))

(defugen (buf-allpass-l "BufAllpassL")
    (&optional (buf 0) (in 0.0) (delay 0.2) (decay 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen  buf (as-audio-rate-input in) delay decay) mul add))))

(defugen (buf-allpass-c "BufAllpassC")
    (&optional (buf 0) (in 0.0) (delay 0.2) (decay 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen buf (as-audio-rate-input in) delay decay) mul add))))


(defugen (deltap-wr "DelTapWr") (&optional (buf 0) (in 0))
  ((:ar (multinew new 'ugen buf in))
   (:kr (multinew new 'ugen buf in))))

(defugen (deltap-rd "DelTapRd") (&optional buf phase deltime &key (interp 1)
					   (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen buf phase deltime interp) mul add))
   (:kr (madd (multinew new 'ugen buf phase deltime interp) mul add))))
