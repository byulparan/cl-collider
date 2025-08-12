(in-package #:sc)

(defugen (red-impulse "RedImpulse")
  (&optional (freq 440.0) (reset .0) (mul 1.0) (add 0.0))
  ((:ar (let ((osc (phasor.ar reset (/~ freq (sample-rate.ir)))))
	  (mul-add (+~ (<~ (-~ osc (delay-1.ar osc)) 0) (impulse.ar 0)) mul add)))
   (:kr (let ((osc (phasor.kr reset (/~ freq (control-rate.ir)))))
	  (mul-add (+~ (<~ (-~ osc (delay-1.kr osc)) 0) (impulse.kr 0)) mul add)))))


