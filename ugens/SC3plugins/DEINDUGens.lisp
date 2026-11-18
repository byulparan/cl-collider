(in-package #:sc)

(defugen (greyhole "GreyholeRaw")
    (in &optional (delay-time 2.0) (damp 0.0) (size 1.0) (diff 0.707) (feedback 0.9) (mod-depth 0.1) (mod-freq 2.0))
  ((:ar
    (let ((in (alexandria:ensure-list in)))
      (multinew new 'multiout-ugen 2
		(if (alexandria:length= 1 in)
		    in
		    (elt in 0))
		(if (alexandria:length= 1 in)
		    in
		    (elt in 1))
		delay-time damp size diff feedback mod-depth mod-freq)))))

(defugen (jpverb "JPverbRaw")
    (in &optional (time 1.0) (damp 0.0) (size 1.0) (early-diff 0.707) (mod-depth 0.1) (mod-freq 2.0) (low 1.0) (mid 1.0) (high 1.0) (low-cut 500.0) (high-cut 2000.0))
  ((:ar
    (let ((in (alexandria:ensure-list in)))
      (multinew new 'multiout-ugen 2
		(if (alexandria:length= 1 in)
		    in
		    (elt in 0))
		(if (alexandria:length= 1 in)
		    in
		    (elt in 1))
		damp early-diff high-cut high low-cut low mod-depth mod-freq mid size time)))))

(defugen (diode-ring-mod "DiodeRingMod")
    (carrier modulator)
  ((:ar (sc::multinew new 'ugen
                      carrier modulator))))

(defugen (complex-res "ComplexRes")
    (&optional (in 0) (freq 100) (decay 0.2))
  ((:ar (sc::multinew new 'ugen
                      in freq decay))))

(defugen (rms "RMS")
    (in &key (lp-freq 100))
  ((:ar (multinew new 'ugen
                  in lp-freq))
   (:kr (multinew new 'ugen
                  in lp-freq))))
