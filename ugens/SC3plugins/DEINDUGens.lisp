(in-package #:sc)

(defugen (greyhole "GreyholeRaw")
    (in &optional (delay-time 2.0) (damp 0.0) (size 1.0) (diff 0.707) (feedback 0.9) (mod-depth 0.1) (mod-freq 2.0))
  ((:ar
    (let ((in (alexandria:ensure-list in)))
      (multinew new 'multiout-ugen 2 (if (alexandria:length= 1 in)
					 in
					 (elt in 0))
		(if (alexandria:length= 1 in)
		    in
		    (elt in 1))
		delay-time damp size diff feedback mod-depth mod-freq)))))
