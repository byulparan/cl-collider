(in-package #:sc)

(defugen (g-verb "GVerb")
    (in &optional (roomsize 10) (revtime 3) (damping .5) (inputbw .5) (spread 15)
	(drylevel 1) (earlyreflevel .7) (taillevel .5) (maxroomsize 300) (mul 1) (add 0))
  ((:ar (madd (multinew new 'multiout-ugen 2 in roomsize revtime damping inputbw spread
			drylevel earlyreflevel taillevel maxroomsize) mul add)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 1)))
