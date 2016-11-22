
(in-package #:sc)

(defugen (freeverb "FreeVerb")
    (in &key (mix 0.33) (room 0.5) (damp 0.5) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in mix room damp) mul add))))

(defugen (freeverb2 "FreeVerb2")
    (in1 in2 &key (mix 0.33) (room 0.5) (damp 0.5) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen 2 in1 in2 mix room damp) mul add)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 2)))

(defugen (g-verb "GVerb")
    (in &optional (roomsize 10) (revtime 3) (damping .5) (inputbw .5) (spread 15)
	(drylevel 1) (earlyreflevel .7) (taillevel .5) (maxroomsize 300) (mul 1) (add 0))
  ((:ar (madd (multinew new 'multiout-ugen 2 in roomsize revtime damping inputbw spread
			drylevel earlyreflevel taillevel maxroomsize) mul add)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 1)))
