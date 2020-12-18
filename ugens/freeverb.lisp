(in-package #:sc)

(defugen (freeverb "FreeVerb")
    (in &key (mix 0.33) (room 0.5) (damp 0.5) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in mix room damp) mul add))))

(defugen (freeverb2 "FreeVerb2")
    (in1 in2 &key (mix 0.33) (room 0.5) (damp 0.5) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen 2 in1 in2 mix room damp) mul add)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 2)))
