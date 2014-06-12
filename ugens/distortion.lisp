
(in-package #:sc)

(defugen (crossover-distortion "CrossoverDistortion")
    (in &optional (amp .5) (smooth .5) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen in amp smooth) mul add))))

(defugen (smooth-decimator "SmoothDecimator")
    (in &optional (rate 44100.0) (smoothing .5) (mul 1.0) (add 0))
  ((:ar (madd (multinew new 'ugen in rate smoothing) mul add))))

(defugen (decimator "Decimator")
    (in &optional (rate 44100.0) (bits 24) (mul 1.0) (add 0))
  ((:ar (madd (multinew new 'ugen in rate bits) mul add))))

(defugen (sine-shaper "SineShaper")
    (in &optional (limit 1.0) (mul 1.0) (add 0))
  ((:ar (madd (multinew new 'ugen in limit) mul add))))

(defugen (disintegrator "Disintegrator")
    (in &optional (probability 0.5) (multiplier 0.0) (mul 1.0) (add 0))
  ((:ar (madd (multinew new 'ugen in probability multiplier) mul add))))
