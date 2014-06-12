(in-package #:sc)

(defugen (tartini "Tartini")
    (&optional (in 0.0) (threshold .93) (n 2048) (k 0) (overlap 1024) (small-cutoff .5))
  ((:kr (multinew new 'multiout-ugen 2 in threshold n k overlap small-cutoff))))

