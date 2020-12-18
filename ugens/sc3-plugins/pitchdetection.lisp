(in-package #:sc)

(defugen (tartini "Tartini")
    (&optional (in 0.0) (threshold .93) (n 2048) (k 0) (overlap 1024) (small-cutoff .5))
  ((:kr (multinew new 'multiout-ugen 2 in threshold n k overlap small-cutoff))))

(defugen (qitch "Qitch")
    (&optional (in 0.0) (databufnum 0.0) (ampthreshold 0.01) (algoflag 1.0) ampbufnum (minfreq 0.0) (maxfreq 2500))
  ((:kr (multinew new 'multiout-ugen 2 in databufnum ampthreshold algoflag (if ampbufnum ampbufnum -1) minfreq maxfreq))))

