(in-package #:sc)

(defugen (p-sin-grain "PSinGrain")
    (&optional (freq 440.0) (dur 0.2) (amp 0.1))
  ((:ar (multinew new 'ugen freq dur amp))))

