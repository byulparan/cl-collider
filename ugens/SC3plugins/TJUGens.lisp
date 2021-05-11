(in-package #:sc)

(defugen (dfm-1 "DFM1")
    (in &optional (freq 1000) (res 0.1) (input-gain 1) (type 0) (noise-level 0.0003) (mul 1) (add 0))
  ((:ar
    (madd (multinew new 'pure-ugen in freq res input-gain type noise-level) mul add)))
  :check-fn #'check-same-rate-as-first-input)
