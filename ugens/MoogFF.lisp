(in-package #:sc)

(defugen (moog-ff "MoogFF")
    (in &optional (freq 100) (gain 2) (reset 0) (mul 1) (add 0))
  ((:ar
    (madd (multinew new 'pure-ugen in freq gain reset) mul add))
   (:kr
    (madd (multinew new 'pure-ugen in freq gain reset) mul add)))
  :check-fn #'check-same-rate-as-first-input)
