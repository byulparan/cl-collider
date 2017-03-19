
(in-package #:sc)

(defugen (env-follow "EnvFollow")
    (&optional (in 0.0) (decaycoeff 0.99) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in decaycoeff) mul add))
   (:kr (madd (multinew new 'pure-ugen in decaycoeff) mul add))))
