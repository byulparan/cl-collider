(in-package #:sc)

(defugen (i-env-gen "IEnvGen")
    (envelope index &optional (mul 1.0) (add 0.0))
  ((:ar (madd (unbubble (mapcar #'process-env (multinew new 'ugen index
						  (make-env-array-from-env envelope)))) mul add))
   (:kr (madd (unbubble (mapcar #'process-env (multinew new 'ugen index
						  (make-env-array-from-env envelope)))) mul add))))
