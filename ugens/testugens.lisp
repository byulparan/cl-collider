(in-package #:sc)

(defugen (check-bad-values "CheckBadValues")
    (&optional (in 0.0) (id 0) (post 2))
  ((:ar (multinew new 'ugen in id post))
   (:kr (multinew new 'ugen in id post))))

(defugen (sanitize "Sanitize")
    (&optional (in 0.0) (replace 0.0))
  ((:ar (multinew new 'ugen in replace))
   (:kr (multinew new 'ugen in replace))))
