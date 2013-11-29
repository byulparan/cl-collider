(in-package #:sc)

(def-binary-op clip2~ (lambda (a b) (clip a (* -1 b) b))
    (:special-index 42))

(defmethod clip2 ((a ugen) &optional (b 1))
  (clip2~ a b))

(defmethod clip2 ((a number) &optional b)
  (clip2~ a b))














