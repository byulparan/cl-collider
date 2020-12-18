(in-package #:sc)

(defugen (spring "Spring")
    (&optional (in 0.0) (spring 1.0) (damp 0.0))
  ((:ar (multinew new 'ugen in spring damp))
   (:kr (multinew new 'ugen in spring damp))))

(defugen (ball "Ball")
    (&optional (in 0.0) (g 1.0) (damp 0.0) (friction 0.01))
  ((:ar (multinew new 'ugen in g damp friction))
   (:kr (multinew new 'ugen in g damp friction))))

(defugen (t-ball "TBall")
    (&optional (in 0.0) (g 10.0) (damp 0.0) (friction 0.01))
  ((:ar (multinew new 'ugen in g damp friction))
   (:kr (multinew new 'ugen in g damp friction))))


