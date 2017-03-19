
(in-package #:sc)

(defugen (poll "Poll")
    (&optional (trig 0) (in 0.0) (label 0) (trigid -1))
  ((:ar (multinew new 'pure-ugen trig in label trigid))
   (:kr (multinew new 'pure-ugen trig in label trigid))))
