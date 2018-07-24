(in-package #:cl-collider)

(defugen (ladspa "LADSPA")
    (num-channels id &rest args)
  ((:ar (multinew-list new 'multiout-ugen (append (list 1 num-channels id) args)))))
