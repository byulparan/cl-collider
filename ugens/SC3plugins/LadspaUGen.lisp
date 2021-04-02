(in-package #:cl-collider)

(defugen (ladspa "LADSPA")
    (num-channels id &rest args)
  ((:ar (multinew-list new 'multiout-ugen (append (list num-channels num-channels id) args)))))
