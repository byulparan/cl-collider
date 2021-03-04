(in-package #:sc)

;; Logger

;; ListTrig

;; ListTrig2

;; GaussClass

;; BufMax

;; BufMin

(defugen (array-max "ArrayMax") (array)
  ((:ar (multinew-list new 'multiout-ugen (append (list 2) array)))
   (:kr (multinew-list new 'multiout-ugen (append (list 2) array)))))

(defugen (array-min "ArrayMin") (array)
  ((:ar (multinew-list new 'multiout-ugen (append (list 2) array)))
   (:kr (multinew-list new 'multiout-ugen (append (list 2) array)))))
