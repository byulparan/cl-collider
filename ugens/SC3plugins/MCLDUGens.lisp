(in-package #:cl-collider)


;; ================================================================================
;; MCLDOscUGens
;; ================================================================================

(defugen (saw-dpw "SawDPW")
    (&optional (freq 440.0) (iphase 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq iphase) mul add))
   (:kr (madd (multinew new 'ugen freq iphase) mul add))))



;; ================================================================================
;; MCLDFFTUGens
;; ================================================================================

(def-pv-chain-ugen (pv-whiten "PV_Whiten") ;; from MCLDUgens
    (buffer track-bufnum &optional (relax-time 2.0) (floor 0.1) (smear 0.0) (bin-downsample 0.0))
  (multinew new 'pv-chain-ugen buffer track-bufnum relax-time floor smear bin-downsample))




;; ================================================================================
;; MCLDBufferUGens
;; ================================================================================

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
