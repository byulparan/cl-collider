(in-package #:cl-collider)

(def-pv-chain-ugen (pv-whiten "PV_Whiten") ;; from MCLDUgens
    (buffer track-bufnum &optional (relax-time 2.0) (floor 0.1) (smear 0.0) (bin-downsample 0.0))
  (multinew new 'pv-chain-ugen buffer track-bufnum relax-time floor smear bin-downsample))
