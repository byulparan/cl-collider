
(in-package #:sc)


(defun pm-osc.ar (car-freq mod-freq &optional (pm-index 0.0) (mod-phase 0.0) (mul 1.0) (add 0.0))
  (sin-osc.ar car-freq (sin-osc.ar mod-freq mod-phase pm-index) mul add))

(defun pm-osc.kr (car-freq mod-freq &optional (pm-index 0.0) (mod-phase 0.0) (mul 1.0) (add 0.0))
  (sin-osc.kr car-freq (sin-osc.kr mod-freq mod-phase pm-index) mul add))


(export 'pm-osc.ar)
(export 'pm-osc.kr)


