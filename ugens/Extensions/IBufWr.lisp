(in-package :cl-collider)

;; https://github.com/tremblap/IBufWr
;; an interpolating buffer writer.

(sc::defugen (ibuf-wr "IBufWr")
    (input-array &optional (bufnum 0) (phase 0.0) (interp 1) (feedback 0))
  ((:ar (sc::multinew-list sc::new 'sc::ugen (append (list bufnum phase interp feedback) (alexandria:ensure-list input-array))))
   (:kr (sc::multinew-list sc::new 'sc::ugen (append (list bufnum phase interp feedback) (alexandria:ensure-list input-array)))))
  :check-fn #'sc::check-when-audio)
