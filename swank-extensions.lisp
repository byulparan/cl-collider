(in-package #:cl-collider)

;; make slime show the synthdef's argument list for (synth ...)
(defmethod swank::compute-enriched-decoded-arglist ((operator-form (eql 'synth))
                                                    argument-forms)
  (let* ((fst (car argument-forms))
         (controls (unless (typep fst 'swank::arglist-dummy)
                     (synthdef-metadata (if (and (listp fst)
                                                 (eql 'quote (car fst)))
                                            (cadr fst)
                                            fst)
                                        :controls))))
    (if controls
        (loop
          :for ctl :in controls
          :if (atom ctl)
            :collect ctl :into req
          :if (listp ctl)
            :collect (swank::make-keyword-arg
                      (alexandria:make-keyword (car ctl))
                      (car ctl)
                      (cadr ctl))
              :into key
          :finally
             (return
               (swank::make-arglist
                :required-args (append (list fst) req)
                :key-p t
                :keyword-args (append
                               key
                               (swank::keywords-of-operator operator-form)))))
        (call-next-method))))
