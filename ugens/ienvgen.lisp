(in-package #:sc)

(defun as-array-for-interpolation (env)
  "Convert ENV into an array in a format suitable for i-env-gen."
  (with-slots (levels times curve-value) env
    (let* ((size (length times))
           (contents (append (list
                              0 ;; Env offset (not implemented in cl-collider)
                              (elt levels 0)
                              size
                              (reduce #'+ times))
                             (loop :for i :from 0 :below size
                                :append (list (elt times i)
                                              (env-shape-number (nth-wrap i curve-value))
                                              (nth-wrap i curve-value)
                                              (elt levels (1+ i)))))))
      (mapcar (lambda (list)
                (coerce list 'vector))
              (flop contents)))))

(defugen (i-env-gen "IEnvGen")
    (envelope index &optional (mul 1.0) (add 0.0))
  ((:ar (madd (unbubble (mapcar #'process-env (multinew new 'ugen index
						                                (as-array-for-interpolation envelope)))) mul add))
   (:kr (madd (unbubble (mapcar #'process-env (multinew new 'ugen index
						                                (as-array-for-interpolation envelope)))) mul add))))
