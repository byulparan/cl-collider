(in-package #:sc)

(defun quant (next-time)
  (let ((time (* 1.0d0 (floor (now)))))
    (+ time (- next-time (mod time next-time)))))

