
(in-package #:sc)

(defun sound-in-prim (chan-offset bus mul add)
  (if (not (listp bus)) (sc::madd (in.ar (sc::add chan-offset bus) 1) mul add)
    (if (equal bus (loop repeat (length bus) for i from (car bus) collect i))
	(sc::madd (in.ar (sc::add chan-offset (car bus)) (length bus)) mul add)
      (sc::madd (in.ar (sc::add chan-offset bus)) mul add))))

(defun sound-in.ar (&optional (bus 0) (mul 1.0) (add 0.0))
  (let ((chan-offset (num-output-buses.ir)))
    (sound-in-prim chan-offset bus mul add)))

(defun audio-in.ar (&optional (channel 0) (mul 1.0) (add 0.0))
  (let ((chan-offset (minus (num-output-buses.ir) 1)))
    (sound-in-prim chan-offset channel mul add)))


(export 'sound-in.ar)
