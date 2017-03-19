
(in-package #:sc)

(defclass poll-ugen (ugen)
  ())

(defugen (poll "Poll")
    (&optional (trig 0) (in 0.0) label (trigid -1))
  ((:ar (let ((trig (alexandria:ensure-cons trig)))
	  (setf trig (unbubble (loop for tr in trig collect (etypecase tr
							      (number (impulse.kr tr))
							      (ugen tr)))))
	  (multinew new 'poll-ugen trig in label trigid)
	  in))
   (:kr (let ((trig (alexandria:ensure-list trig)))
	  (setf trig (unbubble (loop for tr in trig collect (etypecase tr
							      (number (impulse.kr tr))
							      (ugen tr)))))
	  (multinew new 'poll-ugen trig in label trigid)
	  in)))
  :check-fn #'check-same-rate-as-first-input)

(defmethod new1 ((ugen poll-ugen) &rest inputs)
  (destructuring-bind (trig in label trigid)
      inputs
    (unless label (setf label (format nil "UGen(~a)" in)))
    (setf label (map 'list #'char-code (format nil "~a" label)))
    (setf inputs (append (list trig in trigid (length label)) label))
    (setf (inputs ugen) inputs)
    (add-to-synth ugen)))

