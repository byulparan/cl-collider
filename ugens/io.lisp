
(in-package #:sc)


(defugen (in "In")
    (&optional (bus 0) (chanls 1))
  ((:ar (multinew new 'multiout-ugen chanls bus))
   (:kr (multinew new 'multiout-ugen chanls bus))))

(defugen (local-in "LocalIn") (&optional (chanls 1) (default 0.0))
  ((:ar (multinew new 'multiout-ugen chanls default))
   (:kr (multinew new 'multiout-ugen chanls default))))

(defugen (lag-in "LagIn") (&optional (bus 0) (chanls 1) (lag 0.1))
  ((:kr (multinew new 'multiout-ugen chanls bus lag))))

(defugen (in-feedback "InFeedback") (&optional (bus 0) (chanls 1))
  ((:ar (multinew new 'multiout-ugen chanls bus))))

(defugen (in-trig "InTrig") (&optional (bus 0) (chanls 1))
  ((:kr (multinew new 'multiout-ugen chanls bus))))


(defclass abstract-out (ugen) ())

(defmethod num-outputs ((ugen abstract-out))
  0)

(defun abstract-out-check (ugen fixed-args)
  (when (eql (rate ugen) :audio)
    (loop for i from fixed-args below (length (inputs ugen))
	  do (unless (eql (rate (nth i (inputs ugen))) :audio)
	       (error (format nil "Input at index ~a (~a) is not audio rate."
			      i (nth i (inputs ugen))))))))

(defun replace-zeroes-with-silence (channel-array)
  (let* ((pos 0)
	 (num-zeros (count 0 channel-array :test #'equalp)))
    (when (zerop num-zeros) (return-from replace-zeroes-with-silence channel-array))
    (let ((silent-ch (alexandria:ensure-list (silent.ar num-zeros))))
      (loop for item in channel-array
	    for i from 0
	    do (let (res)
		 (if (equalp item 0.0) (progn (setf (nth i channel-array) (nth pos silent-ch))
					      (incf pos))
		     (when (listp item)
		       (setf res (replace-zeroes-with-silence item))
		       (setf (nth i channel-array) res)))))
      channel-array)))


(defugen (out "Out") (bus channels-array)
  ((:ar (let ((channels (replace-zeroes-with-silence (alexandria:ensure-list channels-array))))
	  (apply #'multinew new 'abstract-out (cons bus channels))
	  0))
   (:kr (progn
	  (apply #'multinew new 'abstract-out (cons bus (alexandria:ensure-list channels-array)))
	  0)))
  :check-fn (lambda (ugen) (abstract-out-check ugen 1)))

(defugen (replace-out "ReplaceOut") (bus channels-array)
  ((:ar (let ((channels (replace-zeroes-with-silence (alexandria:ensure-list channels-array))))
	  (apply #'multinew new 'abstract-out (cons bus channels))
	  0))
   (:kr (progn
	  (apply #'multinew new 'abstract-out (cons bus (alexandria:ensure-list channels-array)))
	  0)))
  :check-fn (lambda (ugen) (abstract-out-check ugen 1)))

(defugen (offset-out "OffsetOut") (bus channels-array)
  ((:ar (let ((channels (replace-zeroes-with-silence (alexandria:ensure-list channels-array))))
	  (apply #'multinew new 'abstract-out (cons bus channels))
	  0))
   (:kr (progn new (error "Control rate OffsetOut is not implemented (bus: ~a channels-array: ~a)." bus channels-array))))
  :check-fn (lambda (ugen) (abstract-out-check ugen 1)))





(defugen (local-out "LocalOut") (channels-array)
  ((:ar (let ((channels (replace-zeroes-with-silence (alexandria:ensure-list channels-array))))
	  (apply #'multinew new 'abstract-out channels)
	  0))
   (:kr (progn (apply #'multinew new 'abstract-out (alexandria:ensure-list channels-array))
	  0)))
  :check-fn (lambda (ugen) (abstract-out-check ugen 0)))



(defugen (x-out "XOut") (bus xfade channels-array)
  ((:ar (let ((channels (replace-zeroes-with-silence (alexandria:ensure-list channels-array))))
	  (apply #'multinew new 'abstract-out (cons bus (cons xfade channels)))
	  0))
   (:kr (progn (apply #'multinew new 'abstract-out (cons bus (cons xfade (alexandria:ensure-list channels-array)))) 0)))
  :check-fn (lambda (ugen) (abstract-out-check ugen 2)))


