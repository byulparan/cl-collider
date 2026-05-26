(in-package #:sc)

(defugen (line "Line")
    (&optional (start 0.0) (end 1.0) (dur 1.0) &key (mul 1.0) (add 0.0) (act :no-action))
  ((:ar (madd (multinew new 'ugen start end dur (act act)) mul add))
   (:kr (madd (multinew new 'ugen start end dur (act act)) mul add))))

(defugen (x-line "XLine")
    (&optional (start 1.0) (end 2.0) (dur 1.0) &key (mul 1.0) (add 0.0) (act :no-action))
  ((:ar (madd (multinew new 'ugen start end dur (act act)) mul add))
   (:kr (madd (multinew new 'ugen start end dur (act act)) mul add))))



(defun lin-lin.ar (in &optional (srclo 0.0) (srchi 1.0) (dstlo 1.0) (dsthi 2.0))
  (let* ((scale (/~ (-~ dsthi dstlo) (-~ srchi srclo)))
	 (offset (-~ dstlo (*~ scale srclo))))
    (madd in scale offset)))

(defun lin-lin.kr (in &optional (srclo 0.0) (srchi 1.0) (dstlo 1.0) (dsthi 2.0))
  (let* ((scale (/~ (-~ dsthi dstlo) (-~ srchi srclo)))
	 (offset (-~ dstlo (*~ scale srclo))))
    (+~ (*~ in scale) offset)))

(export '(lin-lin.ar lin-lin.kr))

(defun lin-lin (in in-min in-max out-min out-max &optional (clip :minmax))
  (if (numberp in) (let* ((result (case clip
				    (:minmax (cond ((<= in in-min) out-min)
						   ((>= in in-max) out-max)))
				    (:min (when (<= in in-min) out-min))
				    (:max (when (>= in in-max) out-max)))))
		     (if result result
		       (+ (* (/ (- in in-min) (- in-max in-min))
			     (- out-max out-min))
			  out-min)))
    (ecase (rate in)
      (:audio (lin-lin.ar (prune in in-min in-max clip) in-min in-max out-min out-max))
      (:control (lin-lin.kr (prune in in-min in-max clip) in-min in-max out-min out-max)))))


(defugen (lin-exp "LinExp")
    (&optional (in 0.0) (srclo 0.0) (srchi 1.0) (dstlo 1.0) (dsthi 2.0))
  ((:ar (multinew new 'pure-ugen in srclo srchi dstlo dsthi))
   (:kr (multinew new 'pure-ugen in srclo srchi dstlo dsthi)))
  :check-fn #'check-same-rate-as-first-input)

(defun lin-exp (in in-min in-max out-min out-max &optional (clip :minmax))
  (if (numberp in) (let* ((result (case clip
				    (:minmax (cond ((<= in in-min) out-min)
						   ((>= in in-max) out-max)))
				    (:min (when (<= in in-min) out-min))
				    (:max (when (>= in in-max) out-max)))))
		     (if result result
		       (* (expt (/ out-max out-min) (/ (- in in-min) (- in-max in-min)))
			  out-min)))
    (ecase (rate in)
      (:audio (lin-exp.ar (prune in in-min in-max clip) in-min in-max out-min out-max))
      (:control (lin-exp.kr (prune in in-min in-max clip) in-min in-max out-min out-max)))))

(defun exp-lin (in in-min in-max out-min out-max &optional (clip :minmax))
  (if (numberp in) (let* ((result (case clip
				    (:minmax (cond ((<= in in-min) out-min)
						   ((>= in in-max) out-max)))
				    (:min (when (<= in in-min) out-min))
				    (:max (when (>= in in-max) out-max)))))
		     (if result result
		       (+ (* (/ (log (/ in in-min)) (log (/ in-max in-min)))
			     (- out-max out-min))
			  out-min)))
    (+~ (*~ (/~ (log~ (/~ (prune in in-min in-max clip) in-min))
		(log~ (/~ in-max in-min)))
	    (-~ out-max out-min))
	out-min)))

(defun exp-exp (in in-min in-max out-min out-max &optional (clip :minmax))
  (if (numberp in) (let* ((result (case clip
				    (:minmax (cond ((<= in in-min) out-min)
						   ((>= in in-max) out-max)))
				    (:min (when (<= in in-min) out-min))
				    (:max (when (>= in in-max) out-max)))))
		     (if result result
		       (* (expt (/ out-max out-min) (/ (log (/ in in-min)) (log (/ in-max in-min))))
			  out-min)))
    (*~ (expt~ (/~ out-max out-min) (/~ (log~ (/~ (prune in in-min in-max clip) in-min))
					(log~ (/~ in-max in-min))))
	out-min)))

(defun lin-curve (in in-min in-max out-min out-max &optional (curve -4) (clip :minmax))
  (if (numberp in) (let* ((result (case clip
				    (:minmax (cond ((<= in in-min) out-min)
						   ((>= in in-max) out-max)))
				    (:min (when (<= in in-min) out-min))
				    (:max (when (>= in in-max) out-max)))))
		     (unless result
                       (when (< (abs curve) 0.001)
                         (setf result
                               (+ (* (/ (- in in-min) (- in-max in-min)) (- out-max out-min))
                                  out-min))))
                     (if result result
                         (let* ((grow (exp curve))
                                (a (/ (- out-max out-min) (- 1.0 grow)))
                                (b (+ out-min a))
                                (scaled (/ (- in in-min) (- in-max in-min))))
                           (- b (* a (expt grow scaled))))))
      (let* ((result (when (and (numberp curve) (< (abs curve) 0.125))
                           (lin-lin in in-min in-max out-min out-max clip))))
        (if result result
            (let* ((grow (exp~ curve))
                   (a (/~ (-~ out-max out-min) (-~ 1.0 grow)))
                   (b (+~ out-min a))
                   (scaled (/~ (-~ (prune in in-min in-max clip)
                                 in-min)
                              (-~ in-max in-min)))
                   (curved-result (-~ b (*~ a (expt~ grow scaled)))))
              (ecase (rate in)
                (:scalar curved-result)
                (:audio (select.ar (>=~ (abs~ curve) 0.125)
                                   (list (lin-lin in in-min in-max out-min out-max clip)
                                         curved-result)))
                (:control (select.kr (>=~ (abs~ curve) 0.125)
                                     (list (lin-lin in in-min in-max out-min out-max clip)
                                           curved-result)))))))))

(defun curve-lin (in in-min in-max out-min out-max &optional (curve -4) (clip :minmax))
  (if (numberp in) (let* ((result (case clip
				    (:minmax (cond ((<= in in-min) out-min)
						   ((>= in in-max) out-max)))
				    (:min (when (<= in in-min) out-min))
				    (:max (when (>= in in-max) out-max)))))
		     (unless result
                       (when (< (abs curve) 0.001)
                         (setf result
                               (+ (* (/ (- in in-min) (- in-max in-min)) (- out-max out-min))
                                  out-min))))
                     (if result result
                         (let* ((grow (exp curve))
                                (a (/ (- in-max in-min) (- 1.0 grow)))
                                (b (+ in-min a)))
                           (+ (/ (* (log (/ (- b in) a))
                                    (- out-max out-min))
                                 curve)
                              out-min))))
      (let* ((result (when (and (numberp curve) (< (abs curve) 0.125))
                           (lin-lin in in-min in-max out-min out-max clip))))
        (if result result
            (let* ((grow (exp~ curve))
                   (a (/~ (-~ in-max in-min) (-~ 1.0 grow)))
                   (b (+~ in-min a))
                   (lin-result (+~ (/~ (*~ (log~ (/~ (-~ b (prune in in-min in-max clip)) a))
                                           (-~ out-max out-min))
                                       curve)
                                   out-min)))
              (ecase (rate in)
                (:scalar lin-result)
                (:audio (select.ar (>=~ (abs~ curve) 0.125)
                                   (list (lin-lin in in-min in-max out-min out-max clip)
                                         lin-result)))
                (:control (select.kr (>=~ (abs~ curve) 0.125)
                                     (list (lin-lin in in-min in-max out-min out-max clip)
                                           lin-result)))))))))

(defun when-audio-check-first-input (ugen)
  (when (eql (rate ugen) :audio)
    (check-same-rate-as-first-input ugen)))

(defugen (amp-comp "AmpComp") (&optional (freq (midicps 60.0)) (root (midicps 60.0)) (exp 0.3333))
  ((:ir (multinew new 'pure-ugen freq root exp))
   (:ar (multinew new 'pure-ugen freq root exp))
   (:kr (multinew new 'pure-ugen freq root exp)))
  :check-fn #'when-audio-check-first-input )

(defugen (amp-comp-a "AmpCompA") (&optional (freq 1000) (root 0.0) (min-amp 0.32) (root-amp 1.0))
  ((:ir (multinew new 'pure-ugen freq root min-amp root-amp))
   (:ar (multinew new 'pure-ugen freq root min-amp root-amp))
   (:kr (multinew new 'pure-ugen freq root min-amp root-amp)))
  :check-fn #'when-audio-check-first-input)

(defugen (k2a "K2A") (&optional (in 0.0))
  ((:ar (multinew new 'pure-ugen in))))

(defugen (a2k "A2K") (&optional (in 0.0))
  ((:kr (multinew new 'pure-ugen in))))

(defugen (t2k "T2K") (&optional (in 0.0))
  ((:kr (multinew new 'pure-ugen in)))
  :check-fn #'when-audio-check-first-input)

(defugen (t2a "T2A") (&optional (in 0.0) (offset 0.0))
  ((:ar (multinew new 'pure-ugen in offset))))


(defugen (dc "DC") (&optional (in 0.0))
  ((:ar (multinew new 'multiout-ugen 1 in))
   (:kr (multinew new 'multiout-ugen 1 in))))

(defugen (silent "Silent") (&optional (chanls 1))
  ((:ar (let ((sig (dc.ar 0)))
	  new
	  (if (= chanls 1) sig (dup sig chanls))))))
