
(in-package #:sc)

(defugen (line "Line")
    (&optional (start 0.0) (end 1.0) (dur 1.0) &key (mul 1.0) (add 0.0) (act :no-action))
  ((:ar (madd (multinew new 'ugen start end dur (act act)) mul add))
   (:kr (madd (multinew new 'ugen start end dur (act act)) mul add))))

(defugen (x-line "XLine")
    (&optional (start 1.0) (end 2.0) (dur 1.0) &key (mul 1.0) (add 0.0) (act :no-action))
  ((:ar (madd (multinew new 'ugen start end dur (act act)) mul add))
   (:kr (madd (multinew new 'ugen start end dur (act act)) mul add))))

(defugen (lin-exp "LinExp")
    (&optional (in 0.0) (src-lo 0.0) (src-hi 1.0) (dst-lo 1.0) (dst-hi 2.0))
  ((:ar (multinew new 'pure-ugen in src-lo src-hi dst-lo dst-hi))
   (:kr (multinew new 'pure-ugen in src-lo src-hi dst-lo dst-hi)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (lin-lin-ugen "LinLin") (&optional (in 0.0) (src-lo 0.0) (src-hi 1.0) (dst-lo 1.0) (dst-hi 2.0))
  ((:ar  (let* ((scale (divide (minus dst-hi dst-lo) (minus src-hi src-lo)))
	       (offset (minus dst-lo (mul scale src-lo))))
	   new
	  (madd in scale offset)))
   (:kr (let* ((scale (divide (minus dst-hi dst-lo) (minus src-hi src-lo)))
	       (offset (minus dst-lo (mul scale src-lo))))
	  new
	  (add (mul in scale) offset)))))

(defun lin-lin (ugen in-min in-max out-min out-max &optional (clip :minmax))
  (if (eql (rate ugen) :audio) (lin-lin-ugen (prune ugen in-min in-max clip) in-min in-max out-min out-max)
      (lin-lin-ugen.kr (prune ugen in-min in-max clip) in-min in-max out-min out-max)))

(unexport '(lin-lin-ugen lin-lin-ugen.kr))


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
  ((:ar (let ((sig (dc 0)))
	  new
	  (if (= chanls 1) sig (su:dup sig chanls))))))
