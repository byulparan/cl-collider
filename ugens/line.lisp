
(in-package #:sc)

(defugen (line "Line")
    (&optional (start 0.0) (end 1.0) (dur 1.0) &key (mul 1.0) (add 0.0) (act :no-action))
  ((:ar (madd (multinew new 'ugen start end dur (act act)) mul add))
   (:kr (madd (multinew new 'ugen start end dur (act act)) mul add))))

(defugen (x-line "XLine")
    (&optional (start 1.0) (end 2.0) (dur 1.0) &key (mul 1.0) (add 0.0) (act :no-action))
  ((:ar (madd (multinew new 'ugen start end dur (act act)) mul add))
   (:kr (madd (multinew new 'ugen start end dur (act act)) mul add))))

(defugen (lin-lin-ugen "LinLin")
    (&optional (in 0.0) (in-min 0.0) (in-max 1.0) (out-min 1.0) (out-max 2.0))
  ((:ir (+~ (*~ (/~ (-~ in in-min) (-~ in-max in-min)) (-~ out-max out-min)) out-min))
   (:ar (multinew new 'pure-ugen in in-min in-max out-min out-max))
   (:kr (multinew new 'pure-ugen in in-min in-max out-min out-max)))
  :check-fn #'check-same-rate-as-first-input)

(defun lin-lin (in in-min in-max out-min out-max &optional (clip :minmax))
  (let ((in (prune in in-min in-max clip)))
    (funcall (case (rate (list in in-min in-max out-min out-max))
               (:scalar #'lin-lin-ugen.ir)
               (:audio #'lin-lin-ugen.ar)
               (:control #'lin-lin-ugen.kr))
             in in-min in-max out-min out-max)))

(unexport '(lin-lin-ugen.ar lin-lin-ugen.kr lin-lin-ugen.ir))


(defugen (lin-exp-ugen "LinExp")
    (&optional (in 0.0) (in-min 0.0) (in-max 1.0) (out-min 1.0) (out-max 2.0))
  ((:ir (*~ (expt~ (/~ out-max out-min) (/~ (-~ in in-min) (-~ in-max in-min))) out-min))
   (:ar (multinew new 'pure-ugen in in-min in-max out-min out-max))
   (:kr (multinew new 'pure-ugen in in-min in-max out-min out-max)))
  :check-fn #'check-same-rate-as-first-input)

(defun lin-exp (in in-min in-max out-min out-max &optional (clip :minmax))
  (let* ((in (prune in in-min in-max clip))
         (rate (rate (list in in-min in-max out-min out-max))))
    (funcall (case rate
               (:audio #'lin-exp-ugen.ar)
               (:control #'lin-exp-ugen.kr)
               (:scalar #'lin-exp-ugen.ir))
             in in-min in-max out-min out-max)))

(unexport '(lin-exp-ugen.ar lin-exp-ugen.kr lin-exp-ugen.ir))


(defugen (exp-lin-ugen "ExpLin")
    (&optional (in 0.0) (in-min 1.0) (in-max 2.0) (out-min 0.0) (out-max 1.0))
  ((:ir (+~ (*~ (/~ (log~ (/~ in in-min)) (log~ (/~ in-max in-min))) (-~ out-max out-min)) out-min))
   (:ar (multinew new 'pure-ugen in in-min in-max out-min out-max))
   (:kr (multinew new 'pure-ugen in in-min in-max out-min out-max)))
  :check-fn #'check-same-rate-as-first-input)

(defun exp-lin (in in-min in-max out-min out-max &optional (clip :minmax))
  (let* ((in (prune in in-min in-max clip))
         (rate (rate (list in in-min in-max out-min out-max))))
    (funcall (case rate
               (:audio #'exp-lin-ugen.ar)
               (:control #'exp-lin-ugen.kr)
               (:scalar #'exp-lin-ugen.ir))
             in in-min in-max out-min out-max)))

(unexport '(exp-lin-ugen.ar exp-lin-ugen.kr exp-lin-ugen.ir))


(defugen (exp-exp-ugen "ExpExp")
    (&optional (in 0.0) (in-min 1.0) (in-max 2.0) (out-min 1.0) (out-max 2.0))
  ((:ir (*~ (expt~ (/~ out-max out-min) (/~ (log~ (/~ in in-min)) (log~ (/~ in-max in-min)))) out-min))
   (:ar (multinew new 'pure-ugen in in-min in-max out-min out-max))
   (:kr (multinew new 'pure-ugen in in-min in-max out-min out-max)))
  :check-fn #'check-same-rate-as-first-input)

(defun exp-exp (in in-min in-max out-min out-max &optional (clip :minmax))
  (let* ((in (prune in in-min in-max clip))
         (rate (rate (list in in-min in-max out-min out-max))))
    (funcall (case rate
               (:audio #'exp-exp-ugen.ar)
               (:control #'exp-exp-ugen.kr)
               (:scalar #'exp-exp-ugen.ir))
             in in-min in-max out-min out-max)))

(unexport '(exp-exp-ugen.ar exp-exp-ugen.kr exp-exp-ugen.ir))


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
