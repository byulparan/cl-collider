
(in-package #:sc)

(defun splay-new (rate spread level center level-comp &rest in-array)
  (let* ((n (max 2 (length in-array)))
	 (n1 (- n 1))
	 (position (+~
		    (*~ 
		     (-~ (*~ (loop for i from 0 to n1 collect i)
			     (/~ 2 n1))
			 1)
		     spread) center)))
    (when level-comp
      (if (eql rate :audio)
	  (setf level (*~ level (sqrt~ (reciprocal n))))
	  (setf level (/~ level n))))
    (if (eql rate :audio)
	(*~ (sum (pan2.ar in-array position)) level)
	(*~ (sum (pan2.kr in-array position)) level))))

(defugen (splay "Splay") (in-array &optional (spread 1) (level 1) (center 0.0) (level-comp t))
  ((:ar
    (declare (ignore new))
    (apply #'multinew #'splay-new :audio spread level center level-comp (alexandria:ensure-list in-array)))
   (:kr
    (declare (ignore new))
    (apply #'multinew #'splay-new :control spread level center level-comp (alexandria:ensure-list in-array)))))

(defun splay-fill (n function &optional (spread 1) (level 1) (center 0.0) (level-comp t))
  (splay.ar (dup function n) spread level center level-comp))

(export 'splay-fill)

