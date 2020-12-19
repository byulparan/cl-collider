
(in-package #:sc)


(defugen (amplitude "Amplitude")
    (&optional (in 0.0) &key (attack-time 0.01) (release-time 0.01) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen in attack-time release-time) mul add))
   (:kr (madd (multinew new 'ugen in attack-time release-time) mul add))))


(defugen (compander "Compander")
    (&optional (in 0.0) (control 0.0) (thresh 0.5) (slope-below 1.0)
	       (slope-above 1.0) (clamp-time 0.01) (relax-time 0.1) &key
	       (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen in control thresh slope-below slope-above clamp-time
			relax-time) mul add))))

(defun compander-d.ar (&optional (in 0.0) (thresh 0.5) (slope-below 1.0) (slope-above 1.0)
					    (clamp-time 0.01) (relax-time 0.01) &key (mul 1.0) (add 0.0))
  (madd (compander.ar (delay-n.ar in clamp-time clamp-time) in
		      thresh slope-below slope-above clamp-time relax-time) mul add))

(export 'compander-d.ar)

(defugen (normalizer "Normalizer")
    (&optional (in 0.0) (level 1.0) (dur 0.01))
  ((:ar (multinew new 'ugen in level dur))))

(defugen (limiter "Limiter")
    (&optional (in 0.0) (level 1.0) (dur 0.01))
  ((:ar (multinew new 'ugen in level dur))))

