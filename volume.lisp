(in-package :sc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mater volume control for server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf volume) (new-volume (rt-server rt-server))
  (setf new-volume (alexandria:clamp new-volume -90.0 6.0))
  (let* ((new-amp (dbamp new-volume))
	 (is-playing-p (and (volume-control-synth rt-server)
			    (is-playing-p (volume-control-synth rt-server)))))
    (if (not (= new-amp 1.0))
	(if (not is-playing-p)
	    (let* ((num-output (server-options-num-output-bus (server-options rt-server)))
		   (*temp-synth-name* (format nil "volume-amp-control-~d" num-output)))
	      (setf (volume-control-synth rt-server)
		(play
		 (with-controls ((amp new-amp) (gate 1))
		   (x-out.ar 0 (linen.kr gate .01 1.0 .05 :act :free)
			     (* (in.ar 0 num-output) (lag.kr amp .1))))
		 :to 1 :pos :after)))
	  (ctrl (volume-control-synth rt-server) :amp new-amp))
      (when is-playing-p
	(ctrl (volume-control-synth rt-server) :amp 1.0 :gate 0)))
    (setf (slot-value rt-server 'volume) new-volume))
  rt-server)


(defun mute (rt-server)
  (let* ((prev-volume (volume rt-server)))
    (setf (volume rt-server) -90)
    (setf (slot-value rt-server 'volume) prev-volume)
    rt-server))


(defun unmute (rt-server)
  (setf (volume rt-server) (volume rt-server)))



