# cl-collider
SuperCollider client for CommonLisp.  
It support ClozureCL and SBCL.  

tutorial video: <http://youtu.be/JivNMDUqNQc>  
live coding demo: <https://www.youtube.com/watch?v=xzTH_ZqaFKI>  

### require:
- [SuperCollider](http://supercollider.sourceforge.net) - I tested on latest stable version(3.6.5)
- [Quicklisp](http://www.quicklisp.org)
- [ClozureCL](http://www.clozure.com/clozurecl.html) or [SBCL](http://www.sbcl.org)
- [Scheduler](http://github.com/byulparan/scheduler) - The time based task scheduler
- [Simple-Utils](http://github.com/byulparan/Simple-Utils) - The Collection of simple-functions
- [Main-Thread](http://github.com/byulparan/main-thread) - Thread-Utility

## Usage:
### Server

	(in-package :sc)
	(setf *sc-synth-program* "...")  ;; scsynth program path.
	(push "..." *sc-plugin-paths*)   ;; scx file's path.
	(push "..." *sc-plugin-paths*)   ;; it support extensions
	(setf *sc-synthdefs-path* "...") ;; your synthdef file will write here
	
	(defparameter *s* (make-external-server "localhost" :port ... :just-connect-p nil))
	(server-boot *s*)
	;;....hack music.....
	(server-quit *s*)

### Synth Definition
	
	(defsynth sine-wave ((note 60))
	  (let* ((freq (midicps note))
	            (sig (sin-osc.ar [freq (+ freq 2)] 0 .2)))
        (out.ar 0 sig)))

	(defparameter *synth* (sine-wave))
	(ctrl *synth* :note 72)
	(bye *synth*)

### Proxy
	(proxy :sinesynth
	   (sin-osc.ar [440 441] 0 .2))

	(proxy :sinesynth
	   (with-controls ((lfo-speed 4))
          (sin-osc.ar (* [440 441] (range (lf-pulse.ar [lfo-speed (+ lfo-speed .2)]) 0 1)) 0 .2))
	   :fade-time 8.0)

	(ctrl (proxy :sinesynth) :lfo-speed 8)
	(ctrl (proxy :sinesynth) :gate 0)

### Make musical Sequence

	(defsynth saw-synth ((note 60) (dur 4.0))
	   (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
	             (freq (midicps note))
	    		 (sig (lpf.ar (saw.ar freq env) (* freq 2))))
		  (out.ar 0 [sig sig])))

	(defun make-melody (time n &optional (offset 0))
	   (when (> n 0)
          (at time (saw-synth :note (+ offset (alexandria:random-elt '(62 65 69 72)))))
          (let ((next-time (+ time (alexandria:random-elt '(0 1 2 1.5)))))
            (callback next-time #'make-melody next-time (- n 1) offset))))

	(make-melody (quant 4) 16)
	(make-melody (+ 4 (quant 4)) 16 12)

### Make Audiofile from Your Sequence
	(setf *synth-definition-mode* :load)

	;; re-define saw-synth. it's synthdef file write to *sc-synthdefs-path*.
	(defsynth saw-synth ((note 60) (dur 4.0))
	   (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
	             (freq (midicps note))
                 (sig (lpf.ar (saw.ar freq env) (* freq 2))))
		  (out.ar 0 [sig sig])))

	;; redering audio-file.
	(with-rendering ("~/Desktop/foo.aiff" :pad 60)
	  (make-melody 0.0d0 32)
      (make-melody 8.0d0 32 12)
      (make-melody 16.0d0 32 24))
