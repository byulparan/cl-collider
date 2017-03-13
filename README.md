# cl-collider
SuperCollider client for CommonLisp.  
It support ClozureCL / SBCL / ECL.  

tutorial video: <http://youtu.be/JivNMDUqNQc>   
**This video deprecated. because some API Changed. I will reproduce to tutorial video asap**     

live coding demo: <https://www.youtube.com/watch?v=xzTH_ZqaFKI>  

### Status:
cl-collider is my experimental project. so It possible to change API

### version: 2017.3.14

### require:

please check version of dependency library.

- [SuperCollider](http://supercollider.sourceforge.net) - I tested on latest stable version(3.8.0)
- [Quicklisp](http://www.quicklisp.org)
- [ClozureCL](http://www.clozure.com/clozurecl.html) or [SBCL](http://www.sbcl.org) or [ECL](https://common-lisp.net/project/ecl/)
- [Scheduler](http://github.com/byulparan/scheduler)(above 2017.3.14) - The time based task scheduler

### package: sc

## Usage:
### Server
```cl
(in-package :sc)
(setf *sc-synth-program* "...")  ;; scsynth program path.
(push "..." *sc-plugin-paths*)   ;; scx file's path.
(push "..." *sc-plugin-paths*)   ;; it support extensions
(setf *sc-synthdefs-path* "...") ;; your synthdef file will write here

(setf *s* (make-external-server "localhost"
				:server-options
				(make-server-options
				 :realtime-mem-size (expt 2 14) ;; set your server option
				 :ugen-plugins-path (mapcar #'full-pathname *sc-plugin-paths*))
				:port 48800))
				
(server-boot *s*)
;;....hack music.....
(server-quit *s*)
```
### Synth Definition
```cl	
(defsynth sine-wave (&key (note 60))
  (let* ((freq (midicps note))
         (sig (sin-osc [freq (+ freq 2)] 0 .2)))
     (out 0 sig)))

(defparameter *synth* (sine-wave))
(ctrl *synth* :note 72)
(bye *synth*)
```
### Proxy
```cl
(proxy :sinesynth
   (sin-osc [440 441] 0 .2))

(proxy :sinesynth
   (with-controls ((lfo-speed 4))
         (sin-osc (* [440 441] (range (lf-pulse [lfo-speed (+ lfo-speed .2)]) 0 1)) 0 .2))
   :fade-time 8.0)
   
(ctrl (proxy :sinesynth) :lfo-speed 8)
(ctrl (proxy :sinesynth) :gate 0)
```
### Make musical Sequence
```cl
(defsynth saw-synth (&key (note 60) (dur 4.0))
   (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
          (freq (midicps note))
     	  (sig (lpf (saw freq env) (* freq 2))))
	  (out 0 [sig sig])))

(defun make-melody (time n &optional (offset 0))
   (when (> n 0)
      (at time (saw-synth :note (+ offset (alexandria:random-elt '(62 65 69 72)))))
      (let ((next-time (+ time (alexandria:random-elt '(0 1 2 1.5)))))
        (callback next-time #'make-melody next-time (- n 1) offset))))

(make-melody (quant 4) 16)
(make-melody (+ 4 (quant 4)) 16 12)
```
### Make Audiofile from Your Sequence
```cl
(setf *synth-definition-mode* :load)

;; re-define saw-synth. it's synthdef file write to *sc-synthdefs-path*.
(defsynth saw-synth (&key (note 60) (dur 4.0))
   (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
          (freq (midicps note))
          (sig (lpf (saw freq env) (* freq 2))))
	  (out 0 [sig sig])))

;; redering audio-file.
(with-rendering ("~/Desktop/foo.aiff" :pad 60)
  (make-melody 0.0d0 32)
  (make-melody 8.0d0 32 12)
  (make-melody 16.0d0 32 24))
```
