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

### package: sc, sc-user(use this package)
### named-readtable: sc

## Usage:
### Server
```cl
(in-package :sc-user)

;; please check   *sc-synth-program*, *sc-plugin-paths*, *sc-synthdefs-path*
;; if you have different path then set to
;;
;; (setf *sc-synth-program* "/path/to/scsynth")
;; (setf *sc-plugin-paths* (list "/path/to/plugin_path" "/path/to/extension_plugin_path"))
;; (setf *sc-synthdefs-path* "/path/to/synthdefs_path")

(setf *s* (make-external-server "localhost" :port 48800))
				
(server-boot *s*)
;;....hack music.....
(server-quit *s*)
```

### Synth Definition
```cl	
(defsynth sine-wave (&key (note 60))
  (let* ((freq (midicps note))
         (sig (sin-osc.ar [freq (+ freq 2)] 0 .2)))
     (out.ar 0 sig)))

(defparameter *synth* (sine-wave))
(ctrl *synth* :note 72)
(free *synth*)
```
### Proxy
```cl
(proxy :sinesynth
  (sin-osc.ar [440 441] 0 .2))

(proxy :sinesynth
  (with-controls ((lfo-speed 4))
    (sin-osc.ar (* [440 441] (range (lf-pulse.ar [lfo-speed (+ lfo-speed .2)]) 0 1)) 0 .2))
  :fade 8.0)
   
(ctrl :sinesynth :lfo-speed 8)
(ctrl :sinesynth :gate 0)
```
### Make musical Sequence
```cl
(defsynth saw-synth (&key (note 60) (dur 4.0))
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
```
### Make Audiofile from Your Sequence
```cl
(setf *synth-definition-mode* :load)

;; re-define saw-synth. it's synthdef file write to *sc-synthdefs-path*.
(defsynth saw-synth (&key (note 60) (dur 4.0))
  (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
         (freq (midicps note))
         (sig (lpf.ar (saw.ar freq env) (* freq 2))))
    (out 0 [sig sig])))

;; redering audio-file.
(with-rendering ("~/Desktop/foo.aiff" :pad 60)
  (make-melody 0.0d0 32)
  (make-melody 8.0d0 32 12)
  (make-melody 16.0d0 32 24))
```
