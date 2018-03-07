# cl-collider

A <a href="http://supercollider.github.io/">SuperCollider</a> client for <a href="https://www.common-lisp.net/">CommonLisp</a>.  
It is an experimental project, so changes to the API are possible.

## Videos
### <a href="https://www.youtube.com/watch?v=JivNMDUqNQc">Tutorial</a>   
**Due to API changes, this video is deprecated. A new tutorial video is coming soon.**


### <a href="https://www.youtube.com/watch?v=xzTH_ZqaFKI">Live Coding Demo 1</a> 
### <a href="https://www.youtube.com/watch?v=pZyuHjztARY">Live Coding Demo 2</a>

## Dependencies:

- [SuperCollider](http://supercollider.github.io) - compatible with 3.9 and 3.8.
- [Quicklisp](http://www.quicklisp.org)
- [ClozureCL](http://www.clozure.com/clozurecl.html) or [SBCL](http://www.sbcl.org) or [ECL](https://common-lisp.net/project/ecl/)
- [Scheduler](http://github.com/byulparan/scheduler)(above 2017.3.14) - The time based task scheduler

## Usage:
- package: `sc`, `sc-user` (use this package)
- named-readtable: `sc`

```cl
(in-package :sc-user)
(named-readtables:in-readtable :sc)

;; please check   *sc-synth-program*, *sc-plugin-paths*, *sc-synthdefs-path*
;; if you have different path then set to
;;
;; (setf *sc-synth-program* "/path/to/scsynth")
;; (setf *sc-plugin-paths* (list "/path/to/plugin_path" "/path/to/extension_plugin_path"))
;; (setf *sc-synthdefs-path* "/path/to/synthdefs_path")

(setf *s* (make-external-server "localhost" :port 48800))
(server-boot *s*)

;; Hack music
(defvar *synth*)
(setf *synth* (play (sin-osc.ar [320 321] 0 .2)))

;; Stop music
(free *synth*)

;; Quit SuperCollider server
(server-quit *s*)
```

### Create SynthDef
```cl
(defsynth sine-wave ((note 60))
  (let* ((freq (midicps note))
         (sig (sin-osc.ar [freq (+ freq 2)] 0 .2)))
     (out.ar 0 sig)))

(setf *synth* (synth 'sine-wave))
(ctrl *synth* :note 72)
(free *synth*)
```

### Create Proxy
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
### Create Musical Sequence
```cl
(defsynth saw-synth ((note 60) (dur 4.0))
  (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
         (freq (midicps note))
    	 (sig (lpf.ar (saw.ar freq env) (* freq 2))))
	(out.ar 0 [sig sig])))

(defun make-melody (time n &optional (offset 0))
  (when (> n 0)
    (at time (synth 'saw-synth :note (+ offset (alexandria:random-elt '(62 65 69 72)))))
      (let ((next-time (+ time (alexandria:random-elt '(0 1 2 1.5)))))
        (callback next-time #'make-melody next-time (- n 1) offset))))

(make-melody (quant 4) 16)
(make-melody (+ 4 (quant 4)) 16 12)
```
### Record Audio Output
```cl
(setf *synth-definition-mode* :load)

;; Re-define the saw-synth ugen
;; The SynthDef file will be written to the *sc-synthdefs-path*
(defsynth saw-synth ((note 60) (dur 4.0))
  (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
         (freq (midicps note))
         (sig (lpf.ar (saw.ar freq env) (* freq 2))))
    (out 0 [sig sig])))

;; Render audio file
(with-rendering ("~/Desktop/foo.aiff" :pad 60)
  (make-melody 0.0d0 32)
  (make-melody 8.0d0 32 12)
  (make-melody 16.0d0 32 24))
```
