# cl-collider
A <a href="http://supercollider.github.io/">SuperCollider</a> client for <a href="https://www.common-lisp.net/">CommonLisp</a>.  
It supports the <a href="http://sbcl.org/">SBCL</a> & <a href="http://ccl.clozure.com/">ClozureCL</a> compilers.  
It is an experimental project, so changes to the API are possible.
## Videos
### <a href="https://www.youtube.com/watch?v=JivNMDUqNQc">Tutorial</a>   
**Due to API changes, this video is deprecated. A new tutorial video is coming soon.**

### <a href="https://www.youtube.com/watch?v=xzTH_ZqaFKI">Live Coding Demo 1</a> 

### <a href="https://www.youtube.com/watch?v=pZyuHjztARY">Live Coding Demo 2</a>

## Dependencies:
- [SuperCollider](http://supercollider.github.io)
- [Quicklisp](https://www.quicklisp.org/beta/)
- [Scheduler](https://github.com/byulparan/scheduler)
- [Simple-Utils](https://github.com/byulparan/Simple-Utils)
- [SBCL](http://www.sbcl.org) **or** [ClozureCL](http://ccl.clozure.com/)

## Usage:
### Server
#### GNU/Linux
```cl
(in-package :sc)

;; Scsynth executable path
(setf *sc-synth-program* "/usr/local/bin/scsynth")

;; SuperCollider plugins path
(push "/usr/local/lib/SuperCollider/plugins/" *sc-plugin-paths*)

;; Import extensions to *sc-plugins-paths*
;; (eg. SC3 Plugins)
(push "/usr/local/share/SuperCollider/Extensions/SC3plugins/" *sc-plugin-paths*) 

;; Local synthdef directory
(setf *sc-synthdefs-path* "~/.local/share/SuperCollider/synthdefs/")

;; Start external server on a port
;; (eg. port 57999)
(defparameter *s* (make-external-server "localhost" :port 57999))

;; Boot SuperCollider server
(server-boot *s*)

;; Hack music
(play (sin-osc [320 321] 0 .2))

;; Stop music
(bye *)

;; Quit SuperCollider server
(server-quit *s*)
```
#### OSX
```cl
(in-package :sc)

;; Scsynth executable path
(setf *sc-synth-program* "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth")

;; SuperCollider plugins path
(push "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/plugins/" *sc-plugin-paths*)

;; Import extensions to *sc-plugins-paths*
;; (eg. SC3 Plugins)
(push "~/Library/Application Support/SuperCollider/Extensions/SC3plugins/" *sc-plugin-paths*)

;; Local synthdef directory
(setf *sc-synthdefs-path* "~/Library/Application Support/SuperCollider/synthdefs/")

;; Start external server on a port
;; (eg. port 57999)
(defparameter *s* (make-external-server "localhost" :port 57999))

;; Boot SuperCollider server
(server-boot *s*)

;; Hack music
(play (sin-osc [320 321] 0 .2))

;; Stop music
(bye *)

;; Quit SuperCollider server
(server-quit *s*)
```

### Create SynthDef
```cl
(defsynth sine-wave (&key (note 60))
  (let* ((freq (midicps note))
         (sig (sin-osc [freq (+ freq 2)] 0 .2)))
     (out 0 sig)))

(defparameter *synth* (sine-wave))
(ctrl *synth* :note 72)
(bye *synth*)
```

### Create Proxy
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
### Create Musical Sequence
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
### Record Audio Output
```cl
(setf *synth-definition-mode* :load)

;; Re-define the saw-synth ugen
;; The SynthDef file will be written to the *sc-synthdefs-path*
(defsynth saw-synth (&key (note 60) (dur 4.0))
   (let* ((env (env-gen.kr (env [0 .2 0] [(* dur .2) (* dur .8)]) :act :free))
             (freq (midicps note))
                (sig (lpf (saw freq env) (* freq 2))))
	  (out 0 [sig sig])))

;; Render audio file
(with-rendering ("~/Desktop/foo.aiff" :pad 60)
  (make-melody 0.0d0 32)
     (make-melody 8.0d0 32 12)
     (make-melody 16.0d0 32 24))
```
