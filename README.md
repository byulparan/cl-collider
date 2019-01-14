# cl-collider

A <a href="http://supercollider.github.io/">SuperCollider</a> client for <a href="https://www.common-lisp.net/">CommonLisp</a>.  
It is an experimental project, so changes to the API are possible.

## Videos:
- [tempo-clock on cl-collider](https://youtu.be/3Lo7yyZcSzU)   
- [cl-collider on Windows10](https://youtu.be/pCEfV4jOdUA)  
- [Tutorial](https://www.youtube.com/watch?v=JivNMDUqNQc) - Due to API changes, this video is deprecated. A new tutorial video is coming soon.  
- [Live Coding Demo 1](https://www.youtube.com/watch?v=xzTH_ZqaFKI)  
- [Live Coding Demo 2](https://www.youtube.com/watch?v=pZyuHjztARY)  

## Dependencies:

- [SuperCollider](http://supercollider.github.io) - compatible with 3.9 and 3.8.
- [Quicklisp](http://www.quicklisp.org)
- [ClozureCL](http://www.clozure.com/clozurecl.html) or [SBCL](http://www.sbcl.org) or [ECL](https://common-lisp.net/project/ecl/)

## Contrib:
If you have your own additional libraries, please report me. I will add here.

- [sc-extensions](https://github.com/byulparan/sc-extensions) - extension library
- [cl-patterns](https://github.com/defaultxr/cl-patterns) - patterns/sequencing library

## Usage:
- package: `sc`, `sc-user` (use this package)
- named-readtable: `sc`

```cl
(ql:quickload :cl-collider)

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

;; in Linux, maybe you need call this function
#+linux
(jack-connect)

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
;;; write a single channel to disk

;; we can write to buffer number out_buf_num by reading in from the 0 bus
(defsynth disk_writer ((out_buf_num 99))
    (disk-out.ar out_buf_num (in.ar 0)))

(setf mybuffer (buffer-alloc (expt 2 17))) 
mybuffer

;; start a disk_writer synth
(setf writer_0 (synth 'disk_writer))

;; make it output to buffer you allocated
(ctrl writer_0 :out_buf_num (bufnum mybuffer))

;; continuously write the buffer contents to a file
(buffer-write mybuffer "/tmp/foo.aiff" :leave-open-p t)

;; now play whatever sounds you like

;; e.g.
(proxy :blah (sin-osc.ar 440))
(free  :blah)


;; then when you are done

;; stop the disk_writer synth
(free writer_0)

;; close and free the buffer
(buffer-close mybuffer)
(buffer-free mybuffer)


;; then you can play what you recorded with a utility like mpv:
;;     mpv /tmp/foo.aiff
```
