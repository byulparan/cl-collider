(asdf:defsystem #:cl-collider
  :name "cl-collider"
  :author "Park Sungmin. byulparan@gmail.com"
  :description "SuperCollider client for Common Lisp"
  :licence "Public Domain / 0-clause MIT"
  :version "2018.7.15"
  :depends-on (#:sc-osc
	       #:alexandria
	       #:cffi
	       #:bordeaux-threads
	       #:pileup
	       #:flexi-streams
	       #:split-sequence
	       #:named-readtables
	       #-lispworks #:simple-inferiors
	       #:cl-ppcre)
  :serial t
  :components ((:file "package")
	       #-ccl (:file "id-map")
	       (:file "util")
	       ;; swank-extensions.lisp conditionally loaded at the end of util.lisp
	       (:file "server-options")
	       (:file "scheduler")
	       (:file "server")
	       (:file "buffer")
	       (:file "bus")
	       (:file "ugen")
	       (:file "synthdef")
	       (:file "operators")
	       (:file "ugens/line")
	       (:file "ugens/pan")
	       (:file "ugens/fftunpacking")
	       (:file "ugens/fft")
	       (:file "ugens/envgen")
	       (:file "ugens/ienvgen")
	       (:file "ugens/noise")
	       (:file "ugens/filter")
	       (:file "ugens/osc")
	       (:file "ugens/pmosc")
	       (:file "ugens/trig")
	       (:file "ugens/infougens")
	       (:file "ugens/bufio")
	       (:file "ugens/diskio")
	       (:file "ugens/io")
	       (:file "ugens/audioin")
	       (:file "ugens/chaos")
	       (:file "ugens/delay")
	       (:file "ugens/compander")
	       (:file "ugens/demands")
	       (:file "ugens/distortion")
	       (:file "ugens/pluck")
	       (:file "ugens/splay")
	       (:file "ugens/macugens")
	       (:file "ugens/freeverb")
	       (:file "ugens/math")
	       (:file "ugens/grains")
	       (:file "ugens/poll")
	       (:file "ugens/stochastic")
	       (:file "ugens/testugens")
	       (:file "ugens/hilbert")
	       (:file "ugens/machine-listening")
	       (:file "ugens/extras/envfollow")
	       (:file "ugens/extras/mdapiano")
	       (:file "ugens/extras/joshpv")
	       (:file "ugens/extras/mcldbufferugens")
	       (:file "ugens/extras/pitchdetection")
	       (:file "ugens/extras/ladspa")))
