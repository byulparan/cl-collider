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
	       ;; swank-extensions.lisp/slynk-extensions.lisp conditionally loaded at the end of util.lisp
	       (:file "server-options")
	       (:file "scheduler")
	       (:file "server")
	       (:file "buffer")
	       (:file "bus")
	       (:file "ugen")
	       (:file "synthdef")
	       (:file "operators")
	       (:file "ugens/BEQSuite")
	       (:file "ugens/bufio")
	       (:file "ugens/chaos")
	       (:file "ugens/compander")
	       (:file "ugens/delay")
	       (:file "ugens/demands")
	       (:file "ugens/diskio")
	       (:file "ugens/envgen")
	       (:file "ugens/fft")
	       (:file "ugens/fftunpacking")
	       (:file "ugens/Filter")
	       (:file "ugens/freeverb")
	       (:file "ugens/hilbert")
	       (:file "ugens/ienvgen")
	       (:file "ugens/io")
	       (:file "ugens/infougens")
	       (:file "ugens/line")
	       (:file "ugens/macugens")
	       (:file "ugens/machine-listening")
	       (:file "ugens/MoogFF")
	       (:file "ugens/noise")
	       (:file "ugens/osc")
	       (:file "ugens/pan")
	       (:file "ugens/pluck")
	       (:file "ugens/poll")
	       (:file "ugens/SoundIn")
	       (:file "ugens/splay")
	       (:file "ugens/pmosc")
	       (:file "ugens/testugens")
	       (:file "ugens/trig")
	       (:file "ugens/distortion")
	       (:file "ugens/math")
	       (:file "ugens/grains")
	       (:file "ugens/stochastic")
	       (:file "ugens/extras/envfollow")
	       (:file "ugens/extras/mdapiano")
	       (:file "ugens/extras/joshpv")
	       (:file "ugens/extras/mcldbufferugens")
	       (:file "ugens/extras/pitchdetection")
	       (:file "ugens/extras/ladspa")))
