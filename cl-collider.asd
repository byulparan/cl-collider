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
	       (:file "ugens/BufIO")
	       (:file "ugens/Chaos")
	       (:file "ugens/Compander")
	       (:file "ugens/Delay")
	       (:file "ugens/Demands")
	       (:file "ugens/DiskIO")
	       (:file "ugens/EnvGen")
	       (:file "ugens/FFT")
	       (:file "ugens/FFTUnpacking")
	       (:file "ugens/Filter")
	       (:file "ugens/FreeVerb")
	       (:file "ugens/Hilbert")
	       (:file "ugens/IEnvGen")
	       (:file "ugens/InOut")
	       (:file "ugens/InfoUGens")
	       (:file "ugens/Line")
	       (:file "ugens/MacUGens")
	       (:file "ugens/MachineListening")
	       (:file "ugens/MoogFF")
	       (:file "ugens/Noise")
	       (:file "ugens/Osc")
	       (:file "ugens/Pan")
	       (:file "ugens/Pluck")
	       (:file "ugens/Poll")
	       (:file "ugens/SoundIn")
	       (:file "ugens/Splay")
	       (:file "ugens/TestUGens")
	       (:file "ugens/Trig")
	       (:file "ugens/math")
	       (:file "ugens/grains")
	       (:file "ugens/stochastic")
	       (:file "ugens/extras/envfollow")
	       (:file "ugens/extras/mdapiano")
	       (:file "ugens/extras/joshpv")
	       (:file "ugens/extras/mcldbufferugens")
	       (:file "ugens/extras/pitchdetection")
	       (:file "ugens/extras/ladspa")
	       (:file "ugens/extras/DistortionPlugins")))
