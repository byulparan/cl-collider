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
	       (:file "ugens/Delays")
	       (:file "ugens/Demand")
	       (:file "ugens/DiskIO")
	       (:file "ugens/EnvGen")
	       (:file "ugens/FFT")
	       (:file "ugens/FFTUnpacking")
	       (:file "ugens/FSinOsc")
	       (:file "ugens/Filter")
	       (:file "ugens/FreeVerb")
	       (:file "ugens/GVerb")
	       (:file "ugens/Gendyn")
	       (:file "ugens/GrainUGens")
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
	       (:file "ugens/PSinGrain")
	       (:file "ugens/Pan")
	       (:file "ugens/PhysicalModel")
	       (:file "ugens/PitchShift")
	       (:file "ugens/Pluck")
	       (:file "ugens/Poll")
	       (:file "ugens/SoundIn")
	       (:file "ugens/Splay")
	       (:file "ugens/TestUGens")
	       (:file "ugens/Trig")
	       (:file "ugens/sc3-plugins/envfollow")
	       (:file "ugens/sc3-plugins/mdapiano")
	       (:file "ugens/sc3-plugins/joshpv")
	       (:file "ugens/sc3-plugins/mcldbufferugens")
	       (:file "ugens/sc3-plugins/pitchdetection")
	       (:file "ugens/sc3-plugins/ladspa")
	       (:file "ugens/sc3-plugins/DistortionPlugins")))
