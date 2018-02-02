(asdf:defsystem #:sc
  :name "cl-collider"
  :author "Park Sungmin. byulparan@icloud.com"
  :description "SuperCollider client for Common Lisp"
  :version "2017.3.15"
  :depends-on (#:scheduler
	       #:sc-osc
	       #:alexandria
	       #:bordeaux-threads
	       #:flexi-streams
	       #:split-sequence
	       #:named-readtables)
  :serial t
  :components ((:file "package")
	       #+(or sbcl ecl) (:file "id-map")
	       (:file "util")
	       (:file "server-options")
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
	       (:file "ugens/noise")
	       (:file "ugens/filter")
	       (:file "ugens/osc")
	       (:file "ugens/pmosc")
	       (:file "ugens/trig")
	       (:file "ugens/infougens")
	       (:file "ugens/bufio")
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
	       (:file "ugens/extras/envfollow")
	       (:file "ugens/extras/mdapiano")
	       (:file "ugens/extras/joshpv")
	       (:file "ugens/extras/pitchdetection")))
