
(asdf/defsystem:defsystem :osc-lib
  :serial t
  :depends-on (#:osc #:alexandria #:ieee-floats #:bordeaux-threads #:usocket #:chanl)
  :components ((:file "package")
	       (:file "osc")
	       (:file "transmit")))
