(asdf/defsystem:defsystem :sc-osc
  :serial t
  :depends-on (#:osc #:alexandria #:ieee-floats #:bordeaux-threads #:usocket)
  :components ((:file "package")
	       (:file "osc")
	       (:file "transmit")
	       #+ecl (:file "ecl-extension")))
