
(asdf/defsystem:defsystem :osc-ext
  :serial t
  :depends-on (#:osc #:alexandria #:ieee-floats)
  :components ((:file "osc")))
