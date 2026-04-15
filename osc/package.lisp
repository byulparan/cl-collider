(defpackage #:sc-osc
  (:use #:cl)
  (:export
  	   #:osc-device
  	   #:send-message
  	   #:send-bundle
  	   #:close-device
	   #:osc-trace
  	   #:add-osc-responder
  	   #:remove-osc-responder))
