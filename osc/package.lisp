(defpackage #:sc-osc
  (:use #:cl)
  (:export
  	   #:osc-device
  	   #:send-message
  	   #:send-bundle
  	   #:close-device
	   #:debug-msg
  	   #:add-osc-responder
  	   #:remove-osc-responder))
