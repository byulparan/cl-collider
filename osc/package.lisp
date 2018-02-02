(defpackage #:sc-osc
  (:use #:cl)
  (:export  
  	   #:osc-device
  	   #:send-message
  	   #:send-bundle
  	   #:close-device
	   
  	   #:add-osc-responder
  	   #:remove-osc-responder))
