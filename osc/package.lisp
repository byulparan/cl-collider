
(defpackage #:osc
  (:use #:cl)
  (:export #:encode-message
	   #:encode-bundle
	   #:decode-message
	   #:decode-bundle

	   #:osc-device
	   #:send-message
	   #:send-bundle
	   #:close-device
	   
	   #:add-osc-responder
	   #:remove-osc-responder))
