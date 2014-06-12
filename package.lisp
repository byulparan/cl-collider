
(defpackage #:sc
  (:use #:cl)
  (:import-from :cb #:now #:callback #:quant)
  #+ccl (:import-from :ccl #:make-id-map #:assign-id-map-id #:id-map-free-object)
  #+sbcl (:lock t)
  (:export #:*s*
	   #:*sc-synth-program*
	   #:*sc-plugin-paths*
	   #:*sc-synthdefs-path*
	   #:+inf+
	   #:now
	   #:callback
	   #:quant
	   
	   #:*synth-definition-mode*
	   #:defsynth
	   #:with-controls
	   #:play
	   #:proxy
	   #:clip
	   #:fold
	   #:range
	   #:lin-lin
	   #:wrap

	   #:with-rendering

	   #:all-running-servers
	   #:reply-log-p
	   #:make-external-server
	   #:server-boot
	   #:server-quit
	   #:send-message
	   #:send-bundle
	   #:addr
	   #:lisp-port

	   #:control-get
	   #:control-set
	   #:add-reply-responder
	   #:remove-reply-responder
	   #:sync
	   
	   #:at
	   #:bye
	   #:ctrl
	   #:is-playing-p

	   #:make-group
	   #:server-query-all-nodes
	   #:group-free-all
	   #:stop
	   #:server-status
	   #:set-hook-group-free-all

	   #:bufnum
	   #:frames
	   #:chanls
	   #:buffer-read
	   #:buffer-alloc
	   #:buffer-save
	   #:buffer-get
	   #:buffer-get-list
	   #:buffer-set
	   #:buffer-set-list
	   #:b-cheby-msg
	   #:calc-pv-recsize
	   #:local-buf
	   #:set-buf
	   #:clear-buf
	   #:local-buf-list

	   #:neg
	   #:reciprocal
	   #:frac
	   #:sign
	   #:squared
	   #:cubed
	   #:midicps
	   #:dbamp
	   #:ampdb
	   #:distort
	   #:trunc
	   #:fold2
	   #:madd
	   #:mix
	   #:sum
	   #:bubble
	   #:unbubble
	   #:flop
	   #:clump
	   #:product

	   #:clip2
	   
	   #:env-shape-number
	   #:env
	   #:triangle
	   #:sine
	   #:perc
	   #:linen
	   #:cutoff
	   #:dadsr
	   #:adsr
	   #:asr

	   #:fft
	   #:ifft
	   #:pvcalc
	   #:pvcalc2
	   #:pv-collect))

