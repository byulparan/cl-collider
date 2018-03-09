
(eval-when (:compile-toplevel :load-toplevel :execute)
  (named-readtables:defreadtable :sc
    (:merge :common-lisp)))

(defpackage #:sc
  (:use #:cl)
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

	   #:make-server-options
	   #:server-options
	   #:server-options-block-size
	   
	   #:*synth-definition-mode*
	   #:defsynth
	   #:synth
       #:get-synthdef-metadata
	   #:with-controls
	   #:play
	   #:proxy
	   #:clip
	   #:fold
       #:wrap
	   #:range
       #:exp-range
	   #:lin-lin
       #:lin-exp
       #:exp-lin
       #:exp-exp

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
	   #:free
       #:release
	   #:ctrl
       #:map-bus
	   #:is-playing-p

	   #:make-group
	   #:server-query-all-nodes
	   #:group-free-all
	   #:stop
	   #:server-status
	   #:*group-free-all-hook*
	   #:*stop-hook*

	   #:bufnum
	   #:sr
	   #:frames
	   #:chanls
	   #:buffer-read
       #:buffer-read-channel
	   #:buffer-alloc
	   #:buffer-free
	   #:buffer-save
	   #:buffer-get
	   #:buffer-get-list
	   #:buffer-set
	   #:buffer-set-list
	   #:buffer-zero
       #:buffer-dur
	   #:wavetable
	   #:calc-pv-recsize
	   #:local-buf
	   #:set-buf
	   #:clear-buf
	   #:local-buf-list

       #:bus-audio
       #:bus-control
       #:bus-free
       #:bus-string
       #:busnum

	   #:neg
	   #:reciprocal
	   #:frac
	   #:sign
	   #:squared
	   #:cubed
	   #:midicps
	   #:cpsmidi
	   #:midiratio
	   #:dbamp
	   #:ampdb
	   #:distort
       #:softclip
	   #:trunc
	   #:fold2
	   #:madd
	   #:mix
	   #:sum
	   #:bubble
	   #:unbubble
	   #:flop
	   #:clump
	   #:mean
	   #:product
	   #:dup
	   #:==
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

(defpackage #:sc-user
  (:use #:cl #:sc))

(in-package :sc-user)
(named-readtables:in-readtable :sc)


