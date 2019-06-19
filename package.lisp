(eval-when (:compile-toplevel :load-toplevel :execute)
  (named-readtables:defreadtable :sc
    (:merge
     #-ccl :common-lisp
     #+ccl :current)))

(defpackage #:cl-collider
  (:use #:cl)
  (:nicknames #:sc)
  #+ccl (:import-from :ccl #:make-id-map #:assign-id-map-id #:id-map-free-object)
  #+sbcl (:lock t)
  (:export #:*s*
	   #:*sc-synth-program*
	   #:*sc-plugin-paths*
	   #:*sc-synthdefs-path*
	   #:+inf+
	   
	   #+linux #:jack-connect
	   
	   #:make-server-options
	   #:server-options
	   #:server-options-block-size
	   
	   #:*synth-definition-mode*
	   #:defsynth
	   #:synth
	   #:get-synthdef-metadata
	   #:synthdef-metadata
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
	   #:*server-quit-hooks*
	   #:*group-free-all-hooks*
	   #:*stop-hooks*

	   #:now
	   #:callback
	   #:quant
	   #:set-clock
	   #:clock-bpm
	   #:clock-beats
	   #:clock-add
	   #:clock-quant
	   #:clock-dur
	   #:clock-clear
	   #:at-beat
	   #:at-task
	   
	   #:bufnum
	   #:sr
	   #:frames
	   #:chanls
	   #:buffer-read
	   #:buffer-read-channel
	   #:buffer-alloc
	   #:buffer-cue-soundfile
	   #:buffer-close
	   #:buffer-free
	   #:buffer-write
	   #:buffer-get
	   #:buffer-getn
	   #:buffer-get-to-list
	   #:buffer-load-to-list
	   #:buffer-set
	   #:buffer-setn
	   #:buffer-zero
	   #:buffer-normalize
	   #:buffer-dur
	   #:buffer-copy
	   
	   #:wavetable
	   #:buffer-read-as-wavetable
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
	   #:env-at
	   #:env-as-signal

	   #:fft
	   #:ifft
	   #:pvcalc
	   #:pvcalc2
	   #:pv-collect
	   #:running-sum-rms))

(defpackage #:sc-user
  (:use #:cl #:sc))

(in-package :sc-user)
(named-readtables:in-readtable :sc)


