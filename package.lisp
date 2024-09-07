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

	   #:sched-ahead
	   #:latency
	   
	   #:make-server-options
	   #:server-options
	   #:server-options-block-size
	   
	   #:*synth-definition-mode*
	   #:defsynth
	   #:synth
	   #:get-synthdef-metadata
	   #:synthdef-metadata
	   #:with-controls
	   #:named-control
	   #:kr
	   #:play
	   #:proxy
	   #:proxy-ctrl

	   #:with-rendering

	   #:all-running-servers
	   #:make-external-server
	   #:server-boot
	   #:server-quit
	   #:boot-p 
	   #:send-message
	   #:send-bundle

	   #:control-get
	   #:control-set
	   #:add-reply-responder
	   #:remove-reply-responder
	   #:sync
	   
	   #:at
	   #:free
	   #:release
	   #:ctrl
	   #:map-bus
	   #:is-playing-p

	   #:make-group
	   #:server-query-all-nodes
	   #:group-free-all
	   #:server-free-all
	   #:stop
	   #:server-status
           #:*server-boot-hooks*
	   #:*server-quit-hooks*
	   #:*server-free-all-hooks*
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
	   #:path
	   #:buffer-read
	   #:buffer-read-channel
	   #:buffer-alloc
	   #:buffer-alloc-sequence
	   #:buffer-cue-soundfile
	   #:buffer-close
	   #:buffer-free
	   #:buffer-write
	   #:buffer-get
	   #:buffer-getn
	   #:buffer-get-to-list
	   #:buffer-load-to-list
	   #:buffer-to-list
	   #:buffer-get-to-array
	   #:buffer-load-to-array
	   #:buffer-to-array
	   #:buffer-set
	   #:buffer-setn
	   #:buffer-load
	   #:buffer-zero
	   #:buffer-normalize
	   #:buffer-dur
	   #:buffer-copy
	   
	   #:buffer-fill
	   #:buffer-read-as-wavetable
	   #:calc-pv-recsize
	   #:local-buf
	   #:set-buf
	   #:clear-buf
	   #:local-buf-list
	   #:as-wavetable
	   #:as-wavetable-no-wrap
	   
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
	   #:if~
	   #:clip
	   #:clip2
	   #:fold
	   #:wrap
	   #:range
	   #:exp-range
	   #:unipolar
	   #:bipolar
	   #:lin-lin
	   #:lin-exp
	   #:exp-lin
	   #:exp-exp

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
	   #:pv-calc
	   #:pv-calc2
	   #:pv-collect
	   #:running-sum-rms))

(defpackage #:sc-user
  (:use #:cl #:sc))

(in-package :sc-user)
(named-readtables:in-readtable :sc)


