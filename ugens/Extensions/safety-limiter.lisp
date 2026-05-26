;; https://github.com/nhthn/supercollider-safety-limiter
;;
;; SafetyLimiter is a SuperCollider UGen that serves as a transparent brickwall limiter.
;; It uses 4x oversampling to compute the signal peak, therefore making it a "true peak" limiter,
;; and unlike the built-in Limiter, it induces no latency on the input signal.

(in-package :sc)

(sc::defugen (safety-limiter "SafetyLimiter")
    (in &key (releaseTime 0.5) (holdTime 0.1))
  ((:ar (sc::multinew sc::new 'sc::ugen in releaseTime holdTime))))
