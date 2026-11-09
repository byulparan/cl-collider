(in-package :sc)

(defugen (fm7 "FM7") (ctrl-matrix mod-matrix)
  ((:ar
    (apply #'multinew new 'multiout-ugen 6
           (append (alexandria:flatten ctrl-matrix)
                   (alexandria:flatten mod-matrix))))))

;; this works a little differently from the one in SuperCollider
;; and that also has pretty bad documentation.

(setf (documentation 'fm7.ar 'function)
      "fm7 implements a 6x6 oscillator matrix, where each oscillator's phase
can be modulated by any of the other oscillators' output. If you are familiar
with Native Instruments FM7 or FM8, this works on very similar principles.

ctrl-matrix allows you to define the frequency, phase and amplitude for each oscillator:

[
 [240 0 1] ;; oscillator 1 has frequency of 240, phase of 0 and amplitude of 1
 [25 pi 3] ;; oscillator 2 has frequency of 25, phase of pi (180 degrees) and amplitude of 3
 [240 0 0] ;; this oscillator is off
 [240 0 1]
 [240 0 1]
 [240 0 1]] 

 This does not have to be a nested list, but it supports nested lists for convenience.

 Frequency and amplitude can be modulated at control rate, phase which can only be set at initialization

  mod-matrix allows you to wire up the difference oscillators so that they feed into each other.

  [
   [0 1.5 1 0 0 0] ;; oscillator 1 received modulation of 1.5 times output of oscillator 2 and 1 times oscillator 3
   [0 0 0 0 0 0] ;; oscillator 2 is not modulated by any of the other oscillators
   [0 0 1 0 0 0] ;; oscillator 3 is modulated by itself
   [1 0 0 0 0 0] ;; oscillator 4 is modulated by oscillator 1. There will be a 1 sample delay, as
                 ;; oscillator 4 comes before oscillator 1
   [0 0 0 0 0 0]
   [0 0 0 0 0 0]
  ]
   Again this does not have to be a nested liste, but nested lists are supported for readability.

   Values in modMatrix can be modulated at control rate.

   fm7 outputs 6 channels (1 for each of the 6 oscillators), and so you will need to select the outputs/
   carriers that you want.

   See also pm7os from the oversampling-oscillators package which is oversampled, and supports audio
   modulation if you need it.
")
