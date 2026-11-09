(in-package :sc)

(defugen (pm7os "PM7OS") (ctrl-matrix mod-matrix &key (oversample 4) (mul 1) (add 0))
  ((:ar
    (flet ((convert (x)
             (if (eq (rate x) :audio) x
                 (k2a.ar x))))
      (setf ctrl-matrix
            (loop for rest = (alexandria:flatten  ctrl-matrix) then (cdddr rest)
                  while rest
                  for a = (convert (first rest))
                  for b = (second rest)
                  for c = (convert (third rest))
                  collect (list a b c)))
      (setf mod-matrix (mapcar #'convert (alexandria:flatten mod-matrix)))
      (madd
       (apply #'multinew new 'multiout-ugen 6
              (append (alexandria:flatten ctrl-matrix)
                      (alexandria:flatten mod-matrix)
                      (list oversample)))
         mul add)))))

;; documentation is messed up in my installation, and isn't super clear. So overriden here:

(setf (documentation 'fm7.ar 'function)
      "pm7os implements a 6x6 oscillator matrix, where each oscillator's phase
can be modulated by any of the other oscillators' output. If you are familiar
with Native Instruments FM7 or FM8 or supercollider's FM7, then this is similar.

Main differences from fm7 in supercollider. This is oversampled and it supports audio rate modulation
for every input other than phase.

ctrl-matrix allows you to define the frequency, phase and amplitude for each oscillator:

[
 [240 0 1] ;; oscillator 1 has frequency of 240, phase of 0 and amplitude of 1
 [25 pi 3] ;; oscillator 2 has frequency of 25, phase of pi (180 degrees) and amplitude of 3
 [240 0 0] ;; this oscillator is off
 [240 0 1]
 [240 0 1]
 [240 0 1]] 

 This does not have to be a nested list, but it supports nested lists for convenience.

 Frequency and amplitude can be modulated at audio rate, phase which can only be set at initialization

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

   Values in modMatrix can be modulated at audio rate.

   pm7os outputs 6 channels (1 for each of the 6 oscillators), and so you will need to select the outputs/
   carriers that you want.

   See also fm7 from supercolliders extensions library if you want less resource heavy version of this.
")
