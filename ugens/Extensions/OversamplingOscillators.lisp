(in-package :cl-collider)

(flet ((ctrl->audio (x)
         (if (eq (rate x) :audio) x
             (k2a.ar x))))

  (sc::defugen (saw-os "SawOS")
      (freq &key (phase 0) (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl->audio freq) (ctrl->audio phase) oversample)
                  mul add))))

  (sc::defugen (sin-osc-os "SinOscOS")
      (freq &key (phase 0) (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl->audio freq) (ctrl->audio phase) oversample)
                    oversample) mul add)))

  (sc::defugen (pm-osc-os "PMOscOS")
      (&key (car-freq 440) (mod-freq 220) (pm-mul 1) (pm-mod-phase 0) (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen
                                  (ctrl->audio car-freq)
                                  (ctrl->audio mod-freq)
                                  (ctrl->audio pm-mul)
                                  (ctrl->audio pm-mod-phase)
                                  oversample) mul add))))

  (defugen (fm7os "FM7OS") (ctrl-matrix mod-matrix &key (oversample 4) (mul 1) (add 0))
    ((:ar
      (setf ctrl-matrix
            (loop for rest = (alexandria:flatten  ctrl-matrix) then (cdddr rest)
                  while rest
                  for a = (ctrl->audio (first rest))
                  for b = (second rest)
                  for c = (ctrl->audio (third rest))
                  collect (list a b c)))
      (setf mod-matrix (mapcar #'ctrl->audio (alexandria:flatten mod-matrix)))
      (madd
       (apply #'multinew new 'multiout-ugen 6
              (append (alexandria:flatten ctrl-matrix)
                      (alexandria:flatten mod-matrix)
                      (list oversample)))
       mul add))))


  (defugen (fm7aos "FM7aOS") (ctrl-matrix mod-matrix
                                          &key (synth-types (list 0 0 0 0))
                                          (oversample 4) (mul 1) (add 0))
    ((:ar
      (setf ctrl-matrix (mapcar #'ctrl->audio (alexandria:flatten ctrl-matrix)))
      (setf mod-matrix
            (loop for rest = (alexandria:flatten  mod-matrix) then (cddddr rest)
                  while rest
                  for i :from 0
                  for a = (if (eq 0 i) (car synth-types) (ctrl->audio (first rest)))
                  for b = (if (eq 1 i) (car synth-types) (ctrl->audio (second rest)))
                  for c = (if (eq 2 i) (car synth-types) (ctrl->audio (third rest)))
                  for d = (if (eq 3 i) (car synth-types) (ctrl->audio (fourth rest)))
                  collect (list a b c d)))
      (madd
       (apply #'multinew new 'multiout-ugen 4
              (append (alexandria:flatten ctrl-matrix)
                      (alexandria:flatten mod-matrix)
                      (list oversample)))
       mul add))))

  (defugen (fm7bos "FM7bOS") (ctrl-matrix mod-matrix
                                          &key (synth-types (list 0 0 0 0))
                                          (oversample 4) (mul 1) (add 0))
    ((:ar
      (setf ctrl-matrix (mapcar #'ctrl->audio (alexandria:flatten ctrl-matrix)))
      (setf mod-matrix
            (loop for rest = (alexandria:flatten  mod-matrix) then (cddddr rest)
                  while rest
                  for i :from 0
                  for a = (if (eq 0 i) (car synth-types) (ctrl->audio (first rest)))
                  for b = (if (eq 1 i) (car synth-types) (ctrl->audio (second rest)))
                  for c = (if (eq 2 i) (car synth-types) (ctrl->audio (third rest)))
                  for d = (if (eq 3 i) (car synth-types) (ctrl->audio (fourth rest)))
                  collect (list a b c d)))
      (madd
       (apply #'multinew new 'multiout-ugen 4
              (append (alexandria:flatten ctrl-matrix)
                      (alexandria:flatten mod-matrix)
                      (list oversample)))
       mul add))))

  (defugen (pm7os "PM7OS") (ctrl-matrix mod-matrix &key (oversample 4) (mul 1) (add 0))
    ((:ar
      (setf ctrl-matrix
            (loop for rest = (alexandria:flatten  ctrl-matrix) then (cdddr rest)
                  while rest
                  for a = (ctrl->audio (first rest))
                  for b = (second rest)
                  for c = (ctrl->audio (third rest))
                  collect (list a b c)))
      (setf mod-matrix (mapcar #'ctrl->audio (alexandria:flatten mod-matrix)))
      (madd
       (apply #'multinew new 'multiout-ugen 6
              (append (alexandria:flatten ctrl-matrix)
                      (alexandria:flatten mod-matrix)
                      (list oversample)))
       mul add))))

  (sc::defugen (square-os "SquareOS")
      (freq &key (phase 0) (width 0.5) (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl->audio freq) (ctrl->audio phase)
                                  (ctrl->audio width) oversample)
                    mul add))))
  
  (sc::defugen (tri-os "TriOS")
      (freq &key (phase 0) (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl->audio freq) (ctrl->audio phase) oversample)
                    mul add))))
  
  (sc::defugen (var-saw-os "VarSawOS")
      (freq &key (phase 0) (width 0.5) (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl->audio freq) (ctrl->audio phase)
                                  (ctrl->audio width) oversample)
                    mul add))))

    (sc::defugen (buchla-fold-os "BuchlaFoldOS")
      (sig amp &key (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen
                                  (ctrl->audio sig) (ctrl->audio amp)
                                  oversample) mul add))))

  (sc::defugen (serge-fold-os "SergeFoldOS")
      (sig amp &key (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen
                                  (ctrl->audio sig) (ctrl->audio amp)
                                  oversample) mul add))))

  (sc::defugen (saw-bl "SawBL")
      (freq &key (iphase 0) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl->audio freq) iphase) mul add))))

  (sc::defugen (square-bl "SquareBL")
      (freq &key (width 0.5) (iphase 0) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl->audio freq)
                                  iphase
                                  (ctrl->audio width))
                    mul add))))

    (sc::defugen (tri-bl "TriBL")
      (freq &key (width 0.5) (phase 0) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl->audio freq)
                                  phase
                                  (ctrl->audio width))
                    mul add))))

  (sc::defugen (impulse-bl "ImpulseBL")
      (freq &key (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl->audio freq))
                    mul add))))

    (sc::defugen (shaper-os "ShaperOS")
      (buffer in &key (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen
                                  buffer (ctrl->audio in) oversample)
                    mul add))))

  (sc::defugen (shaper-os "ShaperOS2")
      (buffer in &key (buf-divs 1) buf-loc (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen
                                  buffer (ctrl->audio in) buf-divs
                                  (ctrl->audio buf-loc) oversample)
                    mul add))))

  (sc::defugen (osc-os "OscOS")
      (bufnum phase &key (buf-divs 1) (buf-loc 0) (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen
                                  bufnum (ctrl->audio phase) buf-divs (ctrl->audio buf-loc)
                                  (ctrl->audio buf-loc) oversample)
                    mul add))))

  (sc::defugen (osc-os "OscOS2")
      (sound-buf-num phase-buf-num freq phase
                     &key (buf-divs 1) (buf-loc 0) (phase-buf-divs 1)
                     (phase-buf-loc 0) (oversample 1) (mul 1) (add 0))
    ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen
                                  sound-buf-num phase-buf-num (ctrl->audio freq)
                                  (ctrl->audio phase) buf-divs (ctrl->audio buf-loc)
                                  phase-buf-divs (ctrl->audio phase-buf-loc) oversample)
                    mul add)))))


;; documentation is messed up in my installation, and isn't super clear. So overriden here:

(setf (documentation 'pm7os.ar 'function)
      "pm7os implements a 6x6 oscillator matrix, where each oscillator's phase
can be modulated by any of the other oscillators' output. If you are familiar
with Native Instruments FM7 or FM8 or supercollider's FM7, then this is similar.

Main differences from fm7 in supercollider. This is oversampled and it supports audio rate modulation
for every input other than phase.

Oversampling values are:
- 0: No oversampling
- 1: 2x
- 2: 4x
- 3: 8x
- 4: 16x

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

(setf (documentation 'fm7os.ar 'function)
      "fm7os implements a 6x6 oscillator matrix, where each oscillator's frequency
can be modulated by any of the other oscillators' output (this is true FM, rather than PM). Feedback is also
implemented, but feedback is phase modulation.

Oversampling values are:
- 0: No oversampling
- 1: 2x
- 2: 4x
- 3: 8x
- 4: 16x

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
   [0 0 1 0 0 0] ;; oscillator 3 is modulated by itself (phase modulation)
   [1 0 0 0 0 0] ;; oscillator 4 is modulated by oscillator 1. There will be a 1 sample delay, as
                 ;; oscillator 4 comes before oscillator 1
   [0 0 0 0 0 0]
   [0 0 0 0 0 0]
  ]
   Again this does not have to be a nested liste, but nested lists are supported for readability.

   Values in modMatrix can be modulated at audio rate.

   fm7os outputs 6 channels (1 for each of the 6 oscillators), and so you will need to select the outputs/
   carriers that you want.
")

(setf (documentation 'fm7aos.ar 'function)
      "fm7aos implements a 4x4 oscillator matrix, where each oscillator's frequency
can be modulated by any of the other oscillators' output using Chowing style 'through zero modulation'.

 A through-zero oscillator allows the frequency to cross the zero threshold into 'negative' frequencies.
Instead of stalling, the oscillator core instantly flips its phase by 180 degrees and keeps running. This
is more stable than traditional linear FM, and less chaotic than traditional 'analog FM'. You can also use
over types of wave form (square, triangle and saw)

Oversampling values are:
- 0: No oversampling
- 1: 2x
- 2: 4x
- 3: 8x
- 4: 16x

synth-types values are:
- 0: sine wave (default if parameter is unset)
- 1: triangle wave
- 2: square wave
- 3: Saw Wave

ctrl-matrix allows you to define the frequency, phase and amplitude for each oscillator:

[
 [240 0 1] ;; oscillator 1 has frequency of 240, phase of 0 and amplitude of 1
 [25 pi 3] ;; oscillator 2 has frequency of 25, phase of pi (180 degrees) and amplitude of 3
 [240 0 0] ;; this oscillator is off
 [240 0 1]
] 

 This does not have to be a nested list, but it supports nested lists for convenience.

 All control values can be modulated at analog rate.

  mod-matrix allows you to wire up the difference oscillators so that they feed into each other. Note, there
is no feedback possible, so the feedback position is ignored (use PM7OS or FM7OS if you need that.)

  [
   [0 1.5 1 0 ] ;; oscillator 1 received modulation of 1.5 times output of oscillator 2 and 1 times oscillator 3
   [0 0 0 0 ] ;; oscillator 2 is not modulated by any of the other oscillators
   [0 0 1 0 ] ;; The 3rd value is ignored.
   [1 0 0 0 ] ;; oscillator 4 is modulated by oscillator 1. There will be a 1 sample delay, as
  ]           ;; oscillator 4 comes before oscillator 1


   Again this does not have to be a nested list, but nested lists are supported for readability.

   Values in modMatrix can be modulated at audio rate.

   fm7aos outputs 4 channels (1 for each of the 4oscillators), and so you will need to select the outputs/
   carriers that you want.
")

(setf (documentation 'fm7bos.ar 'function)
      "fm7aos implements a 4x4 oscillator matrix, where each oscillator's frequency
can be modulated by any of the other oscillators' output using 1 volt/octave style modular FM' (or exponential).

So the frequency shift will double for each value of 1, giving an exponential range. This gives a very chaotic result.
Which can be fun...

Oversampling values are:
- 0: No oversampling
- 1: 2x
- 2: 4x
- 3: 8x
- 4: 16x

synth-types values are:
- 0: sine wave (default if parameter is unset)
- 1: triangle wave
- 2: square wave
- 3: Saw Wave

ctrl-matrix allows you to define the frequency, phase and amplitude for each oscillator:

[
 [240 0 1] ;; oscillator 1 has frequency of 240, phase of 0 and amplitude of 1
 [25 pi 3] ;; oscillator 2 has frequency of 25, phase of pi (180 degrees) and amplitude of 3
 [240 0 0] ;; this oscillator is off
 [240 0 1]
] 

 This does not have to be a nested list, but it supports nested lists for convenience.

 All control values can be modulated at analog rate.

  mod-matrix allows you to wire up the difference oscillators so that they feed into each other. Note, there
is no feedback possible, so the feedback position is ignored (use PM7OS or FM7OS if you need that.)

  [
   [0 1.5 1 0 ] ;; oscillator 1 received modulation of 1.5 times output of oscillator 2 and 1 times oscillator 3
   [0 0 0 0 ] ;; oscillator 2 is not modulated by any of the other oscillators
   [0 0 1 0 ] ;; The 3rd value is ignored.
   [1 0 0 0 ] ;; oscillator 4 is modulated by oscillator 1. There will be a 1 sample delay, as
  ]           ;; oscillator 4 comes before oscillator 1


   Again this does not have to be a nested list, but nested lists are supported for readability.

   Values in modMatrix can be modulated at audio rate.

   fm7aos outputs 4 channels (1 for each of the 4oscillators), and so you will need to select the outputs/
   carriers that you want.

")
