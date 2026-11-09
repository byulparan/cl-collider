(in-package :cl-collider)

(sc::defugen (saw-os "SawOS")
    (freq &key (phase 0) (oversample :x2) (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl-to-audio freq) (ctrl-to-audio phase)
                                (convert-os oversample)) mul add))))

(sc::defugen (sin-osc-os "SinOscOS")
    (freq &key (phase 0) (oversample :x2) (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl-to-audio freq) (ctrl-to-audio phase)
                                (convert-os oversample)) mul add))))

(sc::defugen (tri-os "TriOS")
    (freq &key (phase 0) (oversample :x2) (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl-to-audio freq) (ctrl-to-audio phase)
                                (convert-os oversample)) mul add))))

(sc::defugen (square-os "SquareOS")
    (freq &key (phase 0) (width 0.5) (oversample :x2) (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl-to-audio freq) (ctrl-to-audio phase)
                                (ctrl-to-audio width)
                                (convert-os oversample)) mul add))))

(sc::defugen (var-saw-os "VarSawOS")
    (freq &key (phase 0) (width 0.5) (oversample :x2) (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl-to-audio freq) (ctrl-to-audio phase)
                                (ctrl-to-audio width)
                                (convert-os oversample)) mul add))))

(sc::defugen (pm-osc-os "PMOscOS")
    (&key (car-freq 440) (mod-freq 220) (pm-mul 1) (pm-mod-phase 0) (oversample :x2) (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen
                                (ctrl-to-audio car-freq)
                                (ctrl-to-audio mod-freq)
                                (ctrl-to-audio pm-mul)
                                (ctrl-to-audio pm-mod-phase)
                                (convert-os oversample)) mul add))))


(sc::defugen (saw-bl "SawBL")
    (freq &key (iphase 0) (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl-to-audio freq) iphase) mul add))))

(sc::defugen (tri-bl "TriBL")
    (freq &key (width 0.5) (iphase 0) (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl-to-audio freq)
                                iphase
                                (ctrl-to-audio width))
                  mul add))))

(sc::defugen (square-bl "SquareBL")
    (freq &key (width 0.5) (iphase 0) (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl-to-audio freq)
                                iphase
                                (ctrl-to-audio width))
                  mul add))))

(sc::defugen (impulse-bl "ImpulseBL")
    (freq &key (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen (ctrl-to-audio freq)) mul add))))

(sc::defugen (serge-fold "SergeFoldOS")
    (in amp &key (oversample 1) (mul 1) (add 0))
  ((:ar (sc::madd (sc::multinew sc::new 'sc::ugen
                                in amp oversample) mul add))))

