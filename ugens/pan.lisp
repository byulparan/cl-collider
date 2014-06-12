
;;; ...from [Pan.sc]

(in-package #:sc)

(defugen (pan2 "Pan2") (in &optional (pos 0.0) (level 1.0))
  ((:ar (multinew new 'multiout-ugen 2 in pos level))
   (:kr (multinew new 'multiout-ugen 2 in pos level)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 1)))

(defugen (lin-pan2 "LinPan2") (in &optional (pos 0.0) (level 1.0))
  ((:ar (multinew new 'multiout-ugen 2 in pos level))
   (:kr (multinew new 'multiout-ugen 2 'pan2 in pos level)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 1)))

(defugen (pan4 "Pan4") (in &optional (x-pos 0.0) (y-pos 0.0) (level 1.0))
  ((:ar (multinew new 'multiout-ugen 4 in x-pos y-pos level))
   (:kr (multinew new 'multiout-ugen 4 in x-pos y-pos level)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 1)))

(defugen (balance2 "Balance2")
    (left right &optional (pos 0.0) (level 1.0))
  ((:ar (multinew new 'multiout-ugen 2 left right pos level))
   (:kr (multinew new 'multiout-ugen 2 left right pos level)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 2)))

(defugen (rotate2 "Rotate2") (x y &optional (pos 0.0))
  ((:ar (multinew new 'multiout-ugen 2 x y pos))
   (:kr (multinew new 'multiout-ugen 2 x y pos)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 2)))


(defugen (pan-b "PanB")
    (in &optional (azimuth 0.0) (elevation 0.0) (gain 1.0))
  ((:ar (multinew new 'multiout-ugen 4 in azimuth elevation gain))
   (:kr (multinew new 'multiout-ugen 4 in azimuth elevation gain)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 1)))


(defugen (pan-b2 "PanB2")
    (in &optional (azimuth 0.0) (gain 1.0))
  ((:ar (multinew new 'multiout-ugen 3 in azimuth gain))
   (:kr (multinew new 'multiout-ugen 3 in azimuth gain)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 1)))


(defugen (bi-pan-b2 "BiPanB2")
    (in-a in-b &optional (azimuth 0.0) (gain 1.0))
  ((:ar (multinew new 'multiout-ugen 3 in-a in-b azimuth gain))
   (:kr (multinew new 'multiout-ugen 3 in-a in-b azimuth gain)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 2)))

(defugen (decode-b2 "DecodeB2")
    (num-chans w x y &optional (orientation 0.5))
  ((:ar (multinew new 'multiout-ugen num-chans w x y orientation))
   (:kr (multinew new 'multiout-ugen num-chans w x y orientation)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 3)))

(defugen (pan-az "PanAz")
    (num-chans in &optional (pos 0.0) (level 1.0) (width 2.0) (orientation 0.5))
  ((:ar (multinew new 'multiout-ugen num-chans in pos level width orientation))
   (:kr (multinew new 'multiout-ugen num-chans in pos level width orientation)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 1)))


(defugen (x-fade2 "XFade2")
    (in-a &optional (in-b 0.0) (pan 0.0) (level 1.0))
  ((:ar (multinew new 'ugen in-a in-b pan level))
   (:kr (multinew new 'ugen in-a in-b pan level)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 2)))

(defugen (lin-x-fade2 "LinXFade2")
    (in-a &optional (in-b 0.0) (pan 0.0) (level 1.0))
  ((:ar (sc::mul (multinew new 'ugen in-a in-b pan) level))
   (:kr (sc::mul (multinew new 'ugen in-a in-b pan) level)))
  :check-fn (lambda (ugen) (check-n-inputs ugen 2)))
