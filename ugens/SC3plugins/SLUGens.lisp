(in-package #:sc)

(defugen (sort-buf "SortBuf")
    (&optional (bufnum 0.0) (sortrate 10.0) (reset 0.0))
  ((:ar (multinew new 'ugen bufnum sortrate reset))))

(defugen (gravity-grid "GravityGrid")
    (&optional (reset 0.0) (rate 0.1) (newx 0.0) (newy 0.0) bufnum (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen reset rate newx newy (if bufnum bufnum -1)) mul add))))

(defugen (gravity-grid2 "GravityGrid2")
    (&optional (reset 0.0) (rate 0.1) (newx 0.0) (newy 0.0) bufnum (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen reset rate newx newy bufnum) mul add))))

(defugen (breakcore "Breakcore")
    (&optional (bufnum 0.0) capturein  capturetrigger (duration 0.1) ampdropout)
  ((:ar (multinew new 'ugen bufnum capturein capturetrigger duration ampdropout))))

(defugen (max "Max")
    (in &optional (numsamp 64))
  ((:kr (multinew new 'ugen in numsamp))))

(defugen (print-val "PrintVal")
    (in &optional (numblocks 100) (id 0.0))
  ((:kr (multinew new 'ugen in numblocks id))))

(defugen (env-detect "EnvDetect")
    (in &optional (attack 100) (release 0))
  ((:ar (multinew new 'ugen in attack release)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (fitz-hugh-nagumo "FitzHughNagumo")
    (&optional (reset 0.0) (rateu 0.01) (ratew 0.01) (b0 1.0) (b1 1.0) (initu 0.0) (initw 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen reset rateu ratew b0 b1 initu initw) mul add))))

(defugen (double-well "DoubleWell")
    (&optional (reset 0.0) (ratex 0.01) (ratey 0.01) (f 1.0) (w 0.001) (delta 1.0) (initx 0.0) (inity 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen reset ratex ratey f w delta initx inity) mul add))))

(defugen (double-well2 "DoubleWell2")
    (&optional (reset 0.0) (ratex 0.01) (ratey 0.01) (f 1.0) (w 0.001) (delta 1.0) (initx 0.0) (inity 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen reset ratex ratey f w delta initx inity) mul add))))

(defugen (double-well3 "DoubleWell3")
    (&optional (reset 0.0) (rate 0.01) (f 0.0) (delta 0.25) (initx 0.0) (inity 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen reset rate f delta initx inity) mul add))))

(defugen (weakly-nonlinear "WeaklyNonlinear")
    (input &optional (reset 0.0) (ratex 1.0) (ratey 1.0) (freq 440.0) (initx 0.0) (inity 0.0) (alpha 0.0) (xexponent 0.0)
	   (beta 0.0) (yexponent 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen input reset ratex ratey freq initx inity alpha xexponent beta yexponent) mul add))))

(defugen (weakly-nonlinear2 "WeaklyNonlinear2")
    (input &optional (reset 0.0) (ratex 1.0) (ratey 1.0) (freq 440.0) (initx 0.0) (inity 0.0) (alpha 0.0) (xexponent 0.0)
	   (beta 0.0) (yexponent 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen input reset ratex ratey freq initx inity alpha xexponent beta yexponent) mul add))))

(defugen (terman-wang "TermanWang")
    (&optional (input 0.0) (reset 0.0) (ratex 0.01) (ratey 0.01) (alpha 1.0) (beta 1.0) (eta 1.0) (initx 0.0) (inity 0.0) 
	       (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen input reset ratex ratey alpha beta eta initx inity) mul add))))

(defugen (lti "LTI")
    (input &optional (bufnuma 0.0) (bufnumb 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen input bufnuma bufnumb) mul add))))

(defugen (nl "NL")
    (input &optional (bufnuma 0.0) (bufnumb 1.0) (guard1 1000.0) (guard2 100.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen input bufnuma bufnumb guard1 guard2) mul add))))

(defugen (nl2 "NL2")
    (input &optional (bufnum 0.0) (maxsizea 10.0) (maxsizeb 10.0) (guard1 1000.0) (guard2 100.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen input bufnum maxsizea maxsizeb guard1 guard2) mul add))))

(defugen (lpc-error "LPCError")
    (input &optional (p 10.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen input p) mul add))))

(defugen (kmeans-to-bpset1 "KmeansToBPSet1")
    (&optional (freq 440.0) (numdatapoints 20.0) (maxnummeans 4.0) (nummeans 4.0) (tnewdata 1.0) (tnewmeans 1.0)
	       (soft 1.0) bufnum (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq numdatapoints maxnummeans nummeans tnewdata tnewmeans soft (if bufnum bufnum -1)) mul add))))

(defugen (instruction "Instruction")
    (&optional (bufnum 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen bufnum) mul add))))

(defugen (wave-terrain "WaveTerrain")
    (&optional (bufnum 0.0) x y (xsize 100.0) (ysize 100.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen bufnum x y xsize ysize) mul add))))

(defugen (vm-scan2d "VMScan2D")
    (&optional (bufnum 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen 2 bufnum) mul add))))

(defugen (sl-onset "SLOnset")
    (input &optional (memorysize1 20.0) (before 5.0) (after 5.0) (threshold 10.0) (hysteresis 10.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen input memorysize1 before after threshold hysteresis) mul add))))

(defugen (two-tube "TwoTube")
    (&optional (input 0.0) (k 0.01) (loss 1.0) (d1length 100.0) (d2length 100.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen input k loss d1length d2length) mul add))))

;; NTube

(defugen (env-follow "EnvFollow")
    (&optional (in 0.0) (decaycoeff 0.99) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen in decaycoeff) mul add))
   (:kr (madd (multinew new 'ugen in decaycoeff) mul add))))

(defugen (sieve1 "Sieve1")
    (bufnum &optional (gap 2.0) (alternate 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen bufnum gap alternate) mul add))
   (:kr (madd (multinew new 'ugen bufnum gap alternate) mul add) )))

(defugen (oregonator "Oregonator")
    (&optional (reset 0.0) (rate 0.01) (epsilon 1.0) (mu 1.0) (q 1.0) (initx 0.5) (inity 0.5) (initz 0.5)
	       (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen 3 reset rate epsilon mu q initx inity initz) mul add))))

(defugen (brusselator "Brusselator")
    (&optional (reset 0.0) (rate 0.01) (mu 1.0) (gamma 1.0) (initx 0.5) (inity 0.5) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen 2 reset rate mu gamma initx inity) mul add))))

(defugen (spruce-budworm "SpruceBudworm")
    (&optional (reset 0.0) (rate 0.1) (k1 27.9) (k2 1.5) (alpha 0.1) (beta 10.1) (mu 0.3) (rho 10.1)
	       (initx 0.9) (inity 0.1) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen 2 reset rate k1 k2 alpha beta mu rho initx inity) mul add))))


