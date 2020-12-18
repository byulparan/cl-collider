(in-package #:sc)

(def-pv-chain-ugen (pv-noise-synth-p "PV_NoiseSynthP")
    (buffer &optional (threshold 0.1) (num-frames 2.0) (initflag 0.0))
  (multinew new 'pv-chain-ugen buffer threshold num-frames initflag))

(def-pv-chain-ugen (pv-partial-synth-p "PV_PartialSynthP")
    (buffer &optional (threshold 0.1) (num-frames 2.0) (initflag 0.0))
  (multinew new 'pv-chain-ugen buffer threshold num-frames initflag))

(def-pv-chain-ugen (pv-noise-synth-f "PV_NoiseSynthF")
    (buffer &optional (threshold 0.1) (num-frames 2.0) (initflag 0.0))
  (multinew new 'pv-chain-ugen buffer threshold num-frames initflag))

(def-pv-chain-ugen (pv-partial-synth-f "PV_PartialSynthF")
    (buffer &optional (threshold 0.1) (num-frames 2.0) (initflag 0.0))
  (multinew new 'pv-chain-ugen buffer threshold num-frames initflag))

(def-pv-chain-ugen (pv-mag-map "PV_MagMap")
    (buffer mapbuf)
  (multinew new 'pv-chain-ugen buffer mapbuf))

(def-pv-chain-ugen (pv-max-mag-n "PV_MaxMagN")
    (buffer numbins)
  (multinew new 'pv-chain-ugen buffer numbins))

(def-pv-chain-ugen (pv-min-mag-n "PV_MinMagN")
    (buffer numbins)
  (multinew new 'pv-chain-ugen buffer numbins))

(def-pv-chain-ugen (pv-mag-buffer "PV_MagBuffer")
    (buffer databuffer)
  (multinew new 'pv-chain-ugen buffer databuffer))

(def-pv-chain-ugen (pv-freq-buffer "PV_FreqBuffer")
    (buffer databuffer)
  (multinew new 'pv-chain-ugen buffer databuffer))

(def-pv-chain-ugen (pv-odd-bin "PV_OddBin")
    (buffer)
  (multinew new 'pv-chain-ugen buffer))

(def-pv-chain-ugen (pv-even-bin "PV_EvenBin")
    (buffer)
  (multinew new 'pv-chain-ugen buffer))

(def-pv-chain-ugen (pv-bin-filter "PV_BinFilter")
    (buffer &optional (start 0) (end 0))
  (multinew new 'pv-chain-ugen buffer start end))

(def-pv-chain-ugen (pv-invert "PV_Invert")
    (buffer)
  (multinew new 'pv-chain-ugen buffer))

(def-pv-chain-ugen (pv-bin-delay "PV_BinDelay")
    (buffer maxdelay delaybuf fbbuf &optional (hop 0.5))
  (multinew new 'pv-chain-ugen buffer maxdelay delaybuf fbbuf hop))

(def-pv-chain-ugen (pv-freeze "PV_Freeze")
    (buffer &optional (freeze 0.0))
  (multinew new 'pv-chain-ugen buffer freeze))


(defgeneric calc-pv-recsize (buffer frame-size hop &optional sample-rate))

(defmethod calc-pv-recsize ((buffer buffer) frame-size hop &optional sample-rate)
  (calc-pv-recsize (* 1.0 (/ (frames buffer) (sr buffer))) frame-size hop sample-rate))

(defmethod calc-pv-recsize ((buffer number) frame-size hop &optional sample-rate)
  (let ((rawsize))
    (setf sample-rate (if sample-rate sample-rate 44100.0))
    (setf rawsize (* frame-size (ceil~ (/ (* buffer sample-rate) frame-size))))
    (+ (* rawsize (reciprocal hop)) 3)))


(def-pv-chain-ugen (pv-record-buf "PV_RecordBuf")
    (buffer recbuf &key (offset 0.0) (run 0.0) (loop 0.0)
	    (hop 0.5) (wintype 0))
  (multinew new 'pv-chain-ugen buffer recbuf offset run loop hop wintype))

(def-pv-chain-ugen (pv-play-buf "PV_PlayBuf")
    (buffer playbuf rate &key (offset 0.0) (loop 0.0))
  (multinew new 'pv-chain-ugen buffer playbuf rate offset loop))

(def-pv-chain-ugen (pv-bin-play-buf "PV_BinPlayBuf")
    (buffer playbuf rate &key (offset 0.0) (binstart 0.0) (binskip 1.0) (numbins 1.0) (loop 0.0) (clear 0.0))
  (multinew new 'pv-chain-ugen buffer playbuf rate offset loop binstart binskip numbins clear))

(def-pv-chain-ugen (pv-buf-rd "PV_BufRd")
    (buffer playbuf &key (point 1.0))
  (multinew new 'pv-chain-ugen buffer playbuf point))

(def-pv-chain-ugen (pv-bin-buf-rd "PV_BinBufRd")
    (buffer playbuf &key (point 1.0) (binstart 0.0) (binskip 1.0) (numbins 1.0) (clear 0.0))
  (multinew new 'pv-chain-ugen buffer playbuf point binstart binskip numbins clear))

(def-pv-chain-ugen (pv-spectral-map "PV_SpectralMap")
    (buffer specbuffer &key (floor 0.0) (freeze 0.0) (mode 0.0) (norm 0.0) (window 0.0))
  (multinew new 'pv-chain-ugen buffer specbuffer floor freeze mode norm window))

(def-pv-chain-ugen (pv-spectral-enhance "PV_SpectralEnhance")
    (buffer &key (num-partials 8) (ratio 2.0) (strength 0.1))
  (multinew new 'pv-chain-ugen buffer num-partials ratio strength))

(defugen (bin-data "BinData")
    (buffer bin &optional (overlaps 0.5))
  ((:ar (multinew new 'multiout-ugen 2 buffer bin overlaps))
   (:kr (multinew new 'multiout-ugen 2 buffer bin overlaps))))


;; very poorly implemented for the time being - do not use
;; PV_PitchShift



