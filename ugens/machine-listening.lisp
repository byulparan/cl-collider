(in-package #:sc)

(defugen (beat-track "BeatTrack")
    (chain &optional (lock 0))
  ((:kr (multinew new 'multiout-ugen 4 chain lock))))

(defugen (loudness "Loudness")
    (chain &optional (s-mask 0.25) (t-mask 1))
  ((:kr (multinew new 'ugen chain s-mask t-mask))))

(defugen (onsets "Onsets")
    (chain &optional (threshold 0.5) (odf-type :rcomplex) (relax-time 1)
	   (floor 0.1) (min-gap 10) (median-span 11) (wh-type 1) (raw-odf 0))
  ((:kr
    (let* ((odf-types '(:power :magsum :complex :rcomplex :phase :wphase :mkl))
	   (odf-type (if (and (integerp odf-type)
			      (>= odf-type 0)
			      (< odf-type (length odf-types)))
			 odf-type
			 (position odf-type odf-types))))
      (multinew new 'ugen
		chain threshold odf-type relax-time floor min-gap median-span
		wh-type raw-odf)))))

(defugen (keytrack "KeyTrack")
    (chain &optional (key-decay 2.0) (chroma-leak 0.5))
  ((:kr (multinew new 'ugen chain key-decay chroma-leak))))

(defugen (mfcc "MFCC")
    (chain &optional (numcoeff 13))
  ((:kr (multinew new 'multiout-ugen numcoeff chain numcoeff))))

(defugen (beat-track2 "BeatTrack2")
    (bus-index num-features
	       &optional (window-size 2.0) (phase-accuracy 0.02) (lock 0) (weighting-scheme -2.5))
  ((:kr
    (multinew new 'multiout-ugen 6
	      bus-index num-features window-size phase-accuracy lock weighting-scheme))))

(defugen (spec-flatness "SpecFlatness") (buffer)
  ((:kr (multinew new 'ugen buffer))))

(defugen (spec-pcile "SpecPcile")
    (buffer &optional (fraction 0.5) (interpolate 0))
  ((:kr (multinew new 'ugen buffer fraction interpolate))))

(defugen (spec-centroid "SpecCentroid") (buffer)
  ((:kr (multinew new 'ugen buffer))))
