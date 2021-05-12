(in-package #:sc)

(defugen (nhhall "NHHall")
    (&optional (in 0.0) (rt60 1) (stereo 0.5) (lowFreq 200) (lowRatio 0.5) (hiFreq 4000) (hiRatio 0.5) (earlyDiffusion 0.5) (lateDiffusion 0.5) (modRate 0.2) (modDepth 0.3))
  ((:ar (madd (multinew new 'multiout-ugen 2 (elt in 0) (elt in 1) rt60 stereo lowFreq lowRatio hiFreq hiRatio earlyDiffusion lateDiffusion modRate modDepth)))))
