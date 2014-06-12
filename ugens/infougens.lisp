
(in-package #:sc)

(defugen (info-ugen-base "InfoUGenBase") ()
  ((:ir (multinew new 'ugen))))

(defugen (buf-info-ugen-base "BufInfoUGenBase") (bufnum)
  ((:kr (multinew new 'ugen bufnum))
   (:ir (multinew new 'ugen bufnum))))

(defmacro def-info-ugen-base (name)
  `(defugen (,(car name) ,(second name))
       ()
     ((:ir (multinew new 'ugen)))))

(defmacro def-buf-info-ugen-base (name)
  `(defugen (,(car name) ,(second name))
       (bufnum)
     ((:kr (multinew new 'ugen bufnum))
      (:ir (multinew new 'ugen bufnum)))))
 
(defugen (num-running-synths "NumRunningSynths") ()
  ((:kr (multinew new 'ugen))))

(def-info-ugen-base (sample-rate "SampleRate"))
(def-info-ugen-base (sample-dur "SampleDur"))
(def-info-ugen-base (radians-per-sample "RadiansPerSample"))
(def-info-ugen-base (control-rate "ControlRate"))
(def-info-ugen-base (control-dur "ControlDur"))
(def-info-ugen-base (subsample-offset "SubsampleOffset"))
(def-info-ugen-base (num-output-buses "NumOutputBuses"))
(def-info-ugen-base (num-input-buses "NumInputBuses"))
(def-info-ugen-base (num-audio-buses "NumAudioBuses"))
(def-info-ugen-base (num-control-buses "NumControlBuses"))
(def-info-ugen-base (num-buffers "NumBuffers"))

(def-buf-info-ugen-base (buf-sample-rate "BufSampleRate"))
(def-buf-info-ugen-base (buf-rate-scale "BufRateScale"))
(def-buf-info-ugen-base (buf-frames "BufFrames"))
(def-buf-info-ugen-base (buf-samples "BufSamples"))
(def-buf-info-ugen-base (buf-dur "BufDur"))
(def-buf-info-ugen-base (buf-channels "BufChannels"))

(defun min-nyquist (ugen)
  (min~ ugen (mul (sample-rate.ir) 0.5)))
