(in-package #:sc)

(defun b-pass-sc (low-hi freq rq)
  (let* ((w0 (*~ 2 pi freq (sample-dur.ir)))
         (cos-w0 (cos~ w0))
         (i (funcall (if (eq :low low-hi) #'-~ #'+~) 1 cos-w0))
         (alpha (*~ (sin~ w0) 0.5 (sqrt~ rq)))
         (b0rz (/~ 1 (+~ 1 alpha)))
         (a0 (*~ i 0.5 b0rz))
         (a1 (*~ (if (eq :low low-hi) i (*~ -1 i)) b0rz))
         (b1 (*~ cos-w0 2 b0rz))
         (b2 (*~ (-~ 1 alpha) (*~ -1 b0rz))))
    (list a0 a1 a0 b1 b2)))

(defun b-pass4-new (low-hi new in freq rq)
  (declare (ignorable new))
  (let ((coefs (b-pass-sc low-hi freq rq)))
    (apply #'multinew #'sos.ar (apply #'sos.ar in coefs) coefs)))

(defugen (b-lowpass "BLowPass")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-hipass "BHiPass")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defugen (b-allpass "BAllPass")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defugen (b-bandpass "BBandPass")
    (&optional (in 0.0) (freq 1200.0) (bw 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq bw) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defugen (b-bandstop "BBandStop")
    (&optional (in 0.0) (freq 1200.0) (bw 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq bw) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defugen (b-peak-eq "BPeakEQ")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (db 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq db) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-lowshelf "BLowShelf")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (db 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq db) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defugen (b-hishelf "BHiShelf")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (db 0.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'pure-ugen in freq rq db) mul add)))
  :check-fn #'check-same-rate-as-first-input)


(defugen (b-lowpass4 "BLowPass4")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew #'b-pass4-new :low new in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)

(defugen (b-hipass4 "BHiPass4")
    (&optional (in 0.0) (freq 1200.0) (rq 1.0) (mul 1.0) (add 0.0))
  ((:ar (madd (multinew #'b-pass4-new :hi new in freq rq) mul add)))
  :check-fn #'check-same-rate-as-first-input)
