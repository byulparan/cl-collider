(in-package #:sc)

(defugen (hilbert "Hilbert") (in &optional (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'multiout-ugen 2 in)
	      mul add))))

(defun hilbert-fir.ar (in buffer)
  (let* ((fft (fft buffer in))
	 (fft (pv-phase-shift90 fft))
	 (delay (buf-dur.kr buffer)))
    (list (delay-n.ar in delay delay) (ifft.ar fft))))

(export 'hilbert-fir.ar)

(defugen (freq-shift "FreqShift")
    (in &optional (freq 0.0) (phase 0.0) (mul 1.0) (add 0))
  ((:ar (madd (multinew new 'ugen in freq phase) mul add))))
