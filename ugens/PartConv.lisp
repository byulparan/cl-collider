;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2025.11.20 byulparan@gmail.com
;; 
;; 

(in-package #:sc)

(defugen (part-conv "PartConv")
    (in fftsize irbufnum &optional (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen in fftsize irbufnum) mul add))))



(defun part-conv-calc-buf-size (fftsize irbuffer)
  (let* ((partition-size (floor (/ fftsize 2)))
	 (siz (frames irbuffer)))
    (* fftsize (ceiling (/ siz partition-size)))))


(defun buffer-prepare-part-conv (buffer buf fftsize)
  (let* ((server (server buffer) ))
    (send-message server "/b_gen" (floatfy buffer) "PreparePartConv" (floatfy buf) fftsize)
    (sync server)
    buffer))


