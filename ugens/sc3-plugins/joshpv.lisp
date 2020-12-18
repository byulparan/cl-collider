
(in-package #:sc)

(defgeneric calc-pv-recsize (buffer frame-size hop &optional sample-rate))

(defmethod calc-pv-recsize ((buffer buffer) frame-size hop &optional sample-rate)
  (calc-pv-recsize (* 1.0 (/ (frames buffer) (sr buffer))) frame-size hop sample-rate))

(defmethod calc-pv-recsize ((buffer number) frame-size hop &optional sample-rate)
  (let ((rawsize))
    (setf sample-rate (if sample-rate sample-rate 44100.0))
    (setf rawsize (* frame-size (ceil~ (/ (* buffer sample-rate) frame-size))))
    (+ (* rawsize (reciprocal hop)) 3)))


(def-pv-chain-ugen (pv-record-buf "PV_RecordBuf") (buffer recbuf &optional (offset 0.0) (run 0.0) (loop 0.0)
							  (hop 0.5) (wintype 0))
  (multinew new 'pv-chain-ugen buffer recbuf offset run loop hop wintype))

(def-pv-chain-ugen (pv-play-buf "PV_PlayBuf") (buffer playbuf rate &key (offset 0.0) (loop 0.0))
  (multinew new 'pv-chain-ugen buffer playbuf rate offset loop))

(def-pv-chain-ugen (pv-buf-rd "PV_BufRd") (buffer playbuf point)
  (multinew new 'pv-chain-ugen buffer playbuf point))
