(in-package :sc)

(defugen (disk-in "DiskIn")
    (chanls bufnum &optional (loop 0))
  ((:ar (multinew new 'multiout-ugen chanls bufnum loop))))

(defugen (v-disk-in "VDiskIn")
  (chanls bufnum &optional (rate 1) (loop 0) (send-id 0))
  ((:ar (multinew new 'multiout-ugen chanls bufnum rate loop send-id))))

(defugen (disk-out "DiskOut")
    (bufnum channels-array)
  ((:ar (multinew-list new 'ugen (append (list bufnum) (alexandria:ensure-list channels-array)))))
  :check-fn (lambda (ugen)
	      (when (eql (rate ugen) :audio)
		(loop for i from 1 below (length (inputs ugen))
		      do (unless (eql (rate (nth i (inputs ugen))) :audio)
			   (error (format nil "input was not audio rate : ~a" (nth i (inputs ugen)))))))))
