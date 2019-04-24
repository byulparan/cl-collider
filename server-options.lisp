(in-package :sc)

(defvar *sc-plugin-paths*)

(defstruct server-options
  (num-control-bus 16384)
  (num-audio-bus 1024)
  (num-input-bus 2)
  (num-output-bus 2)
  (block-size 64)
  (hardware-buffer-size 0)
  (hardware-samplerate 0)
  (num-sample-buffers 1024)
  (max-num-nodes 1024)
  (max-num-synthdefs 1024)
  (realtime-mem-size 8192)
  (num-wire-buffers 64)
  (num-random-seeds 64)
  (load-synthdefs-p 1)
  (publish-to-rendezvous-p 1)
  (max-logins 1)
  (verbosity 0)
  (ugen-plugins-path (mapcar #'full-pathname *sc-plugin-paths*))
  (device nil))
  

(defun build-server-options (server-options)
  (reduce #'append
    (mapcar (lambda (pair)
              (let ((param-name (first pair))
                    (param-value (funcall (second pair) server-options)))
                (when param-value
                  (list param-name
                        (if (stringp param-value)
                          param-value
                          (write-to-string param-value))))))
            (list '("-c" server-options-num-control-bus)
                  '("-a" server-options-num-audio-bus)
                  '("-i" server-options-num-input-bus)
                  '("-o" server-options-num-output-bus)
                  '("-z" server-options-block-size)
                  '("-S" server-options-hardware-samplerate)
                  '("-b" server-options-num-sample-buffers)
                  '("-n" server-options-max-num-nodes)
                  '("-d" server-options-max-num-synthdefs)
                  '("-m" server-options-realtime-mem-size)
                  '("-w" server-options-num-wire-buffers)
                  '("-r" server-options-num-random-seeds)
                  '("-D" server-options-load-synthdefs-p)
                  '("-R" server-options-publish-to-rendezvous-p)
                  '("-l" server-options-max-logins)
                  '("-V" server-options-verbosity)
                  '("-H" server-options-device)))
             
    :initial-value (let* ((paths (server-options-ugen-plugins-path server-options)))
                       (if (not paths) nil
                        (list "-U" (format nil
                                    #-windows "~{~a~^:~}"
                                                 #+windows "~{~a~^;~}"
                                    paths))))))
