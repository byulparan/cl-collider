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
  (ugen-plugins-path (mapcar #'full-pathname *sc-plugin-paths*)))

(defun build-server-options (server-options)
  (append
   (list "-c" (write-to-string (server-options-num-control-bus server-options))
	 "-a" (write-to-string (server-options-num-audio-bus server-options))
	 "-i" (write-to-string (server-options-num-input-bus server-options))
	 "-o" (write-to-string (server-options-num-output-bus server-options))
	 "-z" (write-to-string (server-options-block-size server-options))
	 ;;-Z hardware-buffer-size
	 "-S" (write-to-string (server-options-hardware-samplerate server-options))
	 "-b" (write-to-string (server-options-num-sample-buffers server-options))
	 "-n" (write-to-string (server-options-max-num-nodes server-options))
	 "-d" (write-to-string (server-options-max-num-synthdefs server-options))
	 "-m" (write-to-string (server-options-realtime-mem-size server-options))
	 "-w" (write-to-string (server-options-num-wire-buffers server-options))
	 "-r" (write-to-string (server-options-num-random-seeds server-options))
	 "-D" (write-to-string (server-options-load-synthdefs-p server-options))
	 "-R" (write-to-string (server-options-publish-to-rendezvous-p server-options))
	 "-l" (write-to-string (server-options-max-logins server-options))
	 "-V" (write-to-string (server-options-verbosity server-options)))
   (let* ((paths (server-options-ugen-plugins-path server-options)))
     (if (not paths) nil
       (list "-U" (format nil
			  #-windows "\"~{~a~^:~}\""
			  #+windows "\"~{~a~^;~}\""
			  paths))))))
