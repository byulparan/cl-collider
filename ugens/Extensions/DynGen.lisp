;; https://github.com/capital-G/DynGen

(in-package :sc)

(export '(dyn-gen-all dyn-gen-send dyn-gen-load dyn-gen-free dyn-gen-free-all dyn-gen.ar))


(defvar *dyn-gen-register-table* (make-hash-table))


(defun dyn-gen-hash (name)
  (flet ((hash (name)
	   (let* ((h (sxhash name) ))
	     (* (mod h (expt 2 20)) (sign h)))))
    (let* ((h (hash name)))
      (loop for n in (remove name (alexandria:hash-table-keys *dyn-gen-register-table*))
	    when (= h (hash n))
	      do (error "Conflict hash key ~a and ~a." name n))
      h)))


(defun dyn-gen-all ()
  "Return all dyn-gen names"
  (loop for k being the hash-keys of *dyn-gen-register-table*
	  using (hash-value v)
	collect (list :name k :params v)))



(defun dyn-gen-code-remove-comment (code)
  (let* ((code-line (uiop:split-string code :separator (list #\newline))))
    (setf code-line (mapcar #'(lambda (line) (ppcre:regex-replace-all "\\/\\/.*?$" line "")) code-line)
	  code-line (mapcar #'(lambda (line) (ppcre:regex-replace-all "\\/\\*.*?\\*\\/"  line "")) code-line))
    (with-output-to-string (stream)
      (loop for line in code-line
	    do (format stream "~&~a" line)))))


;; extract param
(defun dyn-gen-code-extract-param (code)
  (let* ((result nil))
    (ppcre:do-scans (match-start match-end reg-starts reg-ends "[^(?:A-Za-z|\\_|$|]?(\\_(?:[A-Za-z]|[0-9]|_)+)" code)
      (pushnew (subseq code (aref reg-starts 0) (aref reg-ends 0)) result :test #'string=))
    (nreverse result)))



(defun dyn-gen-send (name code)
  "In order to run a DynGen script on the server it is necessary to first register it on the server under a given name, similar to a SynthDef.
If the code for an already existing name gets updated, all running instances of this code will also be updated. This allows to live-code DynGen scripts."
  (assert (and (boot-p *s*) (is-local-p *s*)))
  (setf code (dyn-gen-code-remove-comment code))
  (let* ((hash (dyn-gen-hash name))
	 (params (dyn-gen-code-extract-param code))
	 (message (append (list "/cmd" "dyngenscript" hash code (length params)) params (list nil)))
	 (script-msg (apply #'osc:encode-message message)))
    (if (< (length script-msg) (/ 65535 4)) (let* ((osc-device (osc-device *s*)))
					      (usocket:socket-send (sc-osc::socket osc-device) script-msg (length script-msg)
								   :port (sc-osc::port osc-device)
								   :host (sc-osc::host osc-device)))
      (progn
	(unless (is-local-p *s*)
	  (error "DynGenDef ~a could not be added  to server because it is too big for sending via OSC and server is not local" name))
	(uiop:with-temporary-file (:stream stream
				   :pathname p)
	  (format stream code)
	  (close stream)
	  (let* ((message (append (list "/cmd" "dyngenfile" hash (namestring p) (length params)) params (list nil))))
	    (apply #'send-message *s* message)))))
    (sync)
    (setf (gethash name *dyn-gen-register-table*) params)
    name))



(defun dyn-gen-load (name pathname)
  (dyn-gen-send name (alexandria:read-file-into-string pathname)))



(defun dyn-gen-free (name)
  "Unregisters a DynGen script from the server."
  (send-message *s* "/cmd" "dyngenfree" (dyn-gen-hash name))
  (remhash name *dyn-gen-register-table*)
  (sync)
  name)


(defun dyn-gen-free-all ()
  "Unregisters all DynGen scripts from the server."
  (send-message *s* "/cmd" "dyngenfreeall")
  (setf *dyn-gen-register-table* (make-hash-table))
  (sync))



(defun dyn-gen.ar (num-output name &key inputs params (update 1.0) (sync 0.0))
  "This UGen evaluates EEL2 (expression evaluation library/realtime compiler) code in a VM which runs on the server. This allows to write DSP code on the fly and perform single sample operations.

The code has to be registered using DynGenDef and follows a similar approach to SynthDef by registering a resource (code) under a given name. This also allows to update the evaluated code on the fly.

Each script can expose multiple outputs and the output for each channel can be written to via the variable out0, out1, ... When the script gets evaluated through DynGen it is also necessary to tell how many outputs of the script should be exposed."
  (multiple-value-bind (spec registed-p)
      (gethash name *dyn-gen-register-table*)
    (assert registed-p nil "Can't found dyn-gen module ~a" name)
    (let* ((param-spec spec)
	   (inputs (alexandria:ensure-list inputs))
	   (params (alexandria:ensure-list params)))
      (when params
	(assert (evenp (length params)) nil "params should be pair of key, value"))
      (setf params
	(loop for (key value) on params by #'cddr
	      for index = (or (position (format nil "_~a" key) param-spec :test #'string=)
			      (position key param-spec :test #'string=))
	      do (assert index nil "Can't found param ~s in dyn-gen module ~a" key name)
	      append (list index (if (not (eql :audio (rate value))) (k2a.ar value) value))))
      (setf inputs (mapcar (lambda (sig) (if (not (eql :audio (rate sig))) (k2a.ar sig) sig)) inputs))
      (apply 'ugen-new "DynGen" :audio 'multiout-ugen #'identity :bipolar
	     num-output
	     (dyn-gen-hash name)
	     update
	     sync
	     (length inputs)
	     (/ (length params) 2)
	     (append inputs params)))))






