;; https://github.com/capital-G/sc_faust

(in-package :sc)
(export '(faust-all faust-init faust-send faust-load fuast-free faust-free-all faust.ar))

(defvar *faust-register-table* nil)

(defun faust-hash (name)
  (let* ((hash (sxhash name)))
    (* (mod hash (expt 2 20)) (sign hash))))



(defun faust-all ()
  "Returns a list of Faust DSP modules and their parameters sent to the current server."
  (loop for k being the hash-keys of *faust-register-table*
	  using (hash-value v)
	collect (list :name k :params (car v))))


(defun faust-init (&optional path)
  "Initiates the Faust compiler on the server by sending the server the path of the Faust libraries."
  (unless *faust-register-table*
    (setf *faust-register-table* (make-hash-table))
    (unless path
      (setf path (sc::full-pathname
		  (concatenate 'string
			       (find-if (lambda (path) (search "extensions" (string-downcase path))) *sc-plugin-paths*)
			       "sc_faust/externals/faustlibs"))))
    (format t "Set Faust Library path to ~a" path)
    (send-message *s* "/cmd" "faustlibpath" path)
    (sync)))




(defun parse-param-file (file)
  (let* ((params (remove ""
			 (uiop:split-string (alexandria:read-file-into-string file)
					    :separator '(#\NewLine))
			 :test #'string=)))
    (delete-file file)
    (flet ((parse-param (spec)
	     (let* ((params (uiop:split-string spec :separator '(#\$))))
	       (list :name (car params)
		     :default (read-from-string (second params))
		     :min (read-from-string (third params))
		     :max (read-from-string (fourth params))
		     :step (read-from-string (fifth params))))))
      
      (mapcar #'parse-param params))))



(defun faust-send (name code)
  "Registers and compiles the associated code on the server by sending an OSC message. May take time."
  (let* ((hash (faust-hash name))
	 (param-file-path (format nil "/tmp/param~d" hash))
	 (script-msg (osc:encode-message "/cmd" "faustscript" hash param-file-path code nil)))
    (if (< (length script-msg) (/ 65535 4)) (let* ((osc-device (osc-device *s*)))
					      (usocket:socket-send (sc-osc::socket osc-device) script-msg (length script-msg)
								   :port (sc-osc::port osc-device)
								   :host (sc-osc::host osc-device)))
      (progn
	(unless (is-local-p *s*)
	  (error "FaustDef ~a could not be added  to server because it is too big for sending via OSC and server is not local" name))
	(uiop:with-temporary-file (:stream stream :pathname pathname)
	  (format stream code)
	  (close stream)
	  (send-message *s* "/cmd" "faustfile" hash param-file-path (namestring pathname) nil))))
    (sync)
    (unless (uiop:file-exists-p param-file-path) 
      (warn "FaustGen ~a was not successfully compiled" name)
      (return-from faust-send))
    (setf (gethash name *faust-register-table*)
      (parse-param-file param-file-path))
    name))


(defun faust-load (name pathname)
  "Loads a faust file on the client side."
  (faust-send name (alexandria:read-file-into-string pathname)))


(defun faust-free (name)
  "Unregisters the script from the server and from the client."
  (send-message *s* "/cmd" "faustfree" (faust-hash name))
  (remhash name *faust-register-table*)
  (sync)
  name)


(defun faust-free-all ()
  "Frees all registered FaustDef instances from the client and server side."
  (send-message *s* "/cmd" "faustfreeall")
  (setf *faust-register-table* nil)
  (sync))



(defun faust.ar (num-output name &key inputs params)
  "Faust is a functional programming language for implementing signal process algorithms. In order to run Faust code on the SuperCollider server it is first necessary to register the code with a name using FaustDef. After the FaustDef instance has been sent to the server, the code will be compiled and will become accessible via Faust.

For further information and resources about the programming language, please take a look at their official website at https://faust.grame.fr/ ."
  (multiple-value-bind (spec registed-p)
      (gethash name *faust-register-table*)
    (assert registed-p nil "Can't found faust module ~a" name)
    (let* ((param-spec spec)
	   (inputs (alexandria:ensure-list inputs))
	   (params (alexandria:ensure-list params)))
      (when params
	(assert (evenp (length params)) nil "params should be pair of key, value"))
      (setf params
	(loop for (key value) on params by #'cddr
	      for spec = (find key param-spec :test #'string= :key (lambda (spec) (getf spec :name)))
	      for index = (position spec param-spec :test #'equal)
	      do (assert (and spec index) nil "Can't found param ~s in faust module ~a" key name)
	      append (list index
			   (clip (if (not (eql :audio (rate value))) (k2a.ar value) value)
				 (getf spec :min) (getf spec :max)))))
      (setf inputs (mapcar (lambda (sig) (if (not (eql :audio (rate sig))) (k2a.ar sig) sig)) inputs))
      (apply 'ugen-new "Faust" :audio 'multiout-ugen #'identity :bipolar
	     num-output
	     (faust-hash name)
	     (length inputs)
	     (/ (length params) 2)
	     (append inputs params)))))



