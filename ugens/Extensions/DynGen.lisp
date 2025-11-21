;; https://github.com/capital-G/DynGen

(in-package :sc)

(export '(list-all-dyn-gen make-dyn-gen dyn-gen.ar dyn-gen-rt.ar))


(defvar *dyn-gen-table* (make-hash-table))

(defun list-all-dyn-gen ()
  "Return all dyn-gen objects"
  (alexandria:hash-table-values *dyn-gen-table*))


(defstruct (dyn-gen
	    (:constructor %make-dyn-gen (name hash code)))
  name hash code)


(defun name-hash (name)
  (flet ((hash (name)
	   (mod (sxhash name) (expt 2 20))))
    (let* ((h (hash name)))
      (loop for n in (remove name (alexandria:hash-table-keys *dyn-gen-table*))
	    when (= h (hash n))
	      do (error "Conflict hash key ~a and ~a." name n))
      h)))


(defun make-dyn-gen (name code)
  "In order to run a DynGen script on the server it is necessary to first register it on the server under a given name, similar to a SynthDef.
If the code for an already existing name gets updated, all running instances of this code will also be updated. This allows to live-code DynGen scripts."
  (assert (and (boot-p *s*) (is-local-p *s*)))
  (let* ((hash (name-hash name))
	 (dyn-gen (%make-dyn-gen name hash code)))
    (setf (gethash name *dyn-gen-table*) dyn-gen)
    (uiop:with-temporary-file (:stream stream
			       :pathname p)
      (format stream "~a" code)
      (close stream)
      (send-message *s* "/cmd" "dyngenadd" (floatfy hash) (namestring p))
      (sync))
    dyn-gen))



(defun meta-dyn-gen (ugen-name num-outputs script realtime inputs)
  (let* ((hash (cond ((dyn-gen-p script) (dyn-gen-hash script))
		     (t (name-hash script)))))
    (apply #'ugen-new ugen-name :audio 'multiout-ugen #'identity :bipolar
	   num-outputs hash realtime
	   (mapcar (lambda (in) (if (eql :audio (rate in)) in
				  (k2a.ar in)))
		   inputs))))


(defun dyn-gen.ar (num-outputs script &rest inputs)
  "This UGen evaluates EEL2 (expression evaluation library/realtime compiler) code in a VM which runs on the server. This allows to write DSP code on the fly and perform single sample operations.

The code has to be registered using DynGenDef and follows a similar approach to SynthDef by registering a resource (code) under a given name. This also allows to update the evaluated code on the fly.

Each script can expose multiple outputs and the output for each channel can be written to via the variable out0, out1, ... When the script gets evaluated through DynGen it is also necessary to tell how many outputs of the script should be exposed."
  (meta-dyn-gen "DynGen" num-outputs script 0.0 inputs))



(defun dyn-gen-rt.ar (num-outputs script &rest inputs)
  "See DynGen for a full introduction.

DynGen defers the initialization of the EEL2 VM into a non-realtime thread. This introduces a delay of at least one block size. If therefore sample accurate sequencing becomes necessary, it is also possible to init the EEL2 VM within the realtime-audio thread, though this can lead to audio-dropouts and is therefore not advised. "
  (meta-dyn-gen "DynGenRT" num-outputs script 1.0 inputs))



