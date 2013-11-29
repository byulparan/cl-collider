
(in-package #:collider)

(defclass synthdef ()
  ((name :initarg :name :accessor name)
   (controls :initarg :controls :initform nil :accessor controls)
   (control-names :initarg :control-names :initform nil :accessor control-names)
   (control-ugen-count :accessor control-ugen-count :initform 0)
   (children :initform nil :accessor children)
   (constants :initform nil :accessor constants)
   (max-local-bufs :initform nil :accessor max-local-bufs)
   (available :initform nil :accessor available)
   (width-first-ugens :initform nil :accessor width-first-ugens)
   (rewrite-in-progress :initform nil :accessor rewrite-in-progress)))

(defmethod print-object ((c synthdef) stream)
  (format stream "#<SynthDef:~a>" (name c)))

(defmethod add-ugen ((synthdef synthdef) (ugen ugen))
  (unless (rewrite-in-progress synthdef)
    (setf (synth-index ugen) (length (children synthdef)))
    (setf (width-first-antecedents ugen) (copy-list (width-first-ugens synthdef)))
    (alexandria:appendf (children synthdef) (list ugen)))
  ugen)

(defmethod replace-ugen ((synthdef synthdef) (a ugen) (b ugen))
  (unless (typep b 'ugen) (error "replaceUgen assumes a UGen"))
  (setf (width-first-antecedents b) (width-first-antecedents a))
  (setf (descendants b) (descendants a))
  (setf (synth-index b) (synth-index a))
  (setf (nth (position a (children synthdef)) (children synthdef)) b)
  (loop for item in (children synthdef)
	for i from 0
	do (when item
	     (loop for input in (inputs item) for j from 0
		   do (when (eql input a)
			(setf (nth j (inputs item)) b))))))

(defmethod check-inputs ((synthdef synthdef))
  (dolist (ugen (children synthdef))
    (funcall (check-fn ugen) ugen)))

(defmethod add-constant ((synthdef synthdef) (const float))
  (unless (find const (constants synthdef))
    (alexandria:appendf (constants synthdef) (list const))))

(defmethod collect-constants ((synthdef synthdef))
  (dolist (ugen (children synthdef))
    (collect-constants ugen)))

(defmethod init-topo-sort ((synthdef synthdef))
  (setf (available synthdef) nil)
  (dolist (ugen (children synthdef))
    (setf (antecedents ugen) nil (descendants ugen) nil))
  (dolist (ugen (children synthdef))
    (init-topo-sort ugen))
  (dolist (ugen (reverse (children synthdef)))
    (setf (descendants ugen) (sort (descendants ugen) #'< :key #'synth-index))
    (make-available ugen)))

(defun topo-logical-sort (synthdef)
  (let ((out-stack nil))
    (init-topo-sort synthdef)
    (loop while (> (length (available synthdef)) 0) do
      (setf out-stack (schedule (pop (available synthdef)) out-stack)))
    (setf (children synthdef) (nreverse out-stack))))

(defun index-ugens (synthdef)
  (loop for ugen in (children synthdef)
	for i from 0
	do (setf (synth-index ugen) i)))

(defmethod optimize-graph ((synthdef synthdef))
  (let ((old-size (length (children synthdef))))
    (init-topo-sort synthdef)
    (setf (rewrite-in-progress synthdef) t)
    (dolist (ugen (children synthdef))
      (optimize-graph ugen))
    (setf (rewrite-in-progress synthdef) nil)
    (unless (= old-size (length (children synthdef)))
      (index-ugens synthdef))))

(defmethod build-synthdef ((synthdef synthdef))
  (check-inputs synthdef)
  (optimize-graph synthdef)
  (collect-constants synthdef)
  (topo-logical-sort synthdef)
  (index-ugens synthdef)
  synthdef)

(defun write-synthdef-file (name encoded-synthdef)
  (let ((path (make-pathname :directory (su:get-fullpath *sc-synthdefs-path*) :name name :type "scsyndef")))
    (with-open-file (stream path :direction :output
				 :if-exists :supersede
				 :element-type '(unsigned-byte 8))
      (write-sequence encoded-synthdef stream))
    (namestring path)))


(defmethod load-synthdef ((synthdef synthdef) &optional (completion-message 0))
  (assert (is-local-p *s*) nil "server ~a is not in local-machine. load-synthdef is only can do it in localhost server." *s*)
  (send-message *s* "/d_load"
		(write-synthdef-file (name synthdef) (encode-synthdef synthdef))
		completion-message))

(defmethod recv-synthdef ((synthdef synthdef) &optional (completion-message 0))
  (let* ((name (name synthdef))
	 (data (encode-synthdef synthdef)))
    (cond ((> (/ usocket:+max-datagram-packet-size+ 4) (length data)) (send-message *s* "/d_recv" data completion-message))
	  ((is-local-p *s*) (progn (format t "~&~a too big for sending.  Retrying via synthdef file~%" name)
				   (send-message *s* "/d_load" (write-synthdef-file name data) completion-message)))
	  (t (error "~a too big for sending" name)))))

(defclass control (multiout-ugen) ())

(defmethod new1 ((ugen control) &rest inputs)
  (setf (inputs ugen) nil)
  (setf (channels ugen) (length (alexandria:flatten (first inputs))))
  (setf (special-index ugen) (second inputs))
  (add-to-synth ugen)
  (let ((i 0))
    (loop for control in (first inputs)
	  collect (unbubble (loop repeat (length (su:mklist control))
				  collect (make-instance 'proxy-output :source ugen
								       :rate (rate ugen)
								       :output-index i)
				  do (incf i))))))

(defun make-control (params rate)
  (if *synthdef* (progn
		   (alexandria:appendf (control-names *synthdef*)
			    (mapcar #!(list (first %) (length (su:mklist (second %)))) params))
		   (dolist (con params)
		     (assert (and (every #'identity con)
				  (every #'numberp (su:mklist (second con)))) nil "parameters are must be (name val) pair")
		     (dolist (controls (cdr con))
		       (dolist (control-val (su:mklist controls))
			 (alexandria:appendf (controls *synthdef*) (list (floatfy control-val))))))
		   (let ((control-object
			   (ugen-new "Control" rate 'control #'identity :bipolar (mapcar #'second params) (control-ugen-count *synthdef*))))
		     (incf (control-ugen-count *synthdef*) (length (alexandria:flatten (mapcar #'second params))))
		     control-object))
      (ugen-new "Control" rate 'control #'identity :bipolar  (mapcar #'second params) 0)))


(defmacro with-controls (params &body body)
  (if params (progn
	       (assert (and (every #!(= (length %) 2) params)
			    (every #!(symbolp (first %)) params)) nil "parameters are must be (name val) pair")
	       `(destructuring-bind ,(mapcar #'first params)
		    (make-control (list
				   ,@(mapcar #!(cons 'list (list (string-downcase (first %)) (second %))) params))
				  :control)
		  ,@body))
      `(progn ,@body)))

;;; build  --------------------------------------------------------------------------------------

(defun convert-code-table (atom)
  (case atom
    (abs 'sc::abs~)
    (floor 'sc::floor~)
    (ceil 'sc::ceil~)
    (sqrt 'sc::sqrt~)
    (exp 'sc::exp~)
    (sin 'sc::sin~)
    (cos 'sc::cos~)
    (tan 'sc::tan~)
    (tanh 'sc::tanh~)
    (expt 'sc::expt~)
    (+ 'sc::+~)
    (- 'sc::-~)
    (* 'sc::*~)
    (/ 'sc::/~)
    (round 'sc::round~)
    (< 'sc::<~)
    (> 'sc::>~)
    (<= 'sc::<=~)
    (max 'sc::max~)
    (if 'sc::if~)    
    (t atom)))

(defun convert-code (form)
  (cond ((null form) nil)
	((atom form) (convert-code-table form))
	(t (cons (convert-code (car form))
		 (convert-code (cdr form))))))




(defmacro synth-funcall-definition (name args)
  (alexandria:with-gensyms (next-id new-synth)
    `(defun ,name (&key ,@(su:cat args (list '(pos :head) '(to (synth-group *s*)) 'end-f)))
       (let* ((,next-id (get-next-id *s*))
	      (,new-synth (make-instance 'node :server *s* :id ,next-id :name ,(string-downcase name) :pos pos :to to)))
	 (prog1
	     (message-distribute 
	      ,new-synth
	      (make-synth-msg *s* ,(string-downcase name) ,next-id to pos
			      ,@(alexandria:flatten (mapcar #!(list (string-downcase (first %)) (first %)) args)))
	      *s*)
	   (when end-f
	     (setf (gethash ,next-id (end-node-handler *s*)) end-f)))))))

(defparameter *synth-definition-mode* :recv)

(defmacro defsynth (name params &body body)
  (alexandria:with-gensyms (synthdef)
    `(let* ((,synthdef (make-instance 'synthdef :name ,(string-downcase name)))
	    (*synthdef* ,synthdef))
       (with-controls (,@params)
	 ,@(convert-code body)
	 (build-synthdef ,synthdef)
	 (ecase *synth-definition-mode*
	   (:recv (recv-synthdef ,synthdef))
	   (:load (load-synthdef ,synthdef)))
	 (sync *s*)
	 (synth-funcall-definition ,name ,params)
	 ,synthdef))))

(defvar *temp-synth-name* "temp-synth")

(defmacro play (body &key (out-bus 0) (fade-time 0.02) (to 1) (pos :head))
  (alexandria:with-gensyms (synthdef result dt gate start-val env i_out completion-message node-id name has-fade-time-p)
    `(let* ((,name *temp-synth-name*)
	    (,has-fade-time-p nil)
	    (,synthdef (make-instance 'synthdef :name ,name))
	    (*synthdef* ,synthdef))
       (let ((,i_out (make-control (list (list "i_out" ,out-bus)) :scalar)))
	 (let ((,result ,(convert-code body)))
	   (unless (numberp ,result)
	     (setf ,has-fade-time-p t)
	     (let* ((,dt (first (make-control (list (list "fade-time" ,fade-time)) :control)))
		    (,gate (first (make-control (list (list "gate" 1.0)) :control)))
		    (,start-val (<=~ ,dt 0))
		    (,env (env-gen.kr
			   (env (list ,start-val 1 0) (list 1,1) :lin 1) :gate ,gate :level-scale 1
									 :level-bias 0.0 :time-scale ,dt
									 :act :free)))
	       (setf ,result (*~ ,env ,result))
	       (cond ((eql :audio (rate ,result)) (out.ar ,i_out ,result))
		     ((eql :control (rate ,result)) (out.kr ,i_out ,result))
		     (t (error "not ugen ~a in play" ,result)))))))
       (build-synthdef ,synthdef)
       (let* ((,node-id (get-next-id *s*))
	      (,completion-message (make-synth-msg *s* ,name ,node-id ,to ,pos))
	      (,completion-message (apply #'osc::encode-message (car ,completion-message) (cdr ,completion-message))))
	 (recv-synthdef ,synthdef ,completion-message)
	 (sync *s*)
	 (make-instance 'node :server *s* :id ,node-id :name *temp-synth-name* :pos ,pos :to ,to
			      :meta (list :has-fade-time-p ,has-fade-time-p))))))


(defparameter *node-proxy-table* (make-hash-table :size 256))

(defmacro proxy (key &optional body &key (fade-time 2.0) (pos :head) (to 1) (out-bus 0))
  (alexandria:with-gensyms (node)
    (if body `(progn
		(let ((,node (gethash ,key *node-proxy-table*))
		      (*temp-synth-name* ,(string-downcase key)))
		  (when (and ,node  (is-playing-p ,node))
		    (if (getf (meta ,node) :has-fade-time-p) (ctrl ,node :gate 0 :fade-time ,fade-time)
			(bye ,node)))
		  (setf (gethash ,key *node-proxy-table*)
			(play ,body :out-bus ,out-bus :fade-time ,fade-time :to ,to :pos ,pos))))
	`(gethash ,key *node-proxy-table*))))


;;; ======================================================================
;;; build to ByteArray
;;; ======================================================================

(defparameter +type-id+ (map '(vector (unsigned-byte 8)) #'char-code "SCgf"))
(defparameter *synthdef-version* 1)

(defun encode-synthdef (synthdef)
  (ecase *synthdef-version*
    (1 (to-byte-array-synthdef-1 synthdef))
    (2 (to-byte-array-synthdef-2 synthdef))))

(defun to-byte-array-synthdef-1 (synthdef)
  (flex:with-output-to-sequence (stream)
    (write-sequence +type-id+ stream)
    (write-sequence (osc::encode-int32 *synthdef-version*) stream)
    (write-sequence (osc::encode-int16 1) stream)
    (write-sequence (make-pstring (name synthdef)) stream)
    (write-sequence (osc::encode-int16 (length (constants synthdef))) stream)
    (dolist (const (constants synthdef))
      (write-sequence (osc::encode-float32 const) stream))
    (write-sequence (osc::encode-int16 (length (controls synthdef))) stream)
    (dolist (control (controls synthdef))
      (write-sequence (osc::encode-float32 control) stream))
    (write-sequence (osc::encode-int16 (length (control-names synthdef))) stream)
    (let ((i 0))
      (loop for name in (control-names synthdef)
	    do (write-sequence (make-pstring (first name)) stream)
	       (write-sequence (osc::encode-int16 i) stream)
	       (incf i (second name))))
    (write-sequence (osc::encode-int16 (length (children synthdef))) stream)
    (dolist (ugen (children synthdef))
      (write-def-ugen-version1 ugen stream))
    (write-sequence (osc::encode-int16 0) stream)))

(defun to-byte-array-synthdef-2 (synthdef)
  (flex:with-output-to-sequence (stream)
    (write-sequence +type-id+ stream)
    (write-sequence (osc::encode-int32 *synthdef-version*) stream)
    (write-sequence (osc::encode-int16 1) stream)
    (write-sequence (make-pstring (name synthdef)) stream)
    (write-sequence (osc::encode-int32 (length (constants synthdef))) stream)
    (dolist (const (constants synthdef))
      (write-sequence (osc::encode-float32 const) stream))
    (write-sequence (osc::encode-int32 (length (controls synthdef))) stream)
    (dolist (control (controls synthdef))
      (write-sequence (osc::encode-float32 control) stream))
    (write-sequence (osc::encode-int32 (length (control-names synthdef))) stream)
    (let ((i 0))
      (loop for name in (control-names synthdef)
	    do (write-sequence (make-pstring (first name)) stream)
	       (write-sequence (osc::encode-int32 i) stream)
	       (incf i (second name))))
    (write-sequence (osc::encode-int32 (length (children synthdef))) stream)
    (dolist (ugen (children synthdef))
      (write-def-ugen-version2 ugen stream))
    (write-sequence (osc::encode-int16 0) stream)))



