
(in-package #:sc)

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
  (unless (typep b 'ugen) (error "REPLACE-UGEN requires a UGen."))
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
  (let ((path (make-pathname :directory (full-pathname *sc-synthdefs-path*) :name name :type "scsyndef")))
    (with-open-file (stream path :direction :output
				 :if-exists :supersede
				 :element-type '(unsigned-byte 8))
      (write-sequence encoded-synthdef stream))
    (namestring path)))


(defmethod load-synthdef ((synthdef synthdef) node &optional (completion-message 0))
  (assert (is-local-p *s*) nil "Server ~a is not a local server, so it cannot load synthdefs from a file." *s*)
  (message-distribute node (list "/d_load" (write-synthdef-file (name synthdef) (encode-synthdef synthdef)) completion-message) *s*))

(defmethod recv-synthdef ((synthdef synthdef) node &optional (completion-message 0))
  (let* ((name (name synthdef))
	 (data (encode-synthdef synthdef)))
    (cond ((>= usocket:+max-datagram-packet-size+ (length data)) (message-distribute node (list "/d_recv" data completion-message) *s*))
    	  ((is-local-p *s*) (progn (format t "~&~a too big for sending. Retrying via synthdef file.~%" name)
				   (load-synthdef synthdef node completion-message)))
    	  (t (error "Synthdef ~a is too big to send." name)))))

(defclass control (multiout-ugen) ())

(defmethod new1 ((ugen control) &rest inputs)
  (setf (inputs ugen) (third inputs))
  (setf (channels ugen) (length (alexandria:flatten (first inputs))))
  (setf (special-index ugen) (second inputs))
  (add-to-synth ugen)
  (let ((i 0))
    (loop for control in (first inputs)
	  collect (unbubble (loop repeat (length (alexandria:ensure-list control))
				  collect (make-instance 'proxy-output :source ugen
								       :rate (rate ugen)
								       :output-index i)
				  do (incf i))))))


(defun add-controls (rate lag-p controls)
  (ugen-new (if lag-p "LagControl" "Control")
	    rate 'control #'identity :bipolar (mapcar #'second controls) (control-ugen-count *synthdef*)
	    (alexandria:flatten
	     (when lag-p
	       (mapcar (lambda (ctrl)
			 (let ((lag-value (alexandria:if-let ((value (getf ctrl :lag))) value 0))
			       (ctrl-value (second ctrl)))
			   (when (and (consp lag-value) (numberp ctrl-value))
			     (error "Single control with multiple lag values is not supported."))
			   (when (and (consp lag-value) (consp ctrl-value) (/= (length lag-value) (length ctrl-value)))
			     (error "Number of control values does not match the number of lag values."))
			   (when (and (consp ctrl-value) (atom lag-value))
			     (setf lag-value (make-list (length ctrl-value) :initial-element lag-value)))
			   lag-value))
		       controls)))))

(defun make-control (params rate)
  (assert (and (every #'stringp (mapcar #'first params))
  	       (every #'numberp (alexandria:flatten (mapcar #'second params)))
	       (every #'(lambda (rate) (or (not rate) (find rate (list :ar :tr :lag)))) (mapcar #'third params))))
  (labels ((make-ctrl (params)
	     (dolist (controls (mapcar #'second params))
	       (dolist (control-val (alexandria:ensure-list controls))
		 (alexandria:appendf (controls *synthdef*) (list (floatfy control-val)))))))
    (let* ((trig-controls (remove-if-not #'(lambda (p) (eql :tr (third p))) params))
	   (audio-controls (remove-if-not #'(lambda (p) (eql :ar (third p))) params))
	   (controls (remove-if #'(lambda (p) (find (third p) (list :tr :ar))) params))
	   (lag-p (some #'(lambda (c) (getf c :lag)) controls)))
      (alexandria:appendf (control-names *synthdef*)
			  (loop with control-names = (mapcar #'first (control-names *synthdef*))
				for controls in (append trig-controls audio-controls controls)
				and index = (control-ugen-count *synthdef*) then (incf index (length (alexandria:ensure-list (second controls))))
				do (when (find (first controls) control-names :test #'string=)
				     (error "Duplicate control name: ~s" (first controls)))
				collect (list (first controls) index)))
      (make-ctrl trig-controls) (make-ctrl audio-controls) (make-ctrl controls)
      (append
       (when trig-controls
	 (prog1 (ugen-new "TrigControl" rate 'control #'identity :bipolar
			  (mapcar #'second trig-controls)
			  (control-ugen-count *synthdef*))
	   (incf (control-ugen-count *synthdef*) (length (alexandria:flatten (mapcar #'second trig-controls))))))
       (when audio-controls
	 (prog1 (ugen-new "AudioControl" :audio 'control #'identity :bipolar
			  (mapcar #'second audio-controls)
			  (control-ugen-count *synthdef*))
	   (incf (control-ugen-count *synthdef*) (length (alexandria:flatten (mapcar #'second audio-controls))))))
       (when controls
	 (prog1 (add-controls rate lag-p controls)
	   (incf (control-ugen-count *synthdef*) (length (alexandria:flatten (mapcar #'second controls))))))))))

(defmacro with-controls (params &body body)
  (if params `(destructuring-bind ,(mapcar #'first (append
						    (remove-if-not (lambda (a) (eql :tr (third a))) params)
						    (remove-if-not (lambda (a) (eql :ar (third a))) params)
						    (remove-if (lambda (a) (or (eql :tr (third a)) (eql :ar (third a)))) params)))
		  (make-control (list ,@(mapcar (lambda (a)
						  (cons 'list (list (string-downcase (first a)) `(floatfy ,(second a)) (third a) (fourth a))))
						params))
				:control)
		,@body)
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
    (mod 'sc::mod~)
    (round 'sc::round~)
    (< 'sc::<~)
    (> 'sc::>~)
    (<= 'sc::<=~)
    (max 'sc::max~)
    (min 'sc::min~)
    (if 'sc::if~)
    (logand 'sc::logand~)
    (logior 'sc::logior~)
    (ash 'sc::ash~)
    (t atom)))

(defun convert-code (form &optional head)
  (cond ((null form) nil)
	((atom form) (if head
		(convert-code-table form)
		form))
	(t (cons (convert-code (car form) t)
		 (mapcar #'convert-code (cdr form))))))


(defmacro synth-funcall-definition (name args)
  (alexandria:with-gensyms (next-id new-synth)
    (let ((delim (position '&key args)))
      `(defun ,name (,@(mapcar #'(lambda (arg) (if (listp arg) (car arg) arg)) (if delim (subseq args 0 delim) args))
		     &key ,@(cat (mapcar #'(lambda (arg) (if (listp arg) (subseq arg 0 2) (list arg 0)))
					 (if delim (subseq args (1+ delim)) nil))
				 (list '(pos :head) '(to 1) 'end-f)))
	 (let* ((,next-id (get-next-id *s*))
		(,new-synth (make-instance 'node :server *s* :id ,next-id :name ,(string-downcase name) :pos pos :to to)))
	   (prog1
	       (message-distribute 
		,new-synth
		(make-synth-msg *s* ,(string-downcase name) ,next-id to pos
				,@(alexandria:flatten (mapcar #'(lambda (arg) (list (string-downcase arg) arg))
							      (mapcar #'(lambda (arg) (if (listp arg) (car arg) arg))
								      (remove '&key args)))))
		*s*)
	     (when end-f
	       (setf (gethash ,next-id (end-node-handler *s*)) end-f))))))))

(defparameter *synth-definition-mode* :recv)

(defmacro defsynth (name params &body body)
  (alexandria:with-gensyms (synthdef)
    `(let* ((,synthdef (make-instance 'synthdef :name ,(string-downcase name)))
	    (*synthdef* ,synthdef))
       (with-controls (,@(mapcar #'(lambda (p) (if (symbolp p) (list p 0) p)) (remove '&key params)))
	 ,@(convert-code body)
	 (build-synthdef ,synthdef)
	 (when (and *s* (boot-p *s*))
	   (ecase *synth-definition-mode*
	     (:recv (recv-synthdef ,synthdef nil))
	     (:load (load-synthdef ,synthdef nil)))
	   (sync)
	   (synth-funcall-definition ,name ,params))
	 ,synthdef))))

(defvar *temp-synth-name* "temp-synth")

(defmacro play (body &key (out-bus 0) (gain 1.0) (lag 1.0) (fade 0.02) (to 1) (pos :head))
  (alexandria:with-gensyms (synthdef result dt gate gain-sym lag-sym
				     start-val env node-id name is-signal-p outlets seqs node)
    `(let* ((,name *temp-synth-name*)
	    (,is-signal-p nil)
	    (,synthdef (make-instance 'synthdef :name ,name))
	    (*synthdef* ,synthdef))
       (labels ((,seqs (indx lists)
		  (nth (mod indx (length lists)) lists))
		(,outlets (f bus result gain lag)
		  (if (numberp gain) (funcall f bus (*~ result (var-lag.kr gain lag)))
		    (loop with bus = (alexandria:ensure-list bus)
		  	    with gain = (alexandria:ensure-list gain)
		  	    for i from 0 below (max (length bus) (length gain))
		  	    do (funcall f (,seqs i bus) (*~ (var-lag.kr (,seqs i gain) lag) result))))))
	 (let ((,result ,(convert-code body)))
	   (unless (numberp ,result)
	     (setf ,is-signal-p t)
	     (destructuring-bind (,dt ,gate ,gain-sym ,lag-sym)
		 (make-control (list (list "fade" ,fade) (list "gate" 1.0) (list "gain" ,gain) (list "lag" ,lag)) :control)
	       (let* ((,start-val (<=~ ,dt 0))
		      (,env (env-gen.kr
			     (env (list ,start-val 1 0) (list 1,1) :lin 1) :gate ,gate :level-scale 1 :level-bias 0.0
									   :time-scale ,dt :act :free)))
		 (setf ,result (*~ ,env ,result))
		 (cond ((eql :audio (rate ,result)) (,outlets 'out.ar ,out-bus ,result ,gain-sym ,lag-sym))
		       ((eql :control (rate ,result)) (,outlets 'out.kr ,out-bus ,result ,gain-sym ,lag-sym))
		       (t (error "Play: ~a is not a UGen." ,result))))))))
       (build-synthdef ,synthdef)
       (let* ((,node-id (get-next-id *s*))
	      (,node (make-instance 'node :server *s* :id ,node-id :name *temp-synth-name* :pos ,pos :to ,to :meta (list :is-signal-p ,is-signal-p))))
	 (recv-synthdef ,synthdef ,node (apply 'osc:encode-message (make-synth-msg *s* ,name ,node-id ,to ,pos)))
	 (sync)
	 ,node))))


(defmacro proxy (key &optional body &key (gain 1.0) (fade 0.5) (pos :head) (to 1) (out-bus 0))
  (alexandria:with-gensyms (node)
    `(let ((,node (gethash ,key (node-proxy-table *s*))))
       (labels ((clear-node ()
		  (when (and ,node  (is-playing-p ,node))
		    (if (getf (meta ,node) :is-signal-p) (ctrl ,node :gate 0 :fade ,fade)
			(bye ,node)))))
	 ,(if body `(progn
		      (let ((*temp-synth-name* ,(string-downcase key)))
			(prog1 (setf (gethash ,key (node-proxy-table *s*))
				     (play ,body :out-bus ,out-bus :fade ,fade :to ,to :pos ,pos :gain ,gain))
			  (clear-node))))
	      `(clear-node))))))


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
    (dolist (name (control-names synthdef))
      (write-sequence (make-pstring (first name)) stream)
      (write-sequence (osc::encode-int16 (second name)) stream))
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
    (dolist (name (control-names synthdef))
      (write-sequence (make-pstring (first name)) stream)
      (write-sequence (osc::encode-int32 (second name)) stream))
    (write-sequence (osc::encode-int32 (length (children synthdef))) stream)
    (dolist (ugen (children synthdef))
      (write-def-ugen-version2 ugen stream))
    (write-sequence (osc::encode-int16 0) stream)))



