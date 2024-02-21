
(in-package #:sc)

(defclass synthdef ()
  ((name :initarg :name :accessor name)
   (controls :initarg :controls :initform nil :accessor controls)
   (named-controls :initform nil :accessor named-controls)
   (control-names :initarg :control-names :initform nil :accessor control-names)
   (control-ugen-count :accessor control-ugen-count :initform 0)
   (children :initform nil :accessor children)
   (constants :initform nil :accessor constants)
   (max-local-bufs :initform nil :accessor max-local-bufs)
   (available :initform nil :accessor available)
   (width-first-ugens :initform nil :accessor width-first-ugens)
   (rewrite-in-progress :initform nil :accessor rewrite-in-progress)))

(defmethod print-object ((c synthdef) stream)
  (format stream "#<~s :name ~s>" 'synthdef (name c)))

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

(defmethod add-constant ((synthdef synthdef) (const single-float))
  (unless (find const (constants synthdef))
    (alexandria:appendf (constants synthdef) (list const))))

(defmethod add-constant ((synthdef synthdef) (const t))
  (error "can't available input: ~a" const))

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
  (let ((path (merge-pathnames (make-pathname :name name :type "scsyndef")
			       (full-pathname *sc-synthdefs-path*))))
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
				  collect (make-instance 'proxy-output
					    :source ugen
					    :rate (rate ugen)
					    :signal-range (signal-range ugen)
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

(defun make-control (params)
  (dolist (param params)
    (destructuring-bind (name value &optional rate lag-value)
	param
      (assert (stringp name) (name) "Control Name \"~a\" should be string" name)
      (assert (every #'numberp (alexandria:ensure-list value)) (value) "Control Value \"~a\" should be number" value)
      (when rate
	(assert (find rate (list :ar :tr :lag)))
	(when (eql rate :lag) (assert lag-value (name) "Lag Control \"~a\" should be has lag-value" name))
	(when (find rate (list :tr :ar))
	  (assert (not lag-value) (rate lag-value) "Control Rate ~a not support extra control ~a" rate lag-value)))))
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
	 (prog1 (ugen-new "TrigControl" :control 'control #'identity :bipolar
			  (mapcar #'second trig-controls)
			  (control-ugen-count *synthdef*))
	   (incf (control-ugen-count *synthdef*) (length (alexandria:flatten (mapcar #'second trig-controls))))))
       (when audio-controls
	 (prog1 (ugen-new "AudioControl" :audio 'control #'identity :bipolar
			  (mapcar #'second audio-controls)
			  (control-ugen-count *synthdef*))
	   (incf (control-ugen-count *synthdef*) (length (alexandria:flatten (mapcar #'second audio-controls))))))
       (when controls
	 (prog1 (add-controls :control lag-p controls)
	   (incf (control-ugen-count *synthdef*) (length (alexandria:flatten (mapcar #'second controls))))))))))

(defmacro with-controls (params &body body)
  (if params `(destructuring-bind ,(mapcar #'first (append
						    (remove-if-not (lambda (a) (eql :tr (third a))) params)
						    (remove-if-not (lambda (a) (eql :ar (third a))) params)
						    (remove-if (lambda (a) (or (eql :tr (third a)) (eql :ar (third a)))) params)))
		  (make-control (list ,@(mapcar (lambda (a)
						  (cons 'list (list (string-downcase (first a)) `(floatfy ,(second a)) (third a) (fourth a))))
						params)))
		,@body)
      `(progn ,@body)))


(defun named-control (name rate &optional value lag)
  "A NamedControl directly combines a ControlName and a Control UGen conveniently. Also this makes it safe even if several identical controls exist"
  (when (and (consp lag) (find 0 lag :test #'equalp)) (error "'lagTime' has bad input"))
  (when (eql rate :tr) (setf lag nil))
  (let* ((name (string-downcase name))
	 (lag (if (and (numberp lag) (zerop lag)) nil lag))
	 (rate (ecase rate
		 (:kr :control)
		 (:ar :audio)
		 (:tr :trig)))
	 (reg-control (cadr (assoc name (named-controls *synthdef*) :test #'string=))))
    (if reg-control (let* ((ugen (getf reg-control :ugen))
			   (fixed-lag (getf reg-control :fixed-lag)))
		      (assert (eql rate (getf reg-control :rate)) nil
			      "NamedControl: cannot have more than one set of rates in the same control.")
		      (when value
			(assert (equalp (alexandria:ensure-list (floatfy value))
					(getf reg-control :value)) nil
					"NamedControl: cannot have more than one set of default values in the same control."))
		      (when (and lag fixed-lag)
			(assert (equalp (alexandria:ensure-list lag) (getf reg-control :lag)) nil
				"NamedControl: cannot have more than one set of fixed lag values in the same control."))
		      (cond ((and lag (not fixed-lag) (equal rate :control)) (lag.kr ugen lag))
			    ((and lag (not fixed-lag) (equal rate :audio)) (lag.ar ugen lag))
			    (t ugen)))
      (let* ((fixed-lag (and lag (every #'numberp (alexandria:ensure-list lag)))))
	(when (and (eql rate :control) value fixed-lag)
	  (when (and (numberp value) (consp lag) (/= 1 (length lag)))
	    (error "Single control with multiple lag values is not supported."))
	  (when (and (consp value) (consp lag) (/= (length lag) (length value)))
	    (error "Number of control values does not match the number of lag values."))
	  (when (and (consp value) (atom lag))
	    (setf lag (make-list (length value) :initial-element lag))))
	(let* ((value (alexandria:ensure-list (floatfy (if value value 0.0))))
	       (lag (alexandria:ensure-list lag))
	       (lag (if fixed-lag (subseq lag 0 (length value)) lag))
	       (ugen-name (ecase rate
			    (:control (if fixed-lag "LagControl" "Control"))
			    (:audio "AudioControl")
			    (:trig "TrigControl")))
	       (ugen (unbubble (ugen-new ugen-name (if (eql rate :trig) :control rate) 'control #'identity :bipolar
					 value (control-ugen-count *synthdef*) lag))))
	  (alexandria:appendf (controls *synthdef*) value)
	  (alexandria:appendf (control-names *synthdef*) (list (list name (control-ugen-count *synthdef*))))
	  (incf (control-ugen-count *synthdef*) (length value))
	  (push (list name (list :rate rate :value value :lag lag :fixed-lag fixed-lag :ugen ugen)) (named-controls *synthdef*))
	  (when (and (eql rate :audio) lag) (setf ugen (unbubble (lag.ar ugen lag))))
	  (when (and (eql rate :control) lag (not fixed-lag)) (setf ugen (lag.kr ugen lag)))
	  (pushnew (list name (unbubble value)) (synthdef-metadata (name *synthdef*) :controls))
	  ugen)))))

(defun kr (name &optional value lag)
  "shortcut for named-control(control rate)"
  (named-control name :kr value lag))


;;; build --------------------------------------------------------------------------------------

(defvar *synthdef-function-table* '((abs sc::abs~)
				    (floor sc::floor~)
				    (ceil sc::ceil~)
				    (sqrt sc::sqrt~)
				    (exp sc::exp~)
				    (sin sc::sin~)
				    (cos sc::cos~)
				    (tan sc::tan~)
				    (tanh sc::tanh~)
				    (expt sc::expt~)
				    (+ sc::+~)
				    (- sc::-~)
				    (* sc::*~)
				    (/ sc::/~)
				    (mod sc::mod~)
				    (round sc::round~)
				    (< sc::<~)
				    (> sc::>~)
				    (<= sc::<=~)
				    (>= sc::>=~)
				    (max sc::max~)
				    (min sc::min~)
				    (logand sc::logand~)
				    (logior sc::logior~)
				    (ash sc::ash~))
  "Table mapping function names to their synthdef-compatible equivalents. Used by `convert-code' and `synthdef-equivalent-function' to convert lisp functions in synthdef bodies to ugen functions.")

(defun synthdef-equivalent-function (function)
  "Get the synthdef-compatible equivalent of FUNCTION from `*synthdef-function-table*'. If FUNCTION is not found in the table, return it unchanged."
  (or (cadr (assoc function *synthdef-function-table*))
      function))

(defun (setf synthdef-equivalent-function) (value function)
  "Set the synthdef-compatible equivalent of FUNCTION to VALUE in `*synthdef-function-table*'. If VALUE is nil, remove the entry for FUNCTION from the table."
  (if value
      (push (list function value) *synthdef-function-table*)
      (alexandria:removef *synthdef-function-table* function :key #'car)))

(defun synthdef-embeddable-body (synth args)
  "Make an \"embeddable\" synthdef body, to allow `synth' to be used inside `synthdef'."
  (let* ((body (synthdef-metadata synth :body))
	 (args-keys (loop :for (k v) :on args :by #'cddr :collect k))
	 (unprovided-args (append
			   (loop :for (k v) :on args :by #'cddr
				 :for rk := (intern (symbol-name k))
				 :unless (eql rk v)
				   :collect (list rk v))
			   (mapcan (lambda (c)
				     (unless (or (string= (car c) 'out)
						 (member (car c) args-keys :test #'string=))
				       (list (if (string= (car c) 'amp)
						 (list (car c) 1) ;; default to full volume for embedded synths
						 c))))
				   (synthdef-metadata synth :controls)))))
    (labels ((parse-item (body)
               (if (listp body)
                   (if (member (car body) (list 'out.ar 'out.kr 'replace-out.ar 'replace-out.kr 'x-out.ar 'x-out.kr 'scope-out.ar 'scope-out.kr 'scope-out2.ar 'scope-out2.kr))
		       (caddr body)
		       (mapcar #'parse-item body))
		   body)))
      `(let (,@unprovided-args)
	 ,@(parse-item body)))))

(defun convert-code (form &optional head)
  (cond ((null form) nil)
	((atom form) (if head
			 (synthdef-equivalent-function form)
			 form))
	((position (car form) (list 'let 'let*)) ;; avoid converting names of local bindings
	 `(,(car form) ,(mapcar (lambda (item)
				  (if (atom item)
                                      item
				      `(,(car item) ,@(convert-code (cdr item)))))
				(cadr form))
           ,@(convert-code (cddr form))))
	((eql (car form) 'destructuring-bind)
	 `(,(car form) ,(cadr form) ,(caddr form)
           ,@(convert-code (cdddr form))))
	((eql (car form) 'synth)
	 (let ((synth (cadr form))
	       (args (cddr form)))
	   (convert-code
	    (synthdef-embeddable-body (if (and (listp synth)
					       (eql 'quote (car synth)))
					  (cadr synth)
					  synth)
				      args))))
	(t (cons (convert-code (car form) t)
		 (mapcar #'convert-code (cdr form))))))

(defparameter *synth-definition-mode* :recv)

(defparameter *synthdef-metadata* (make-hash-table)
  "Metadata for each synthdef, such as its name, controls, body, etc.")

(defun synthdef-metadata (synth &optional key)
  "Get metadata about the synthdef with the name of SYNTH. When KEY is provided, return that specific item from the metadata (i.e. controls, body, etc)."
  (let ((metadata (gethash (as-keyword (if (typep synth 'node) (name synth) synth)) *synthdef-metadata*)))
    (if key
        (getf metadata (as-keyword key))
      metadata)))

#-ecl
(uiop:with-deprecation (:style-warning)
  (defun get-synthdef-metadata (synth &optional key)
    "Deprecated alias for `synthdef-metadata'."
    (synthdef-metadata synth key)))

(defun (setf synthdef-metadata) (value synth key)
  "Set a metadatum for the synthdef SYNTH."
  (setf (getf (gethash (as-keyword synth) *synthdef-metadata*) (as-keyword key)) value))

#-ecl
(uiop:with-deprecation (:style-warning)
  (defun set-synthdef-metadata (synth key value)
    "Deprecated alias for `(setf synthdef-metadata)'."
    (setf (synthdef-metadata synth key) value)))

(defmacro defsynth (name params &body body)
  (setf params (mapcar (lambda (param) (if (consp param) param (list param 0.0))) params))
  (alexandria:with-gensyms (synthdef)
    `(progn
       (setf (synthdef-metadata ',name :name) ',name
	     (synthdef-metadata ',name :controls) (mapcar (lambda (param) (append (list (car param)) (cdr param)))
							  ',params)
	     (synthdef-metadata ',name :body) ',body)
       (let* ((,synthdef (make-instance 'synthdef :name ,(string-downcase name)))
	      (*synthdef* ,synthdef))
	 (with-controls (,@params)
	   ,@(convert-code body)
	   (build-synthdef ,synthdef)
	   (when (and *s* (boot-p *s*))
	     (ecase *synth-definition-mode*
	       (:recv (recv-synthdef ,synthdef nil))
	       (:load (load-synthdef ,synthdef nil)))
	     (sync))
	   ,synthdef)))))

(defvar *temp-synth-name* "temp-synth")

(defmacro play (body &key id (out-bus 0) (gain 1.0) (lag 1.0) (fade 0.02) (to 1) (pos :head))
  (alexandria:with-gensyms (synthdef result dt buses gate gain-sym lag-sym
                                     start-val env node-id name fade-time outlets seqs node)
    `(let* ((,name *temp-synth-name*)
            (,fade-time nil)
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
           (unless (eql :scalar (rate ,result))
             (setf ,fade-time ,fade)
             (destructuring-bind (,dt ,buses ,gate ,gain-sym ,lag-sym)
                 (make-control (list (list "fade" ,fade) (list "out-bus" ,out-bus)
				     (list "gate" 1.0) (list "gain" ,gain) (list "lag" ,lag)))
               (let* ((,start-val (<=~ ,dt 0))
                      (,env (env-gen.kr
                             (env (list ,start-val 1 0) (list 1 1) :lin 1) :gate ,gate :level-scale 1 :level-bias 0.0
                             :time-scale ,dt :act :free)))
                 (setf ,result (*~ ,env ,result))
                 (cond ((eql :audio (rate ,result)) (,outlets 'out.ar ,buses ,result ,gain-sym ,lag-sym))
                       ((eql :control (rate ,result)) (,outlets 'out.kr ,buses ,result ,gain-sym ,lag-sym))
                       (t (error "Play: ~a is not a UGen." ,result))))))))
       (build-synthdef ,synthdef)
       (let* ((,node-id (or ,id (get-next-id *s*)))
              (,node (make-instance 'node :server *s* :id ,node-id :name *temp-synth-name* :pos ,pos :to ,to
				    :meta (list :fade-time ,fade-time))))
         (recv-synthdef ,synthdef ,node (apply 'sc-osc::encode-message (make-synth-msg *s* ,name ,node-id ,to ,pos)))
         (sync)
         ,node))))

(defun synth (name &rest args &key id (pos :head) (to 1) &allow-other-keys)
  "Start a synth by name.

Optionally takes keyword arguments ID POS and HEAD.

:ID specifies the synths id.

:POS specifies the synths position in the node tree relative to the node passed
via :TO, possible values are :HEAD, :TAIL, :BEFORE, :AFTER.

:TO passes a node either as a `node' object or node id."
  (let* ((name-string (string-downcase (symbol-name name)))
         (next-id (or id (get-next-id *s*)))
         (new-synth (make-instance 'node :server *s* :id next-id :name name-string :pos pos :to to))
         (args (loop :for (arg val) :on args :by #'cddr
		     :unless (member arg '(:id :to :pos))
		     :append (list (string-downcase arg) (floatfy val)))))
    (message-distribute new-synth
			(apply #'make-synth-msg *s* name-string next-id to pos args)
			*s*)))

(defun get-controls-list (form)
  "Scan FORM for (with-controls ...) and return the list of controls if it exists, or NIL otherwise."
  (cond ((null form) nil)
        ((listp form)
         (if (eq (car form) 'with-controls)
             (cadr form)
           (loop :for i :in (cdr form)
                 :for res = (get-controls-list i)
                 :unless (null res)
                   :return res)))))

(defmacro proxy (key body &key id (gain 1.0) (fade .5) (rel 2) (pos :head) (to 1) (out-bus 0))
  (alexandria:with-gensyms (node node-alive-p d-key)
    `(let* ((,node (gethash ,key (node-proxy-table *s*)))
	    (,node-alive-p (when ,node (if (typep *s* 'nrt-server) t (is-playing-p ,node)))))
       ,(if body
	    (alexandria:once-only (id fade)
	      `(labels ((clear-node ()
			  (when ,node-alive-p
			    (if (getf (meta ,node) :fade-time) (ctrl ,node :gate 0 :fade (* ,fade ,rel))
			      (free ,node)))))
		 (when (and (typep *s* 'rt-server) (is-playing-p ,id))
		   (error "already running id ~d~%" ,id))
		 (let ((,d-key (string-downcase ,key)))
		   (setf (synthdef-metadata ,d-key :name) ,d-key)
		   (let ((controls (get-controls-list ',body)))
		     (setf (synthdef-metadata ,d-key :controls) (mapcar (lambda (param) (append (list (car param)) (cdr param))) controls)))
		   (setf (synthdef-metadata ,d-key :body) ',body))
		 (let ((*temp-synth-name* (string-downcase ,key)))
		   (prog1 (setf (gethash ,key (node-proxy-table *s*))
			    (play ,body :id ,id :out-bus ,out-bus :fade ,fade :to ,to :pos ,pos :gain ,gain))
		     (clear-node)))))
          `(when ,node-alive-p
	     (free ,node))))))

(defun proxy-ctrl (key &rest params &key &allow-other-keys)
  (let* ((node (gethash key (node-proxy-table *s*)))
	 (node-alive-p (and node (if (typep *s* 'nrt-server) t (is-playing-p node)))))
    (assert node nil "can't find proxy ~a" key)
    (let* ((fade-time (getf (meta node) :fade-time)))
      (flet ((clear-node ()
	       (if fade-time  (ctrl node :gate 0 :fade (or (getf params :fade) fade-time))
		 (free node))))
	(let* ((new-node  (apply #'synth key params)))
	  (setf (meta new-node) (list :fade-time fade-time))
	  (prog1
	      (setf (gethash key (node-proxy-table *s*)) new-node)
	    (when node-alive-p
	      (clear-node))))))))


;;; ======================================================================
;;; build to ByteArray
;;; ======================================================================

(defparameter +type-id+ (map '(vector (unsigned-byte 8)) #'char-code "SCgf"))
(defparameter *synthdef-version* 2)

(defun encode-synthdef (synthdef)
  (ecase *synthdef-version*
    (1 (to-byte-array-synthdef-1 synthdef))
    (2 (to-byte-array-synthdef-2 synthdef))))

(defun to-byte-array-synthdef-1 (synthdef)
  (flex:with-output-to-sequence (stream)
    (write-sequence +type-id+ stream)
    (write-sequence (osc::encode-int32 *synthdef-version*) stream)
    (write-sequence (sc-osc::encode-int16 1) stream)
    (write-sequence (make-pstring (name synthdef)) stream)
    (write-sequence (sc-osc::encode-int16 (length (constants synthdef))) stream)
    (dolist (const (constants synthdef))
      (write-sequence (osc::encode-float32 const) stream))
    (write-sequence (sc-osc::encode-int16 (length (controls synthdef))) stream)
    (dolist (control (controls synthdef))
      (write-sequence (osc::encode-float32 control) stream))
    (write-sequence (sc-osc::encode-int16 (length (control-names synthdef))) stream)
    (dolist (name (control-names synthdef))
      (write-sequence (make-pstring (first name)) stream)
      (write-sequence (sc-osc::encode-int16 (second name)) stream))
    (write-sequence (sc-osc::encode-int16 (length (children synthdef))) stream)
    (dolist (ugen (children synthdef))
      (write-def-ugen-version1 ugen stream))
    (write-sequence (sc-osc::encode-int16 0) stream)))

(defun to-byte-array-synthdef-2 (synthdef)
  (flex:with-output-to-sequence (stream)
    (write-sequence +type-id+ stream)
    (write-sequence (osc::encode-int32 *synthdef-version*) stream)
    (write-sequence (sc-osc::encode-int16 1) stream)
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
    (write-sequence (sc-osc::encode-int16 0) stream)))



