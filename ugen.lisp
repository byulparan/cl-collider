
(in-package #:sc)

;;; header ---------------------------------------------------------------------------------------------------------

(defvar *synthdef* nil)

(defconstant +inf+
  #+ccl 1E++0 
  #+sbcl sb-ext:single-float-positive-infinity
  #+ecl ;; ext:single-float-positive-infinity // this is right value. but it signal a #<FLOATING-POINT-OVERFLOW>. maybe it's ecl's bug
  ext::most-positive-single-float)

(defgeneric new1 (ugen &rest inputs))

(defgeneric add-to-synth (ugen))

(defgeneric init-topo-sort (ugen))

(defgeneric add-ugen (synthdef ugen))

(defgeneric width-first-ugens (synthdef))

(defgeneric (setf width-first-ugens) (value synthdef))

(defgeneric add-constant (synthdef float))

(defgeneric available (synthdef))
(defgeneric (setf available) (value synthdef))

(defgeneric children (synthdef))
(defgeneric (setf children) (value synthdef))

(defgeneric constants (synthdef))

(defgeneric rate (ugen))
(defgeneric unbubble (list))

(defgeneric optimize-graph (ugen))

;;; create UGen -------------------------------------------------------------------------

(defun lst-operation (lst &optional constructor)
  (labels ((list-ref (lst n)
	     (if (null lst) nil
		 (nth (mod n (length lst)) lst)))
	   (mulnew (args)
	     (let ((size 0) (results nil))
	       (dolist (item args)
		 (when (listp item) (setf size (max size (length item)))))
	       (if (zerop size) (return-from mulnew (if constructor (funcall constructor args) args))
		   (dotimes (i size)
		     (let ((newargs nil))
		       (dolist (item args)
			 (alexandria:appendf newargs (list (if (listp item) (list-ref item i) item))))
		       (alexandria:appendf results (list (mulnew newargs))))))
	       results)))
    (mulnew lst)))

(defun multinew-list (new cls inputs)
  (lst-operation inputs (lambda (args) (apply new cls args))))

(defun multinew (new cls &rest inputs)
  (multinew-list new cls inputs))

(defun ugen-new (name rate cls check-fn signal-range &rest inputs)
  (let* ((ugen (make-instance cls :name name
				  :synthdef *synthdef*
				  :rate rate
				  :check-fn check-fn
				  :signal-range signal-range)))
    (apply #'new1 ugen inputs)))

;;; UGen rate -------------------------------------------------------------------------------
(defun rate-number (rate)
  (ecase (rate rate)
    (:audio 2)
    (:control 1)
    (:scalar 0)
    (:demand 3)))

(defun number-rate (number)
  (ecase number
    (2 :audio)
    (1 :control)
    (0 :scalar)
    (3 :demand)))

(defmethod rate ((ugen number))
  :scalar)

(defmethod rate ((ugen list))
  (if (null ugen)
      :scalar
      (let ((rate-lst (mapcar #'(lambda (u) (string (rate u))) (alexandria:flatten ugen))))
        (let ((ret (intern (first (sort rate-lst #'string-lessp)) :keyword)))
          ret))))

(defmethod rate ((ugen t))
  (if (eq ugen t) :scalar))


(defun act (act)
  (ecase act
    (:no-action 0)
    (:pause 1)
    (:free 2)
    (:free-and-before 3)
    (:free-and-after 4)
    (:free-and-group-before 5)
    (:free-and-group-after 6)
    (:free-upto-this 7)
    (:free-from-this-on 8)
    (:free-pause-before 9)
    (:free-pause-after 10)
    (:free-and-group-before-deep 11)
    (:free-and-group-after-deep 12)
    (:free-children 13)
    (:free-group 14)))

;;; UGen  ---------------------------------------------------------------------------------------

(defclass ugen ()
  ((name :initarg :name :accessor name)
   (synthdef :initarg :synthdef :accessor synthdef)
   (inputs :accessor inputs)
   (rate :initarg :rate :accessor rate)
   (synth-index :initarg :synth-index :initform -1 :accessor synth-index)
   (check-fn :initarg :check-fn :reader check-fn)
   (signal-range :initarg :signal-range :reader signal-range)
   (special-index :initform 0 :accessor special-index)
   (antecedents :initform nil :accessor antecedents)
   (descendants :initform nil :accessor descendants)
   (width-first-antecedents :initarg :width-first-antecedents :initform nil :accessor width-first-antecedents)))

(defclass nooutput-ugen (ugen)
  ())

(defmethod print-object ((c ugen) stream)
  (format stream "#<~s ~a.~a>" 'ugen (name c) (case (rate c)
                                                (:audio "ar")
                                                (:control "kr")
                                                (:scalar "ir"))))

(defmethod new1 ((ugen ugen) &rest inputs)
  (setf (inputs ugen) inputs)
  (add-to-synth ugen))

(defmethod add-to-synth ((ugen ugen))
  (if *synthdef* (add-ugen (synthdef ugen) ugen)
      ugen))

(defmethod source ((ugen ugen))
  ugen)

(defmethod num-outputs ((ugen ugen))
  1)

(defmethod num-outputs ((ugen nooutput-ugen))
  0)

(defmethod output-index ((ugen ugen))
  0)

(defmethod collect-constants ((ugen ugen))
  (dolist (input (inputs ugen))
    (unless (typep input 'ugen)
      (add-constant (synthdef ugen) (floatfy input)))))

(defmethod collect-constants ((ugen t))
  (add-constant (synthdef ugen) (cond ((null ugen) 0.0)
                                      ((eq t ugen) 1.0))))

(defmethod optimize-graph ((ugen ugen)))

(defmethod perform-dead-code-elimination ((ugen ugen))
  (when (zerop (length (descendants ugen)))
    (dolist (in (inputs ugen))
      (when (typep in 'ugen)
	(alexandria:removef (descendants in) ugen)
	(optimize-graph in)))
    (alexandria:removef (children (synthdef ugen)) ugen)
    t))

(defmethod init-topo-sort ((ugen ugen))
  (dolist (input (inputs ugen))
    (when (typep input 'ugen)
      (pushnew (source input) (antecedents ugen))
      (pushnew ugen (descendants (source input)))))
  (dolist (first-antecs (width-first-antecedents ugen))
    (pushnew first-antecs (antecedents ugen))
    (pushnew ugen (descendants first-antecs))))

(defmethod make-available ((ugen ugen))
  (with-slots (antecedents synthdef) ugen
    (when (zerop (length antecedents))
      (push ugen (available synthdef)))))

(defun schedule (ugen stack)
  (dolist (desc (reverse (descendants ugen)))
    (alexandria:removef (antecedents desc) ugen)
    (make-available desc))
  (push ugen stack))


(defun make-pstring (string)
  (flex:with-output-to-sequence (vec)
    (write-byte (length string) vec)
    (write-sequence (map '(array (unsigned-byte 8) (*)) #'char-code string) vec)))

(defmethod input-spec ((ugen ugen))
  (labels ((input-spec (in)
	     (if (typep in 'ugen) (list (synth-index (source in))
				  (output-index in))
		 (list -1 (position (floatfy in) (constants (synthdef ugen)))))))
    (mapcar #'input-spec (inputs ugen))))

(defmethod write-def-ugen-version1 ((ugen ugen) stream)
  (write-sequence (make-pstring (name ugen)) stream)
  (write-byte (rate-number ugen) stream)
  (write-sequence (sc-osc::encode-int16 (length (inputs ugen))) stream)
  (write-sequence (sc-osc::encode-int16 (num-outputs ugen)) stream)
  (write-sequence (sc-osc::encode-int16 (special-index ugen)) stream)
  (dolist (in (input-spec ugen))
    (write-sequence (sc-osc::encode-int16 (first in)) stream)
    (write-sequence (sc-osc::encode-int16 (second in)) stream))
  (write-sequence (make-array (num-outputs ugen) :element-type '(unsigned-byte 8)
						 :initial-element (rate-number ugen))
		  stream))

(defmethod write-def-ugen-version2 ((ugen ugen) stream)
  (write-sequence (make-pstring (name ugen)) stream)
  (write-byte (rate-number ugen) stream)
  (write-sequence (osc::encode-int32 (length (inputs ugen))) stream)
  (write-sequence (osc::encode-int32 (num-outputs ugen)) stream)
  (write-sequence (sc-osc::encode-int16 (special-index ugen)) stream)
  (dolist (in (input-spec ugen))
    (write-sequence (osc::encode-int32 (first in)) stream)
    (write-sequence (osc::encode-int32 (second in)) stream))
  (write-sequence (make-array (num-outputs ugen) :element-type '(unsigned-byte 8)
						 :initial-element (rate-number ugen))
		  stream))



;;; check inputs ----------------------------------------------------------------------------------------

(defun check-n-inputs (ugen n)
  (when (eql (rate ugen) :audio)
    (dotimes (i n)
      (unless (eql (rate (nth i (inputs ugen))) :audio)
        (error "~a's input ~a is not audio rate (input: ~a rate: ~a)." ugen i (nth i (inputs ugen))
               (rate (nth i (inputs ugen))))))))

(defun check-when-audio (ugen)
  (when (and (eql (rate ugen) :audio) (not (eql (rate (nth 1 (inputs ugen))) :audio)))
    (error "~a's input is not audio rate (input: ~a rate: ~a)." ugen (nth 1 (inputs ugen)) (rate (nth 1 (inputs ugen))))))

(defun check-same-rate-as-first-input (ugen)
  (unless (eql (rate ugen) (rate (car (inputs ugen))))
    (error "~a's rate does not match ~a's rate." ugen (car (inputs ugen)))))


;;; UGEN CLASS   ----------------------------------------------------------------------------

;;; pure ugen    ----------------------------------------------------------------------------

(defclass pure-ugen (ugen) ())

(defmethod optimize-graph ((ugen pure-ugen))
  (perform-dead-code-elimination ugen))

;;; MultioutUgen ----------------------------------------------------------------------------

(defclass proxy-output (ugen)
  ((source :initarg :source :reader source)
   (rate :initarg :rate :accessor rate)
   (output-index :initarg :output-index :reader output-index)))

(defmethod print-object ((c proxy-output) stream)
  (format stream "#<~s :output-index ~s>" 'proxy-output (output-index c)))

(defclass multiout-ugen (ugen)
  ((channels :accessor channels)))

(defmethod new1 ((ugen multiout-ugen) &rest inputs)
  (setf (channels ugen) (first inputs))
  (setf (inputs ugen) (rest inputs))
  (add-to-synth ugen)
  (when (> (channels ugen) 0)
    (unbubble
     (loop repeat (channels ugen) for index from 0
	   collect (make-instance 'proxy-output :source ugen
						:rate (rate ugen)
						:output-index index)))))

(defmethod num-outputs ((ugen multiout-ugen))
  (channels ugen))

;;; WidthFirstUGen --------------------------------------------------------

(defclass width-first-ugen (ugen) ())

(defmethod new1 ((ugen width-first-ugen) &rest inputs)
  (setf (inputs ugen) inputs)
  (add-to-synth ugen)
  (alexandria:appendf (width-first-ugens (synthdef ugen)) (list ugen))
  ugen)


;;; -------------------------------------------------------------------------------------------
;;;  definition synth
;;; ------------------------------------------------------------------------------------------

(defmacro defugen (name args function &key (check-fn '(lambda (ugen) ugen))
					(signal-range :bipolar))
  (labels ((get-rate (rate)
	     (alexandria:if-let ((val (case rate
					(:ar :audio)
					(:kr :control)
					(:ir :scalar))))
	       val rate)))
    (alexandria:with-gensyms (cls inputs)
      `(progn
	 ,@(loop for func in function
		 collect
		 (let* ((ugen-name (intern
				    (if (eql (car func) :ar) (cat (string-upcase (car name)) "." (string-upcase (car func)))
					(cat (string-upcase (car name)) "." (string-upcase (car func)))))))
		   `(progn
		      (defun ,ugen-name ,args
			(let ((new (lambda (,cls &rest ,inputs)
				     (apply #'ugen-new ,(second name) ,(get-rate (car func)) ,cls
					    ,check-fn ,signal-range
					    ,inputs))))
			  (declare (ignorable new))
			  ,@(cdr func)))
		      (export ',ugen-name))))))))
