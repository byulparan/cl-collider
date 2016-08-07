
(in-package #:sc)

(defvar *s* nil
  "It's special symbol bind to default scsynth server. If functions that are not specified target server,
 that message send to *s*")

(defvar *sc-synthdefs-path* ""
  "This path is where the scsyndef file is saved.")

(defvar *sc-plugin-paths* nil
  "list of scsynth's plugins(scx) path.")

(defvar *sc-synth-program* "scsynth"
  "scsynth program's path. If wrong path is given, scsynth is can't run.")

;;; -------------------------------------------------------
;;; Server - base class
;;; -------------------------------------------------------

#+sbcl
(defstruct counter (count 0 :type sb-vm::word))

(defclass server ()
  ((name :initarg :name :initform "" :reader name)
   (buffers :initarg :buffers :initform (make-hash-table) :reader buffers)
   (id-and-buffer-number :initarg :id-and-buffer-number
			 :initform #+ccl (cons 999 -1) #+sbcl (cons (make-counter :count 1000)
								    (make-counter :count 0))
			 :accessor id-and-buffer-number)
  (buffer-number :initarg :buffer-number
		 :initform (make-array 1024 :element-type 'boolean :initial-element nil)
		 :accessor buffer-number))
  (:documentation "This is base server class of scsynth server. This library include realtime server,NRT server,
 internal server(yet..)"))

(defmethod get-next-id ((server server))
  #+ccl(ccl::%atomic-incf-car (id-and-buffer-number server))
  #+sbcl (sb-ext:atomic-incf (counter-count (car (id-and-buffer-number server)))))

(defmethod get-next-buffer-number ((server server))
  (let ((num (position nil (buffer-number server))))
	  (setf (elt (buffer-number server) num) t)
	  num))

(defmethod free-buffer-number ((server server) (bufnum integer))
  (setf (elt (buffer-number server) bufnum) nil))


;;; declare generic function for realtime server
(defgeneric floatfy (object)
  (:documentation "All data that is sent to sc-synth server must be Float32. This function is
 make Float32 for lisp object"))

(defmethod floatfy ((object t))
  object)

(defmethod floatfy ((number number))
  (float number))

(defmethod floatfy ((number double-float))
  (coerce number 'single-float))

(defgeneric is-local-p (server)
  (:documentation "scsynth server can run everywhere in Network.
 If server in local-machine return T, otherwise NIL"))

(defgeneric send-message (server &rest msg))
(defgeneric send-bundle (server time list-of-messages))

(defgeneric initialize-server-responder (rt-server))
(defgeneric bootup-server-process (rt-server))
(defgeneric node-watcher (rt-server))
(defgeneric (setf node-watcher) (value rt-server))
 
(defgeneric sr (server/buffer))
(defgeneric (setf sr) (sr buffer))
(defgeneric chanls (buffer))
(defgeneric (setf chanls) (chanls buffer))
(defgeneric frames (buffer))
(defgeneric (setf frames) (frames buffer))
(defgeneric channels (buffer))



(defvar *bundle-msg-container*)
(defvar *bundle-synth-container*)
(defparameter *run-level* :execute)

(defun message-distribute (synth msg server)
  (if (eql *run-level* :bundle) (progn
				  (assert (eql *s* server))
				  (pushnew msg *bundle-msg-container*)
				  (pushnew synth *bundle-synth-container*))
      (apply #'send-message server msg))
  synth)

;;; Node
(defclass node ()
  ((server :initarg :server :reader server)
   (id :initarg :id :reader id)
   (pos :initarg :pos :reader pos)
   (to :initarg :to :reader to)
   (name :initarg :name :reader name)
   (meta :initarg :meta :initform nil :reader meta)))

(defmethod print-object ((node node) stream)
  (format stream "#<Node :server ~s :id ~a :name ~s>" (server node) (id node) (name node)))

(defmacro at (time &body body)
  `(let ((*run-level* :bundle)
	 (*bundle-msg-container* nil)
	 (*bundle-synth-container* nil))
     ,@body
     (send-bundle *s* ,time *bundle-msg-container*)
     (unbubble (reverse *bundle-synth-container*))))

(defun node-to-pos (pos)
  (case pos
    (:head 0)
    (:tail 1)
    (:before 2)
    (:after 3)
    (t 0)))

(defmacro with-node ((node id server) &body body)
  `(let ((,id (if (numberp ,node) ,node (id ,node)))
	 (,server (if (numberp ,node) *s* (server ,node))))
     ,@body))

(defun make-synth-msg (rt-server name id to pos &rest args)
  (with-node (to target server)
    (assert (eql rt-server server) nil "target server != synth's server.(/= ~a ~a)" server rt-server)
    (apply #'list 9 name id (node-to-pos pos) target (mapcar #'floatfy args))))

(defun ctrl (node &rest param &key &allow-other-keys)
  (with-node (node id server)          ;; /n_set == 15
    (message-distribute node (cons 15 (cons id (mapcar #!(cond ((symbolp %) (string-downcase %)) ;key
							       (t (floatfy %))) ;value
						       param))) server)))

(defun bye (node)
  (with-node (node id server)
    (message-distribute node (list 11 id) server))) ;; /n_free == 11

(defun is-playing-p (node)
  (with-node (node id server)
    (when (find id (node-watcher server))
      t)))

;;; Group
(defclass group (node)
  ())

(defmethod print-object ((node group) stream)
  (format stream "#<Group :server ~s :id ~a>" (server node) (id node)))

(defun make-group (group-id &key (server *s*) (pos :after) (to 1))
  (with-node (to target-id rt-server)
    (unless (numberp to)
      (assert (eql rt-server server) nil "target's server != group's server.(/= ~a ~a)" rt-server server))
    (let ((group (make-instance 'group :server server :id group-id :pos pos :to target-id)))
      (message-distribute group (list "/g_new" group-id (node-to-pos pos) target-id) server))))

(defun server-query-all-nodes (&optional (rt-server *s*))
  (send-message rt-server "/g_dumpTree" 0 0))

(defvar *group-free-all-hook* nil)
(defun set-hook-group-free-all (f)
  (setf *group-free-all-hook* f))

(defun group-free-all (&optional (rt-server *s*))
  (let ((*s* rt-server))
    (cb:scheduler-clear)
    (send-message rt-server "/g_freeAll" 0)		
    (send-message rt-server "/clearSched")
    (make-group 1 :pos :head :to 0)
    (alexandria:when-let ((hook *group-free-all-hook*))
      (funcall hook))))

(defun stop (&optional (group 1) &rest groups)
  (cb:scheduler-clear)
  (dolist (group (cons group groups))
    (send-message *s* "/g_freeAll" group)
    (send-message *s* "/clearSched")))

(defun server-status (&optional (rt-server *s*))
  (send-message rt-server "/status"))



;;; -------------------------------------------------------
;;; realtime Server
;;; -------------------------------------------------------

(defclass rt-server (server)
  ((sr :initarg :sr :initform 44100 :reader sr)
   (boot-p :initform nil :accessor boot-p)
   (buffer-get-handlers :initarg :buffer-get-handlers
			:initform (make-hash-table :test #'equal)
			:reader buffer-get-handlers)
   (control-get-handlers :initform (make-hash-table) :reader control-get-handlers)
   (end-node-handler :initform (make-hash-table) :reader end-node-handler)
   (sync-id-map :initform (make-id-map) :reader sync-id-map :allocation :class)
   (node-watcher :initform nil :accessor node-watcher)))


(let ((semaphore-table (make-hash-table)))
  (defun get-semaphore-by-thread ()
    "Returns a one semaphore per thread."
    (let* ((semaphore (gethash (bt:current-thread) semaphore-table)))
      (unless semaphore
	(let ((new-semaphore #+ccl (ccl:make-semaphore)
			     #+sbcl (sb-thread:make-semaphore)))
	  (setf (gethash (bt:current-thread) semaphore-table) new-semaphore
		semaphore new-semaphore)))
      semaphore)))

(defun sync (&optional (rt-server *s*))
  (let* ((semaphore (get-semaphore-by-thread))
	 (id (assign-id-map-id (sync-id-map rt-server) semaphore)))
    (send-message rt-server "/sync" id)
    #+ccl (ccl:wait-on-semaphore semaphore)
    #+sbcl (sb-thread:wait-on-semaphore semaphore)))

(defmethod server-boot ((rt-server rt-server))
  (when (boot-p rt-server) (error "already supercollider server running"))
  (cb:scheduler-start)
  (bootup-server-process rt-server)
  (initialize-server-responder rt-server)
  (let* ((try-count 0) (success? t))
    (send-message rt-server "/sync" -1)
    (loop while (not (thread-wait-with-timeout (lambda () (boot-p rt-server)) 30))
	  do (incf try-count)
	     (when (> try-count 30) (setf success? nil) (return))
	     (send-message *s* "/sync" -1))
    (unless success?
      (close-osc-device (osc-device rt-server))
      (error "Failed Server Boot"))
    (when success?
      (send-message rt-server "/notify" 1)
      (group-free-all rt-server))))

(defmethod server-quit ((rt-server rt-server))
  (unless (boot-p rt-server) (error "supercollider not running"))
  (cb:scheduler-clear)
  (send-message rt-server "/quit")
  (thread-wait (lambda () (not (boot-p rt-server)))))

(defmethod initialize-server-responder ((rt-server rt-server))
  (let ((*s* rt-server))
    (add-reply-responder "/done"
			 (lambda (args)
			   (cond ((string= (car args) "/quit") (setf (boot-p rt-server) nil))
				 ((string= (car args) "/b_write") (alexandria:when-let ((f (gethash args (buffer-get-handlers rt-server))))
								    (funcall f (gethash (second args) (buffers rt-server))))))))
    (add-reply-responder "/status.reply"
			 (lambda (args)
			   (apply #'format t "~&UGens    : ~4d~&Synths   : ~4d~&Groups   : ~4d~&SynthDefs: ~4d~&% CPU (Averate): ~a~&% CPU (Peak)   : ~a~&SampleRate (Nominal): ~a~&SampleRate (Actual) : ~a~%" (cdr args))))
    (add-reply-responder "/synced"
			 (lambda (args)
			   (let ((id (car args)))
			     (case id
			       (-1  (setf (boot-p rt-server) t))
			       (otherwise (let ((semaphore (id-map-free-object (sync-id-map rt-server) id)))
					    #+ccl (ccl:signal-semaphore semaphore)
					    #+sbcl (sb-thread:signal-semaphore semaphore)))))))
    (add-reply-responder "/c_set"
			 (lambda (args)
			   (funcall (gethash (first args) (control-get-handlers rt-server)) (second args))))
    (add-reply-responder "/b_set"
			 (lambda (args)
			   (destructuring-bind (bufnum index value) args
			     (let ((action (gethash (list "/b_set" bufnum index) (buffer-get-handlers rt-server))))
			       (funcall action value)))))
    (add-reply-responder "/b_setn"
			 (lambda (args)
			   (destructuring-bind (bufnum start frames &rest values) args
			     (let ((action (gethash (list "/b_setn" bufnum start frames) (buffer-get-handlers rt-server))))
			       (funcall action values)))))
    (add-reply-responder "/b_info"
			 (lambda (args)
			   (destructuring-bind (bufnum frames chanls sr) args
			     (let ((buffer (gethash bufnum (buffers rt-server))))
			       (setf (frames buffer) frames (chanls buffer) chanls (sr buffer) sr)))))
    (add-reply-responder "/fail" (lambda (args) (format t "FAIL! ~{~a ~}~%" args)))
    (add-reply-responder "/n_go" (lambda (args) (push (car args) (node-watcher rt-server))))
    (add-reply-responder "/n_end" (lambda (args)
					      (alexandria:when-let ((handle (gethash (car args) (end-node-handler rt-server))))
						(funcall handle))
					      (alexandria:removef (node-watcher rt-server) (car args))))
    (add-reply-responder "/d_removed" (lambda (args) (declare (ignore args))))))



(defun control-get (index &optional action)
  (let ((result nil))
    (setf (gethash index (control-get-handlers *s*)) (if action action #!(setf result %)))
    (message-distribute nil (list "/c_get" index) *s*)
    (unless action
      (sync)
      result)))

(defun control-set (index value)
  (message-distribute nil (list "/c_set" index value) *s*))

;;; --------------------------------------------------------------------------------------------
;;; external server
;;; --------------------------------------------------------------------------------------------
(defclass external-server (rt-server)
  ((host :initarg :host :accessor host)
   (port :initarg :port :accessor port :initform (error "server's port not specified."))
   (client-port :initarg :client-port :reader client-port)
   (osc-device :accessor osc-device)
   (just-connect-p :initarg :just-connect-p :reader just-connect-p)))

(defmethod print-object ((self external-server) stream)
  (format stream "#<SC-SYNTH ~a-~d:~d>" (name self) (host self) (port self)))

(setf *external-servers* nil)

(defun all-running-servers ()
  (remove-if-not #!(boot-p %) *external-servers*))

(defmethod initialize-instance :after ((self external-server) &key)
  (alexandria:when-let ((server (find (port self) *external-servers* :key #'port)))
    (error "~a's port ~a already bind to ~a" self (port self)  server))
  (push self *external-servers*))

(defmethod addr ((server external-server))
  (cons (host server) (port server)))

(defmethod lisp-port ((server external-server))
  (usocket:get-local-port (socket (osc-device server))))

(defmethod is-local-p ((server external-server))
  (string= (host server) "127.0.0.1"))

(defmethod reply-log-p ((server external-server))
  (print-log-p (osc-device server)))

(defmethod (setf reply-log-p) (v (server external-server))
  (setf (print-log-p (osc-device server)) v))

(defmethod bootup-server-process ((rt-server external-server))
  (setf (osc-device rt-server) (make-osc-device (format nil "scsynth:~d-~d" (host rt-server) (port rt-server))))
  (unless (just-connect-p rt-server)
    (let ((sock nil))			;verify port-number.
      (handler-case (setf sock (usocket:socket-connect nil nil :protocol :datagram :local-port (port rt-server)))
	(error nil
	  (unwind-protect (error "~a's port ~d is already used by another process. maybe..that process is \"scsynth\"
which previously create by you. check processes in system." rt-server (port rt-server))
	    (close-osc-device (osc-device rt-server)))))
      (usocket:socket-close sock))
    (bt:make-thread
     (lambda () (su:run-program
		 (format nil "~a -u ~a -U \"~{~a~^:~}\""
			 (su:full-pathname *sc-synth-program*)
			 (port rt-server)
			 (mapcar #'su:full-pathname *sc-plugin-paths*)) 
		 :output t :wait t)
       (when (boot-p rt-server)
	 (unwind-protect (error "~a was abnormal termination!" rt-server)
	   (cb:scheduler-clear)
	   (setf (boot-p rt-server) nil)
	   (close-osc-device (osc-device rt-server)))))
     :name "scsynth")))

(defmethod server-quit ((rt-server external-server))
  (if (just-connect-p rt-server) (setf (boot-p rt-server) nil)
      (call-next-method))
  (close-osc-device (osc-device rt-server)))

(defun add-reply-responder (cmd-name f &optional (rt-server *s*))
  (insert-reply-handler (osc-device rt-server) cmd-name f))

(defun remove-reply-responder (cmd-name &optional (rt-server *s*))
  (remove-reply-handler (osc-device rt-server) cmd-name))

(defun make-external-server (name &key (host "127.0.0.1") port just-connect-p)
  (make-instance 'external-server :name name
				  :host host
				  :port port
				  :just-connect-p just-connect-p))

;;; -------------------------------------------------------
;;; message
;;; -------------------------------------------------------
(defmethod send-message ((server external-server) &rest msg)
  (apply #'send-message-to-osc-device (osc-device server) (host server) (port server) msg)
  (values))

(defmethod send-bundle ((server external-server) time list-of-messages)
  (apply #'send-bundle-to-osc-device
	 (osc-device server)
	 (host server)
	 (port server)
	 time
	 list-of-messages)
  (values))


;;; cleanup
(labels ((clean-up-server ()
	   (dolist (server (all-running-servers))
	     (server-quit server))))
  #+ccl (push #'clean-up-server ccl::*lisp-cleanup-functions*)
  #+sbcl (push #'clean-up-server sb-ext:*exit-hooks*))



;;; -------------------------------------------------------
;;; Non Realtime Server
;;; -------------------------------------------------------
(defclass nrt-server (server)
  ((streams :initarg :streams :initform nil :accessor streams)))

(defmethod send-message ((server nrt-server) &rest msg)
  (send-bundle server 0.0d0 msg))

(defmethod send-bundle ((server nrt-server) time list-of-messages)
  (declare (type double-float time))
  (push (list (- time osc::+unix-epoch+) list-of-messages) (streams server)))


(defmacro with-rendering ((output-files &key (pad nil) (keep-osc-file nil) (format :int24) (sr 44100)) &body body)
  (alexandria:with-gensyms (osc-file file-name non-realtime-stream message)
    `(let* ((,file-name (su:full-pathname ,output-files))
	    (,osc-file (su:cat (subseq ,file-name 0 (position #\. ,file-name)) ".osc"))
	    (scheduler::*scheduling-mode* :step)
	    (*s* (make-instance 'nrt-server :name "NRTSynth" :streams nil)))
       (make-group 1 :pos :head :to 0)
       ,@body
       (when ,pad (send-bundle *s* (* 1.0d0 ,pad) (list "/c_set" 0 0)))
       (with-open-file (,non-realtime-stream ,osc-file :direction :output :if-exists :supersede
						       :element-type '(unsigned-byte 8))
	 (dolist (,message (sort (streams *s*)  #'<= :key #'car))
	   (when (and ,pad (> (car ,message) ,pad)) (return))
	   (let ((,message (osc::encode-bundle (second ,message) (car ,message))))
	     (write-sequence (osc::encode-int32 (length ,message)) ,non-realtime-stream)
	     (write-sequence ,message ,non-realtime-stream))))
       (su:run-program
	(format nil "~a -U \"~{~a~^:~}\" -N ~a _ ~a ~a ~a ~a -o 2"
		(su:full-pathname *sc-synth-program*)
		(mapcar #'su:full-pathname *sc-plugin-paths*)
		,osc-file ,file-name ,sr
		(string-upcase (pathname-type ,file-name))
		(ecase ,format
		  (:int16 "int16")
		  (:int24 "int24")
		  (:float "float")
		  (:double "double")))
	:output t :wait t)
       (unless ,keep-osc-file
	 (delete-file ,osc-file))
       (values))))

