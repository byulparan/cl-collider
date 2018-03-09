(in-package #:sc)

(defvar *s* nil
  "Special symbol bound to the default scsynth server. If functions do not specify a target server, that message is sent to the *s* server.")

(defvar *sc-synth-program*
  #+darwin "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth"
  #+linux "/usr/bin/scsynth"
  "The path to the scsynth binary.")

(setf *sc-plugin-paths*
  #+darwin (list "/Applications/SuperCollider/SuperCollider.app/Contents/Resources/plugins/"
		 "~/Library/Application\ Support/SuperCollider/Extensions/")
  #+linux (list "/usr/lib/SuperCollider/plugins/"
		"/usr/share/SuperCollider/Extensions/SC3plugins/"))

(defvar *sc-synthdefs-path*
  #+darwin (full-pathname "~/Library/Application Support/SuperCollider/synthdefs")
  #+linux (full-pathname "~/.local/share/SuperCollider/synthdefs/")
  "The directory where the scsyndef files for synthdefs are saved.")

;;; -------------------------------------------------------
;;; Server - base class
;;; -------------------------------------------------------

(defclass server ()
  ((name :initarg :name :initform "" :reader name)
   (server-lock :initform (bt:make-lock) :reader server-lock)
   (id :initform (list #-sbcl 999 #+sbcl 1000)
       :reader id)
   (buffers :initarg :buffers :accessor buffers)
   (audio-buses :initarg :audio-buses :accessor audio-buses)
   (control-buses :initarg :control-buses :accessor control-buses))
  (:documentation "This is base class for the scsynth server. This library includes realtime server, NRT server, and internal server (not yet implemented)."))



(defun get-next-id (server)
  #+ccl (ccl::atomic-incf (car (id server)))
  #+sbcl (sb-ext:atomic-incf (car (id server)))
  #+ecl (bt:with-lock-held ((server-lock server))
	  (incf (car (id server)))))


;;; declare generic function for realtime server
(defgeneric floatfy (object)
  (:documentation "All data that is sent to the server must be in Float32 format. This function converts lisp objects to Float32."))

(defmethod floatfy ((object t))
  (cond
    ((null object) 0.0)
    ((eq t object) 1.0)
    (t object)))

(defmethod floatfy ((number number))
  (float number))

(defmethod floatfy ((number double-float))
  (coerce number 'single-float))

(defgeneric is-local-p (server)
  (:documentation "The scsynth server can run across the network.
 If the server is running on the local machine, return T, otherwise NIL."))

(defgeneric sr (buffer))
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

;;; -------------------------------------------------------
;;; realtime Server
;;; -------------------------------------------------------
(defgeneric install-reply-responder (rt-server cmd handler))
(defgeneric uninstall-reply-responder (rt-server cmd))
(defgeneric bootup-server-process (rt-server))
(defgeneric cleanup-server (rt-server))
(defgeneric send-message (server &rest msg))
(defgeneric send-bundle (server time list-of-messages))

(defgeneric node-watcher (rt-server))
(defgeneric (setf node-watcher) (value rt-server))

(defvar *cleanup-functions* nil)
(defvar *all-rt-servers* nil)

(defclass rt-server (server)
  ((server-options
    :initarg :server-options
    :reader server-options)
   (server-time-stamp
    :initarg :server-time-stamp
    :initform #'scheduler:unix-time
    :accessor server-time-stamp)
   (scheduler
    :accessor scheduler)
   (sc-thread
    :initform nil
    :accessor sc-thread)
   (boot-p
    :initform nil
    :accessor boot-p)
   (buffer-get-handlers
    :initarg :buffer-get-handlers
    :initform (make-hash-table :test #'equal)
    :reader buffer-get-handlers)
   (control-get-handlers
    :initform (make-hash-table)
    :reader control-get-handlers)
   (end-node-handler
    :initform (make-hash-table)
    :reader end-node-handler)
   (sync-id-map
    :initform (make-id-map)
    :reader sync-id-map
    :allocation :class)
   (node-watcher
    :initform nil
    :accessor node-watcher)
   (node-proxy-table
    :accessor node-proxy-table)))

(defmethod initialize-instance :after ((self rt-server) &key)
  (push self *all-rt-servers*)
  (setf (scheduler self) (make-instance 'scheduler:scheduler
			   :name (name self)
			   :timestamp (server-time-stamp self))))

(let ((semaphore-table (make-hash-table)))
  (defun get-semaphore-by-thread ()
    "Return one semaphore per thread."
    (let* ((semaphore (gethash (bt:current-thread) semaphore-table)))
      (unless semaphore
	(let ((new-semaphore #+ccl (ccl:make-semaphore)
			     #+sbcl (sb-thread:make-semaphore)
			     #+ecl (mp:make-semaphore)))
	  (setf (gethash (bt:current-thread) semaphore-table) new-semaphore
		semaphore new-semaphore)))
      semaphore)))

(defun sync (&optional (rt-server *s*))
  (when (typep rt-server 'rt-server)
    (let* ((semaphore (get-semaphore-by-thread))
	   (id (assign-id-map-id (sync-id-map rt-server) semaphore)))
      (send-message rt-server "/sync" id)
      #+ccl (ccl:wait-on-semaphore semaphore)
      #+sbcl (sb-thread:wait-on-semaphore semaphore)
      #+ecl (mp:wait-on-semaphore semaphore))))

(defmethod server-boot ((rt-server rt-server))
  (when (boot-p rt-server) (error "SuperCollider server already running."))
  (bootup-server-process rt-server)
  (initialize-server-responder rt-server)
  (labels ((bootup ()
	     (let* ((try-count 0))
	       (send-message rt-server "/sync" -1)
	       (loop while (not (thread-wait-with-timeout (lambda () (boot-p rt-server)) 30))
		     do (incf try-count)
			(when (> try-count 30) (return))
			(send-message rt-server "/sync" -1))
	       (when (and (not (boot-p rt-server)) (bt:thread-alive-p (sc-thread rt-server)))
		 (bootup)))))
    (bootup))
  (unless (boot-p rt-server)
    (cleanup-server rt-server)
    (error "Server failed to boot."))
  (when (boot-p rt-server)
    (send-message rt-server "/notify" 1)
    (scheduler:sched-run (scheduler rt-server))
    (group-free-all rt-server)
    (let ((options (server-options rt-server)))
      (setf (node-proxy-table rt-server) (make-hash-table)
            (buffers rt-server) (make-array (server-options-num-sample-buffers options) :initial-element nil)
            (audio-buses rt-server) (make-array (server-options-num-audio-bus options) :initial-element nil)
            (control-buses rt-server) (make-array (server-options-num-control-bus options) :initial-element nil))
      (loop :for i :upto (server-options-num-output-bus options)
         :do (get-next-bus rt-server :audio 1 i))
      (loop :for i :upto (server-options-num-input-bus options)
         :do (get-next-bus rt-server :audio 1 (+ i (server-options-num-output-bus options))))))
  rt-server)

(defmethod server-quit ((rt-server rt-server))
  (unless (boot-p rt-server) (error "SuperCollider server is not running."))
  (dolist (f *cleanup-functions*)
    (funcall f))
  (send-message rt-server "/quit")
  (thread-wait (lambda () (not (boot-p rt-server))))
  (scheduler:sched-stop (scheduler rt-server))
  (cleanup-server rt-server))

(defun add-reply-responder (cmd handler)
  (install-reply-responder *s* cmd handler))

(defun remove-reply-responder (cmd)
  (uninstall-reply-responder *s* cmd))

(defun initialize-server-responder (rt-server)
  (let ((*s* rt-server))
    (add-reply-responder
     "/done"
     (lambda (path &rest options)
       (cond ((string= path "/notify")
	      (destructuring-bind (notify &optional client-id)
		  options
		(declare (ignore notify client-id))))
	     ((string= path "/quit") (setf (boot-p rt-server) nil))
	     ((string= path "/b_write")
	      (let ((bufnum (car options)))
		(alexandria:when-let
		    ((f (gethash (list path bufnum) (buffer-get-handlers rt-server))))
		  (funcall f (elt (buffers rt-server) bufnum))))))))
    (add-reply-responder
     "/status.reply"
     (lambda (&rest args)
       (apply #'format t "~&UGens    : ~4d~&Synths   : ~4d~&Groups   : ~4d~&SynthDefs: ~4d~&% CPU (Average): ~a~&% CPU (Peak)   : ~a~&SampleRate (Nominal): ~a~&SampleRate (Actual) : ~a~%" (cdr args))))
    (add-reply-responder
     "/synced"
     (lambda (id)
       (case id
	 (-1  (setf (boot-p rt-server) t))
	 (otherwise (let ((semaphore (id-map-free-object (sync-id-map rt-server) id)))
		      #+ccl (ccl:signal-semaphore semaphore)
		      #+sbcl (sb-thread:signal-semaphore semaphore)
		      #+ecl (mp:signal-semaphore semaphore))))))
    (add-reply-responder
     "/c_set"
     (lambda (bus value)
       (funcall (gethash bus (control-get-handlers rt-server)) value)))
    (add-reply-responder
     "/b_set"
     (lambda (bufnum index value)
       (let ((action (gethash (list "/b_set" bufnum index) (buffer-get-handlers rt-server))))
	 (funcall action value))))
    (add-reply-responder
     "/b_setn"
     (lambda (bufnum start frames &rest values)
       (let ((action (gethash (list "/b_setn" bufnum start frames)
			      (buffer-get-handlers rt-server))))
	 (funcall action values))))
    (add-reply-responder
     "/b_info"
     (lambda (bufnum frames chanls sr)
       (let ((buffer (elt (buffers rt-server) bufnum)))
	 (setf (frames buffer) frames (chanls buffer) chanls (sr buffer) sr))))
    (add-reply-responder
     "/fail"
     (lambda (&rest args) (format t "FAILURE in server: ~{~a ~}~%" args)))
    (add-reply-responder
     "/n_go"
     (lambda (id &rest args)
       (declare (ignore args))
       (push id (node-watcher rt-server))))
    (add-reply-responder
     "/n_end"
     (lambda (id &rest args)
       (declare (ignore args))
       (alexandria:when-let ((handle (gethash id (end-node-handler rt-server))))
	 (funcall handle))
       (alexandria:removef (node-watcher rt-server) id)))
    (add-reply-responder
     "/d_removed" (lambda (&rest args) (declare (ignore args))))))



(defun control-get (index &optional action)
  (let ((result nil))
    (setf (gethash index (control-get-handlers *s*)) (if action action (lambda (value) (setf result value))))
    (message-distribute nil (list "/c_get" index) *s*)
    (unless action
      (sync)
      result)))

(defun control-set (index value)
  (message-distribute nil (list "/c_set" index value) *s*))


;;; scheduler
(defun callback (time f &rest args)
  (apply #'scheduler:sched-add (scheduler *s*) time f args))

(defun now ()
  (scheduler:sched-time (scheduler *s*)))

(defun quant (next-time &optional (offset .5))
  (scheduler:sched-quant (scheduler *s*) next-time offset))

;;; --------------------------------------------------------------------------------------------
;;; external server
;;; --------------------------------------------------------------------------------------------

(defclass external-server (rt-server)
  ((host 
    :initarg :host
    :reader host)
   (port
    :initarg :port
    :reader port)
   (osc-device
    :reader osc-device)
   (just-connect-p
    :initarg :just-connect-p
    :reader just-connect-p)))

(defmethod print-object ((self external-server) stream)
  (format stream "#<~s ~a-~d:~d>"
          'external-server (name self) (host self) (port self)))

(defun all-running-servers ()
  (remove-if-not #'(lambda (server) (boot-p server)) *all-rt-servers*))

(defmethod is-local-p ((rt-server external-server))
  (string= (host rt-server) "127.0.0.1"))

(defmethod bootup-server-process ((rt-server external-server))
  (with-slots (osc-device) rt-server
    (setf osc-device (sc-osc:osc-device (host rt-server) (port rt-server) :local-port 0)))
  (unless (just-connect-p rt-server)
    (setf (sc-thread rt-server)
      (bt:make-thread
       (lambda () (run-program
	       (format nil "~a -u ~a ~a"
		       (full-pathname *sc-synth-program*)
		       (port rt-server)
		       (build-server-options (server-options rt-server)))
	       :wait t
	       :output t))
       :name "scsynth"))))

(defmethod cleanup-server ((rt-server external-server))
  (sc-osc::join-thread (sc-thread rt-server))
  (sc-osc:close-device (osc-device rt-server)))

(defmethod server-quit ((rt-server external-server))
  (if (just-connect-p rt-server) (progn
				   (assert (boot-p rt-server) nil "SuperCollider server is not running.")
				   (setf (boot-p rt-server) nil)
				   (sc-osc:close-device (osc-device rt-server))
				   (scheduler:sched-stop (scheduler rt-server)))
      (call-next-method)))


(defmethod install-reply-responder ((rt-server external-server) cmd-name f)
  (sc-osc:add-osc-responder (osc-device rt-server) cmd-name f))

(defmethod uninstall-reply-responder ((rt-server external-server) cmd-name)
  (sc-osc:remove-osc-responder (osc-device rt-server) cmd-name))

(defmethod send-message ((server external-server) &rest msg)
  (apply #'sc-osc:send-message (osc-device server) msg))

(defmethod send-bundle ((server external-server) time list-of-messages)
  (apply #'sc-osc:send-bundle
	 time
	 (osc-device server)
	 list-of-messages))

(defun make-external-server (name &key (server-options (make-server-options))
				    (host "127.0.0.1")
				    port
				    just-connect-p)
  (assert port nil "Server port must be specified.")
  (make-instance 'external-server :name name
				  :server-options server-options
				  :host host
				  :port port
                  :buffers (make-array (server-options-num-sample-buffers server-options) :initial-element nil)
				  :just-connect-p just-connect-p))


;;cleanup
(labels ((clean-up-server ()
	   (dolist (server (all-running-servers))
	     (server-quit server))))
  #+ccl (push #'clean-up-server ccl::*lisp-cleanup-functions*)
  #+sbcl (push #'clean-up-server sb-ext:*exit-hooks*)
  #+ecl (push #'clean-up-server si:*exit-hooks*))



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
    `(let* ((,file-name (full-pathname ,output-files))
	    (,osc-file (cat (subseq ,file-name 0 (position #\. ,file-name)) ".osc"))
	    (scheduler::*scheduling-mode* :step)
	    (*s* (make-instance 'nrt-server :name "NRTSynth" :streams nil)))
       (make-group 1 :pos :head :to 0)
       ,@body
       (when ,pad (send-bundle *s* (* 1.0d0 ,pad) (list "/c_set" 0 0)))
       (with-open-file (,non-realtime-stream ,osc-file :direction :output :if-exists :supersede
						       :element-type '(unsigned-byte 8))
	 (dolist (,message (sort (streams *s*)  #'<= :key #'car))
	   (when (and ,pad (> (car ,message) ,pad)) (return))
	   (let ((,message (sc-osc::encode-bundle (second ,message) (car ,message))))
	     (write-sequence (osc::encode-int32 (length ,message)) ,non-realtime-stream)
	     (write-sequence ,message ,non-realtime-stream))))
       (run-program
	(format nil "~a -U \"~{~a~^:~}\" -N ~a _ ~a ~a ~a ~a -o 2"
		(full-pathname *sc-synth-program*)
		(mapcar #'full-pathname *sc-plugin-paths*)
		,osc-file ,file-name ,sr
		(string-upcase (pathname-type ,file-name))
		(ecase ,format
		  (:int16 "int16")
		  (:int24 "int24")
		  (:float "float")
		  (:double "double")))
	:wait t
	:output t)
       (unless ,keep-osc-file
	 (delete-file ,osc-file))
       (values))))




;;; -------------------------------------------------------
;;; Node
;;; -------------------------------------------------------
(defclass node ()
  ((server :initarg :server :reader server)
   (id :initarg :id :reader id)
   (pos :initarg :pos :reader pos)
   (to :initarg :to :reader to)
   (name :initarg :name :reader name)
   (meta :initarg :meta :initform nil :reader meta)))

(defmethod print-object ((node node) stream)
  (format stream "#<~s :server ~s :id ~s :name ~s>" 'node (server node) (id node) (name node)))

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
  `(let ((,id (etypecase ,node
		(number ,node)
		(node (id ,node))
		(keyword (alexandria:when-let ((node (gethash ,node (node-proxy-table *s*))))
			   (id node)))))
	 (,server (if (typep ,node 'node) (server ,node) *s*)))
     (when ,id
       ,@body)))

(defun make-synth-msg (rt-server name id to pos &rest args)
  (with-node (to target server)
    (assert (eql rt-server server) nil "Target server is not synth's server. (/= ~a ~a)" server rt-server)
    (apply #'list 9 name id (node-to-pos pos) target (mapcar #'floatfy args))))

(defun ctrl (node &rest param &key &allow-other-keys)
  (with-node (node id server)          ;; /n_set == 15
    (message-distribute node (cons 15 (cons id (mapcar #'(lambda (p) (cond ((symbolp p) (string-downcase p)) ;key
								       (t (floatfy p)))) ;value
						       param))) server)))
(defun map-bus (node &rest param &key &allow-other-keys)
  "Map a bus or buses onto the specified controls of a node."
  (with-node (node id server)
    (message-distribute node (append (list "/n_map" id) (mapcar (lambda (p) (cond ((symbolp p) (string-downcase p))
                                                                                  (t (floatfy p))))
                                                                param))
                        server)))

(defun bye (node)
  "Deprecated function; use `free' instead."
  (warn "#'bye is deprecated; please use #'free instead.")
  (free node))

(defun free (node)
  "Free a node running on the server."
  (with-node (node id server)
    (message-distribute node (list "/n_free" id) server)))

(defun release (node)
  "Set the gate control of NODE to 0, releasing the node."
  (ctrl node :gate 0))

(defun is-playing-p (node)
  (with-node (node id server)
    (when (find id (node-watcher server))
      t)))

;;; Group
(defclass group (node)
  ())

(defmethod print-object ((node group) stream)
  (format stream "#<~s :server ~s :id ~s>" 'group (server node) (id node)))

(let ((new-group-id 1))
  (defun make-group (&key id (server *s*) (pos :after) (to 1))
    (with-node (to target-id rt-server)
      (unless (numberp to)
	(assert (eql rt-server server) nil "Target server is not group's server. (/= ~a ~a)" rt-server server))
      (let* ((group-id (if id id (incf new-group-id)))
	     (group (make-instance 'group :server server :id group-id :pos pos :to target-id)))
	(message-distribute group (list "/g_new" group-id (node-to-pos pos) target-id) server)
	(sync)
	group))))

(defun server-query-all-nodes (&optional (rt-server *s*))
  (send-message rt-server "/g_dumpTree" 0 0))

(defvar *group-free-all-hook* nil)

(defun group-free-all (&optional (rt-server *s*))
  (let ((*s* rt-server))
    (scheduler:sched-clear (scheduler rt-server))
    (send-message rt-server "/g_freeAll" 0)
    (send-message rt-server "/clearSched")
    (make-group :id 1 :pos :head :to 0)
    (dolist (hook *group-free-all-hook*)
      (funcall hook))))

(defvar *stop-hook* nil)

(defun stop (&optional (group 1) &rest groups)
  (scheduler:sched-clear (scheduler *s*))
  (dolist (group (cons group groups))
    (send-message *s* "/g_freeAll" group)
    (send-message *s* "/clearSched"))
  (dolist (hook *stop-hook*)
    (funcall hook)))

(defun server-status (&optional (rt-server *s*))
  (send-message rt-server "/status"))
