(in-package #:sc)

(defvar *s* nil
  "Special symbol bound to the default scsynth server. If functions do not specify a target server, that message is sent to the *s* server.")

(defun sc-path-not-found-warning ()
  (progn (warn "SuperCollider was not found in the default path.") #p""))

#+windows
(defvar *win-sc-dir*
  (or (find-if (alexandria:compose (alexandria:curry #'search "SuperCollider")
				   #'namestring)
	       (uiop:subdirectories (uiop:getenv-pathname "ProgramFiles"))
	       :from-end t)
      (sc-path-not-found-warning)))

;; default path are which build target from source
(defvar *sc-synth-program*
  #+darwin (or (find-if #'uiop:file-exists-p '("/Applications/SuperCollider/SuperCollider.app/Contents/Resources/scsynth"
					       "/Applications/SuperCollider.app/Contents/Resources/scsynth"))
	       (sc-path-not-found-warning))
  #+(or linux freebsd) (handler-case
                           (uiop:run-program '("which" "scsynth") :output :line)
                         (t (c)
                           (warn "SuperCollider was not found in the system path.")
                           nil))
  #+windows (make-pathname :name "scsynth" :type "exe" :defaults *win-sc-dir*)
  "The path to the scsynth binary.")

(setf *sc-plugin-paths*
  #+darwin (list (or (find-if #'uiop:truename* '("/Applications/SuperCollider/SuperCollider.app/Contents/Resources/plugins/"
						 "/Applications/SuperCollider.app/Contents/Resources/plugins/")))
		 "~/Library/Application\ Support/SuperCollider/Extensions/")
  #+(or linux freebsd) (remove-if-not #'uiop:directory-exists-p '("/usr/local/lib/SuperCollider/plugins/"
                                                                  "/usr/lib/SuperCollider/plugins/"
                                                                  "/usr/local/share/SuperCollider/Extensions/"
                                                                  "/usr/share/SuperCollider/Extensions/"))
  #+windows (list (merge-pathnames #P"plugins/" *win-sc-dir*)
		  (full-pathname (merge-pathnames #P"SuperCollider/Extensions/"
						  (uiop:get-folder-path :local-appdata)))))

(defvar *sc-synthdefs-path*
  #+darwin (full-pathname "~/Library/Application Support/SuperCollider/synthdefs/")
  #+(or linux freebsd) (full-pathname "~/.local/share/SuperCollider/synthdefs/")
  #+windows (full-pathname (merge-pathnames #P"SuperCollider/synthdefs/"
					    (uiop:get-folder-path :local-appdata)))
  "The directory where the scsyndef files for synthdefs are saved.")

(defvar *sc-help-paths*
  (mapcar (lambda (dir)
	    (merge-pathnames (make-pathname :directory '(:relative :back "HelpSource" "Classes"))
			     dir))
	  *sc-plugin-paths*)
  "A list of directories where helpfiles for SuperCollider UGens will be searched.")

;;; -------------------------------------------------------
;;; Server - base class
;;; -------------------------------------------------------

(defclass server ()
  ((name :initarg :name :initform "" :reader name)
   (server-options :initarg :server-options :reader server-options)
   (server-lock :initform (bt:make-lock) :reader server-lock)
   (id :initform (list #-sbcl 999 #+sbcl 1000) :accessor id)
   (group-id :initform 1 :accessor group-id)
   (buffers :initarg :buffers :accessor buffers)
   (audio-buses :initarg :audio-buses :accessor audio-buses)
   (control-buses :initarg :control-buses :accessor control-buses)
   (tempo-clock :accessor tempo-clock)
   (node-proxy-table :accessor node-proxy-table))
  (:documentation "This is base class for the scsynth server. This library includes realtime server, NRT server, and internal server (not yet implemented)."))

(defun get-next-id (server)
  #+ccl (ccl::atomic-incf (car (id server)))
  #+sbcl (sb-ext:atomic-incf (car (id server)))
  #+ecl (bt:with-lock-held ((server-lock server))
	  (incf (car (id server))))
  #+lispworks (system:atomic-incf (car (id server))))

;;; declare generic function for realtime server
(defgeneric floatfy (object)
  (:documentation "Convert OBJECT to a 32-bit float that the server can understand."))

(defmethod floatfy ((object t))
  (error "Can't floatfy ~s" object))

(defmethod floatfy ((number number))
  (float number))

(defmethod floatfy ((number double-float))
  (coerce number 'single-float))

(defmethod floatfy ((object cons))
  (mapcar #'floatfy object))

(defmethod floatfy ((object string))
  object)

(defmethod floatfy ((object symbol))
  (env-shape-number object))

(defmethod floatfy ((object list))
  (mapcar #'floatfy object))

(defgeneric is-local-p (server)
  (:documentation "The scsynth server can run across the network.
 If the server is running on the local machine, return T, otherwise NIL."))

(defgeneric free (object)
  (:documentation "Free a node or buffer on the server."))

(defgeneric sr (object))
(defgeneric (setf sr) (sr object))

(defmethod sr ((server server))
  (sample-rate server))

(defmethod (setf sr) (sr (server server))
  (setf (sample-rate server) sr))

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

(defgeneric sc-reply-thread (rt-server))

(defgeneric initialize-scheduler (rt-server)
  (:documentation
   "Create scheduler and tempo-clock. Called when the rt-server is created."))


(defvar *server-boot-hooks* nil)
(defvar *server-quit-hooks* nil)
(defvar *all-rt-servers* nil)

(defclass rt-server (server)
  ((sample-rate
    :initform nil
    :accessor sample-rate)
   (sched-ahead
    :initarg :sched-ahead
    :initform 0.2d0
    :accessor sched-ahead)
   (latency
    :initarg :latency
    :initform 0.0d0
    :accessor latency)
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
   (sync-id-map
    :initform (make-id-map)
    :reader sync-id-map
    :allocation :class)
   (node-watcher
    :initform nil
    :accessor node-watcher)))

(defmethod initialize-instance :after ((self rt-server) &key)
  (push self *all-rt-servers*)
  (initialize-scheduler self))



(defmethod initialize-scheduler ((rt-server rt-server))
  (setf (scheduler rt-server) (make-instance 'scheduler
				:name (name rt-server)
				:server rt-server)
	(tempo-clock rt-server) (make-instance 'tempo-clock
				  :name (name rt-server)
				  :server rt-server)))


(let ((semaphore-table (make-hash-table)))
  (defun get-semaphore-by-thread ()
    "Return one semaphore per thread."
    (let* ((semaphore (gethash (bt:current-thread) semaphore-table)))
      (unless semaphore
	(let ((new-semaphore #+ccl (ccl:make-semaphore)
			     #+sbcl (sb-thread:make-semaphore)
			     #+ecl (mp:make-semaphore)
			     #+lispworks (mp:make-semaphore :count 0)))
	  (setf (gethash (bt:current-thread) semaphore-table) new-semaphore
		semaphore new-semaphore)))
      semaphore)))


(defvar *async-in-place* nil)

(defmacro with-async ((&key blocking) &body body)
  "The `sync' function is ignored inside this form. In cl-collider, many functions internally call `sync' (especially those related to buffers). If you want to avoid unnecessary waiting in such cases, you can use `with-async'."
  `(prog1 (let* ((*async-in-place* t))
	    ,@body)
     (when ,blocking
       (sync))))

(defun sync (&optional (rt-server *s*))
  "This function waits until all asynchronous commands on the server are completed. However, if it is called from the server’s response thread, it will be ignored to prevent a deadlock. It is also ignored when used inside the `with-async' macro."
  (if (or (eql (bt:current-thread) (sc-reply-thread rt-server)) *async-in-place*) nil
    (when (typep rt-server 'rt-server)
      (let* ((semaphore (get-semaphore-by-thread))
             (id (assign-id-map-id (sync-id-map rt-server) semaphore)))
        (send-message rt-server "/sync" id)
        #+ccl (ccl:wait-on-semaphore semaphore)
        #+sbcl (sb-thread:wait-on-semaphore semaphore)
        #+ecl (mp:wait-on-semaphore semaphore)
        #+lisworks (mp:semaphore-acquire semaphore)))))

(defmethod server-boot ((rt-server rt-server))
  (assert (not (boot-p rt-server)) nil "~a already running." rt-server)
  (bootup-server-process rt-server)
  (initialize-server-responder rt-server)
  (labels ((bootup ()
	     (let* ((try-count 0))
	       (send-message rt-server "/sync" -1)
	       (loop while (not (thread-wait-with-timeout (lambda () (boot-p rt-server)) 30))
		     do (incf try-count)
			(when (> try-count 30) (return))
			(send-message rt-server "/sync" -1))
	       (when (and (not (boot-p rt-server))
			  (sc-thread rt-server)
			  (bt:thread-alive-p (sc-thread rt-server)))
		 (bootup)))))
    (bootup))
  (unless (boot-p rt-server)
    (cleanup-server rt-server)
    (error "Server failed to boot."))
  (when (boot-p rt-server)
    (setf (node-watcher rt-server) (list 0))
    (send-message rt-server "/notify" 1)
    (send-message rt-server "/status")
    (sched-run (scheduler rt-server))
    (tempo-clock-run (tempo-clock rt-server))
    (sync rt-server)
    (server-free-all rt-server)
    (let ((options (server-options rt-server)))
      (setf (id rt-server) (list #-sbcl 999 #+sbcl 1000)
	    (group-id rt-server) 1
	    (node-proxy-table rt-server) (make-hash-table)
            (buffers rt-server) (make-array (server-options-num-sample-buffers options) :initial-element nil)
            (audio-buses rt-server) (make-array (server-options-num-audio-bus options) :initial-element nil)
            (control-buses rt-server) (make-array (server-options-num-control-bus options) :initial-element nil))
      (dotimes (i (server-options-num-output-bus options))
	(get-next-bus rt-server :audio 1 i))
      (dotimes (i (server-options-num-input-bus options))
	(get-next-bus rt-server :audio 1 (+ i (server-options-num-output-bus options))))))
  (dolist (f *server-boot-hooks*)
    (funcall f))
  rt-server)

(defmethod server-quit ((rt-server rt-server))
  (assert (boot-p rt-server) nil "~a is not running." rt-server)
  (group-free-all 0)
  (send-message rt-server "/quit")
  (thread-wait (lambda () (not (boot-p rt-server))))
  (setf (node-watcher rt-server) nil)
  (setf (sample-rate rt-server) nil)
  (sched-stop (scheduler rt-server))
  (tempo-clock-stop (tempo-clock rt-server))
  (cleanup-server rt-server)
  (dolist (f *server-quit-hooks*)
    (funcall f))
  rt-server)

(defun add-reply-responder (cmd handler)
  (install-reply-responder *s* cmd handler))

(defun remove-reply-responder (cmd)
  (uninstall-reply-responder *s* cmd))

(defun process-buffer-complete-handle (server key &optional values)
  (let* ((handlers (gethash key (buffer-get-handlers server))))
    (when handlers
      (funcall (car handlers) (if values values (elt (buffers server) (second key))))
      (bt:with-lock-held ((server-lock server))
	(setf (gethash key (buffer-get-handlers server)) (cdr handlers))))))

(defun initialize-server-responder (rt-server)
  (let ((*s* rt-server))
    (add-reply-responder
     "/done"
     (lambda (path &rest options)
       (cond ((string= path "/quit") (setf (boot-p rt-server) nil))
	     ((char= (elt path 1) #\b) (process-buffer-complete-handle rt-server (list path (car options)))))))
    (add-reply-responder
     "/status.reply"
     (lambda (&rest args)
       (if (not (sample-rate *s*)) (setf (sample-rate *s*) (car (last args 2)))
         (progn
           (apply #'format t "~&UGens    : ~4d~&Synths   : ~4d~&Groups   : ~4d~&SynthDefs: ~4d~&% CPU (Average): ~a~&% CPU (Peak)   : ~a~&SampleRate (Nominal): ~a~&SampleRate (Actual) : ~a~%" (cdr args))
           (force-output)))))
    (add-reply-responder
     "/synced"
     (lambda (id)
       (case id
	 (-1 (setf (boot-p rt-server) t))
	 (otherwise (let ((semaphore (id-map-free-object (sync-id-map rt-server) id)))
		      #+ccl (ccl:signal-semaphore semaphore)
		      #+sbcl (sb-thread:signal-semaphore semaphore)
		      #+ecl (mp:signal-semaphore semaphore)
		      #+lispworks (mp:semaphore-release semaphore))))))
    (add-reply-responder
     "/c_set"
     (lambda (bus value)
       (funcall (gethash bus (control-get-handlers rt-server)) value)))
    (add-reply-responder
     "/b_set"
     (lambda (bufnum index value)
       (process-buffer-complete-handle rt-server (list "/b_set" bufnum index) value)))
    (add-reply-responder
     "/b_setn"
     (lambda (bufnum start frames &rest values)
       (process-buffer-complete-handle rt-server (list "/b_setn" bufnum start frames) values)))
    (add-reply-responder
     "/b_info"
     (lambda (bufnum frames chanls sr)
       (let ((buffer (elt (buffers rt-server) bufnum)))
	 (setf (slot-value buffer 'frames) frames
	       (slot-value buffer 'chanls) chanls
	       (slot-value buffer 'sr) sr))))
    (add-reply-responder
     "/fail"
     (lambda (&rest args)
       (declare (ignore args))))
    (add-reply-responder
     "/n_go"
     (lambda (id &rest args)
       (declare (ignore args))
       (pushnew id (node-watcher rt-server))))
    (add-reply-responder
     "/n_end"
     (lambda (id &rest args)
       (declare (ignore args))
       (alexandria:removef (node-watcher rt-server) id)))
    (add-reply-responder
     "/d_removed" (lambda (&rest args) (declare (ignore args))))
    (add-reply-responder
     "/n_move" (lambda (&rest args) (declare (ignore args))))))

(defun control-get (index &optional action)
  (let ((result nil)
	(index (floor (floatfy index))))
    (setf (gethash index (control-get-handlers *s*)) (if action action (lambda (value) (setf result value))))
    (message-distribute nil (list "/c_get" index) *s*)
    (unless action
      (sync)
      result)))

(defun control-set (index value)
  (message-distribute nil (list "/c_set" (floatfy index) value) *s*))

#+(or linux freebsd)
(defun jack-connect (&key (client-name "SuperCollider") (input-name "system:capture") (output-name "system:playback"))
  (loop for i from 0 below (server-options-num-input-bus (server-options *s*))
	do (uiop:run-program (format nil "jack_connect ~a_~d ~a:in_~d" input-name (+ i 1) client-name (+ i 1))
			     :ignore-error-status t))
  (loop for i from 0 below (server-options-num-output-bus (server-options *s*))
	do (uiop:run-program (format nil "jack_connect ~a:out_~d ~a_~d" client-name (+ i 1) output-name (+ i 1))
			     :ignore-error-status t)))

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
  (remove-if-not #'boot-p *all-rt-servers*))

(defmethod is-local-p ((rt-server external-server))
  (string= (host rt-server) "127.0.0.1"))

(defmethod sc-reply-thread ((rt-server external-server))
  (sc-osc::listening-thread (osc-device rt-server)))

(defvar *window-server-sleep-time* 2)

(defmethod bootup-server-process ((rt-server external-server))
  (unless (just-connect-p rt-server)
    (setf (sc-thread rt-server)
	  (bt:make-thread
	   (lambda () (sc-program-run (full-pathname *sc-synth-program*)
				      (append
				       (list "-u" (write-to-string (port rt-server)))
				       (build-server-options (server-options rt-server)))))
	   :name "scsynth")))
  #+windows (sleep *window-server-sleep-time*) ; Wait on server boot...It's very temporal.
  (with-slots (osc-device) rt-server
    (setf osc-device (sc-osc:osc-device (host rt-server) (port rt-server) :local-port 0 :debug-msg t))))

(defmethod cleanup-server ((rt-server external-server))
  (when (sc-thread rt-server)
    (bt:join-thread (sc-thread rt-server)))
  (sc-osc:close-device (osc-device rt-server)))

(defmethod server-quit ((rt-server external-server))
  (if (just-connect-p rt-server) (progn
				   (assert (boot-p rt-server) nil "~a is not running." rt-server)
				   (send-message rt-server "/notify" 0)
				   (setf (boot-p rt-server) nil)
				   (sc-osc:close-device (osc-device rt-server))
				   (sched-stop (scheduler rt-server))
				   (tempo-clock-stop (tempo-clock rt-server)))
    (call-next-method)))

(defmethod install-reply-responder ((rt-server external-server) cmd-name f)
  (sc-osc:add-osc-responder (osc-device rt-server) cmd-name f))

(defmethod uninstall-reply-responder ((rt-server external-server) cmd-name)
  (sc-osc:remove-osc-responder (osc-device rt-server) cmd-name))

(defmethod send-message ((server external-server) &rest msg)
  (apply #'sc-osc:send-message (osc-device server) msg))

(defmethod send-bundle ((server external-server) time list-of-messages)
  (apply #'sc-osc:send-bundle
	 (round (* (+ (+ time (latency server)) osc::+unix-epoch+) sc-osc::+2^32+))
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
		 :just-connect-p just-connect-p))

;; cleanup server
(labels ((cleanup-server ()
	   (dolist (server (all-running-servers))
	     (server-quit server))))
  #+ccl (push #'cleanup-server ccl::*lisp-cleanup-functions*)
  #+sbcl (push #'cleanup-server sb-ext:*exit-hooks*)
  #+ecl (push (lambda ()
		(let* ((thead (bt:make-thread #'cleanup-server)))
		  (bt:join-thread thread)))
	      si:*exit-hooks*)
  #+lispworks (lispworks:define-action "When quitting image" "cleanup scsynth"
		#'(lambda () (cleanup-server) t)))



;;; -------------------------------------------------------
;;; Non Realtime Server
;;; -------------------------------------------------------

(defvar *nrt-pad* nil)

(defclass nrt-server (server)
  ((streams :initarg :streams :initform nil :accessor streams)))

(defmethod boot-p ((server nrt-server))
  t)

(defmethod sc-reply-thread ((server nrt-server))
  (bt:current-thread))

(defmethod send-message ((server nrt-server) &rest msg)
  (send-bundle server 0.0d0 msg))

(defmethod send-bundle ((server nrt-server) time list-of-messages)
  (declare (type double-float time))
  (push (list (round (* time sc-osc::+2^32+)) list-of-messages) (streams server)))

(defmacro with-rendering ((output-files &key (pad nil) (keep-osc-file nil) (format :int24) (sr 44100)
					  (clock-bpm (if *s* (clock-bpm) 60.0d0)) (num-of-output 2)) &body body)
  (alexandria:with-gensyms (osc-file file-name non-realtime-stream message bpm)
    `(let* ((,bpm ,clock-bpm)
	    (,file-name (full-pathname ,output-files))
	    (,osc-file (cat (subseq ,file-name 0 (position #\. ,file-name)) ".osc"))
	    (*s* (make-instance 'nrt-server :name "NRTSynth" :streams nil
				:server-options (make-server-options))))
       (setf (buffers *s*) (make-array (server-options-num-sample-buffers (server-options *s*))
			     :initial-element nil)
	     (tempo-clock *s*) (make-instance 'tempo-clock
                                 :server *s*
                                 :bpm ,bpm
				 :timestamp (lambda () 0.0d0))
	     (node-proxy-table *s*) (make-hash-table))
       (let* ((*nrt-pad* ,pad))
	 (make-group :id 1 :pos :head :to 0)
	 ,@body
	 (when *nrt-pad* (send-bundle *s* (* 1.0d0 *nrt-pad*) (list "/c_set" 0 0)))
	 (with-open-file (,non-realtime-stream ,osc-file :direction :output :if-exists :supersede
							 :element-type '(unsigned-byte 8))
	   (dolist (,message (sort (streams *s*) #'<= :key #'car))
	     (when (and ,pad (> (car ,message) (* ,pad sc-osc::+2^32+))) (return))
	     (let ((,message (sc-osc::encode-bundle (second ,message) (car ,message))))
	       (write-sequence (osc::encode-int32 (length ,message)) ,non-realtime-stream)
	       (write-sequence ,message ,non-realtime-stream))))
	 (sc-program-run (full-pathname *sc-synth-program*)
			 (list "-U" (format nil
					    #-windows "~{~a~^:~}"
					    #+windows "~{~a~^;~}"
					    (mapcar #'full-pathname *sc-plugin-paths*))
			       "-o" (write-to-string ,num-of-output)
			       "-N" ,osc-file
			       "_" ,file-name ,(write-to-string sr) (string-upcase (pathname-type ,file-name))
			       (ecase ,format
				 (:int16 "int16")
				 (:int24 "int24")
				 (:float "float")
				 (:double "double"))))
	 (unless ,keep-osc-file
	   (delete-file ,osc-file))
	 (values)))))


;;; -------------------------------------------------------
;;; Node
;;; -------------------------------------------------------

(defclass node ()
  ((server :initarg :server :reader server)
   (id :initarg :id :reader id)
   (pos :initarg :pos :reader pos)
   (to :initarg :to :reader to)
   (name :initarg :name :reader name)
   (meta :initarg :meta :initform nil :accessor meta)))

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
    (:replace 4)
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
    (apply #'list 9 name id (node-to-pos pos) target args)))

(defun ctrl (node &rest param &key &allow-other-keys)
  (with-node (node id server)
    (let* ((args (loop :for (key val) :on param :by #'cddr
		       :append (list (if (numberp key) key (string-downcase key)) (floatfy val)))))
      (message-distribute node (cons 15 (cons id args)) server))))

(defun map-bus (node &rest param &key &allow-other-keys)
  "Map a bus or buses onto the specified controls of a node."
  (with-node (node id server)
    (message-distribute node (append (list "/n_map" id) (mapcar (lambda (p) (cond ((symbolp p) (string-downcase p))
										  (t (floatfy p))))
                                                                param))
                        server)))


;; Move Nodes
(defun move-node (node target action)
  "move node to target by action"
  (let* ((command (ecase action
		    (:before "/n_before")
		    (:after "/n_after")
		    (:head "/g_head")
		    (:tail "/g_tail")))
	 (server))
    (sc::with-node (node id s)
      (setf server s)
      (sc::with-node (target target-id s)
	(if (find action '(:before :after)) (message-distribute node (list command id target-id) server)
	  (message-distribute node (list command target-id id) s))))))



;; Free Node
(defmethod free ((node node))
  (with-node (node id server)
    (message-distribute node (list "/n_free" id) server)))

(defmethod free ((node fixnum))
  (send-message *s* "/n_free" node))

(defmethod free ((node symbol))
  (with-node (node id server)
    (message-distribute node (list "/n_free" id) server)))

(defun release (node)
  "Set the gate control of NODE to 0, releasing the node."
  (ctrl node :gate 0))

(defun is-playing-p (node)
  (when node
    (with-node (node id server)
      (when (find id (node-watcher server))
	t))))

;;; Group
(defclass group (node)
  ())

(defmethod print-object ((node group) stream)
  (format stream "#<~s :server ~s :id ~s>" 'group (server node) (id node)))

(defun make-group (&key id (server *s*) (pos :after) (to 1))
  (with-node (to target-id rt-server)
    (unless (numberp to)
      (assert (eql rt-server server) nil "Target server is not group's server. (/= ~a ~a)" rt-server server))
    (let* ((group-id (if id id (bt:with-lock-held ((server-lock server))
				 (incf (group-id server)))))
	   (group (make-instance 'group :server server :id group-id :pos pos :to target-id)))
      (message-distribute group (list "/g_new" group-id (node-to-pos pos) target-id) server)
      (sync server)
      group)))

(defun server-query-all-nodes (&optional (rt-server *s*))
  (send-message rt-server "/g_dumpTree" 0 0))

(defun group-free-all (group)
  "Frees all nodes in GROUP but not GROUP itself."
  (with-node (group id server)
    (send-message server "/g_freeAll" id)))

(defvar *server-free-all-hooks* nil)

(defun server-free-all (&optional (rt-server *s*))
  "Frees all nodes and clears scheduler on RT-SERVER."
  (let ((*s* rt-server))
    (sched-clear (scheduler rt-server))
    (tempo-clock-clear (tempo-clock rt-server))
    (send-message rt-server "/g_freeAll" 0)
    (send-message rt-server "/clearSched")
    (make-group :id 1 :pos :head :to 0)
    (dolist (hook *server-free-all-hooks*)
      (funcall hook))))

(defvar *stop-hooks* nil)

(defun stop (&optional (group 1) &rest groups)
  (sched-clear (scheduler *s*))
  (tempo-clock-clear (tempo-clock *s*))
  (send-message *s* "/clearSched")
  (dolist (group (cons group groups))
    (send-message *s* "/g_freeAll" group))
  (dolist (hook *stop-hooks*)
    (funcall hook)))

(defun server-status (&optional (rt-server *s*))
  (send-message rt-server "/status"))

;;; scheduler
(defun callback (time f &rest args)
  (if (typep *s* 'rt-server)
      (apply #'sched-add (scheduler *s*) time f args)
    (if *nrt-pad* (when (< time *nrt-pad*)
                    (apply f args))
      (apply f args))))

(defun now ()
  (sched-time (scheduler *s*)))

(defun quant (next-time &optional (offset .5))
  (sched-quant (scheduler *s*) next-time offset))

;;; tempo-clock
(defun set-clock (new-clock)
  (stop)
  (tempo-clock-stop (tempo-clock *s*))
  (setf (tempo-clock *s*) new-clock)
  (setf (server new-clock) *s*)
  (tempo-clock-run (tempo-clock *s*)))

(defun clock-bpm (&optional bpm)
  (tempo-clock-bpm (tempo-clock *s*) bpm))

(defun clock-beats ()
  (tempo-clock-beats (tempo-clock *s*)))

(defun clock-quant (quant)
  (tempo-clock-quant (tempo-clock *s*) quant))

(defun clock-dur (beat)
  (with-slots (beat-dur) (tempo-clock *s*) 
    (* beat-dur beat)))

(defun clock-add (beat function &rest args)
  (if (typep *s* 'rt-server)
      (tempo-clock-add (tempo-clock *s*) beat (lambda () (apply function args)))
    (if *nrt-pad* (when (< (clock-dur beat) *nrt-pad*)
                    (apply function args))
      (apply function args))))

(defun clock-clear ()
  (tempo-clock-clear (tempo-clock *s*)))

(defmacro at-beat (beat &body body)
  (alexandria:with-gensyms (clock)
    `(let* ((,clock (tempo-clock *s*)))
       (at (+ (beats-to-secs ,clock ,beat) (timing-offset ,clock))
	 ,@body))))

(defmacro at-task (beat &body body)
  `(clock-add (+ ,beat (* (sched-ahead (server (tempo-clock *s*))) (/ (clock-bpm) 60.0d0)))
              (lambda () ,@body)))


