;;; This id-map implentation code all from ClozureCL.
;;; I want use it in SBCL/ECL....so just I was copy.(1:1)
;;; I use id-map for sync to scsynth <---> lisp(#'sync).

(in-package #:sc)

;;; ---id map ------------------------------------
(defstruct id-map
  (vector (make-array 1 :initial-element nil))
  (free 0)
  (lock (bt:make-recursive-lock)))

(defun id-map-grow (id-map)
  (let* ((old-vector (id-map-vector id-map))
	 (old-size (length old-vector))
	 (new-size (+ old-size old-size))
	 (new-vector (make-array new-size)))
    (declare (fixnum old-size new-size))
    (dotimes (i old-size)
      (setf (svref new-vector i) (svref old-vector i)))
    (let* ((limit (1- new-size)))
      (declare (fixnum limit))
      (do* ((i old-size (1+ i)))
	   ((= i limit) (setf (svref new-vector i) nil))
	(declare (fixnum i))
	(setf (svref new-vector i) (the fixnum (1+ i)))))
    (setf (id-map-vector id-map) new-vector
	  (id-map-free id-map) old-size)))

(defun assign-id-map-id (id-map object)
  (if (or (null object) (typep object 'fixnum)) (error "OBJECT must not be FIXNUM or NIL"))
  (bt:with-recursive-lock-held ((id-map-lock id-map))
    (let* ((free (or (id-map-free id-map) (id-map-grow id-map)))
           (vector (id-map-vector id-map))
           (newfree (svref vector free)))
      (setf (id-map-free id-map) newfree
            (svref vector free) object)
      free)))

(defun id-map-object (id-map id)
  (let* ((object (bt:with-recursive-lock-held ((id-map-lock id-map))
                   (svref (id-map-vector id-map) id))))
    (if (or (null object) (typep object 'fixnum))
      (error "Invalid index ~d for ~s" id id-map)
      object)))

(defun id-map-free-object (id-map id)
  (bt:with-recursive-lock-held ((id-map-lock id-map))
    (let* ((vector (id-map-vector id-map))
           (object (svref vector id)))
      (if (or (null object) (typep object 'fixnum))
        (error "Invalid index ~d for ~s" id id-map))
      (setf (svref vector id) (id-map-free id-map)
            (id-map-free id-map) id)
      object)))

(defun id-map-modify-object (id-map id old-value new-value)
  (bt:with-recursive-lock-held ((id-map-lock id-map))
    (let* ((vector (id-map-vector id-map))
           (object (svref vector id)))
      (if (or (null object) (typep object 'fixnum))
        (error "Invalid index ~d for ~s" id id-map))
      (if (eq object old-value)
	(setf (svref vector id) new-value)))))
