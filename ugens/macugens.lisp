
(in-package #:collider)

(defugen (mouse-x "MouseX") (&optional (minval 0) (maxval 1) (warp :linear) (lag 0.2))
  ((:kr (progn (ecase warp
		 (:linear (setf warp 0))
		 (:exponential (setf warp 1)))
	       (multinew new 'ugen minval maxval warp lag))))
  :signal-range :unipolar)

(defugen (mouse-y "MouseY") (&optional (minval 0) (maxval 1) (warp :linear) (lag 0.2))
  ((:kr (progn (ecase warp
		 (:linear (setf warp 0))
		 (:exponential (setf warp 1)))
	       (multinew new 'ugen minval maxval warp lag))))
  :signal-range :unipolar)

(defugen (mouse-button "MouseButton") (&optional (minval 0) (maxval 1) (lag 0.2))
  ((:kr (multinew new 'ugen minval maxval lag)))
  :signal-range :unipolar)

(defugen (key-state "KeyState") (&optional (keycode 0) (minval 0) (maxval 1) (lag 0.2))
  ((:kr (multinew new 'ugen keycode minval maxval lag)))
  :signal-range :unipolar)
