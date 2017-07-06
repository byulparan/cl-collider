
(in-package #:sc)

(defugen (mouse-x "MouseX") (&optional (minval 0) (maxval 1) (warp :linear) (lag 0.2))
  ((:kr (multinew new 'ugen minval maxval (ecase warp
                                            (:linear 0)
                                            (:lin 0)
                                            (:exponential 1)
                                            (:exp 1))
                  lag)))
  :signal-range :unipolar)

(defugen (mouse-y "MouseY") (&optional (minval 0) (maxval 1) (warp :linear) (lag 0.2))
  ((:kr (multinew new 'ugen minval maxval (ecase warp
                                            (:linear 0)
                                            (:lin 0)
                                            (:exponential 1)
                                            (:exp 1))
                  lag)))
  :signal-range :unipolar)

(defugen (mouse-button "MouseButton") (&optional (minval 0) (maxval 1) (lag 0.2))
  ((:kr (multinew new 'ugen minval maxval lag)))
  :signal-range :unipolar)

(defugen (key-state "KeyState") (&optional (keycode 0) (minval 0) (maxval 1) (lag 0.2))
  ((:kr (multinew new 'ugen keycode minval maxval lag)))
  :signal-range :unipolar)
