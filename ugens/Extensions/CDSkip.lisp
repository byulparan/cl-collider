;; https://github.com/nhthn/supercollider-cd-skip

(in-package :sc)

(defugen (cd-skip "CDSkip")
  (in &key (max-delay 2.0) (auto-mode 0) (auto-speed 1) (skip-trigger 0) (reset-trigger 0) (pos 0) (freeze 0) (clean 0))
  ((:ar (unless (eql (rate skip-trigger) :audio)
	  (setf skip-trigger (t2a.ar skip-trigger)))
	(unless (eql (rate reset-trigger) :audio)
	  (setf reset-trigger (t2a.ar reset-trigger)))
	(unless (eql (rate pos) :audio)
	  (setf pos (k2a.ar pos)))
	(multinew new 'multiout-ugen 2 (nth 0 in) (nth 1 in) max-delay auto-mode auto-speed skip-trigger reset-trigger pos freeze clean))))


