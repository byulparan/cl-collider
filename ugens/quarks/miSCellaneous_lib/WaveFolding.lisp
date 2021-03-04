(in-package #:sc)

(defun smooth-clip-s-gen (rate in lo hi amount delta)
  (let* ((dc-ugen (if (eql rate :control) #'dc.kr #'dc.ar))
	 (dif (-~ hi lo))
	 (sum (+~ hi lo))
	 (dif (*~ (max~ (abs~ dif) delta)
		 (-~ (*~ (>=~ dif (funcall dc-ugen 0)) 2) 1)))
	 (from-factor (/~ 2 dif))
	 (from-offset (+~ (*~ from-factor (neg hi)) 1))
	 (in (+~ (*~ from-factor in) from-offset))
	 (x0 (min~ (-~ 1.0 amount) (-~ 1.0 delta)))
	 (w (*~ x0 pi 0.5))
	 (p (/~ 1.0 (+~ (-~ 1.0 (sin~ w)) (*~ w (cos~ w)))))
	 (slope (*~ p pi 0.5 (cos~ w)))
	 (lin-sig (*~ slope in))
	 (sine-sig (*~ (+~ (*~ (-~ (sin~ (*~ (abs~ in) pi 0.5)) 1.0) p) 1.0) (sign in)))
	 (case-var (+~ (>~ (abs~ in) x0) (>=~ (abs~ in) 1) (<=~ in -1)))
	 (select-ugen (if (eql rate :control) #'select.kr #'select.ar)))
    (funcall select-ugen case-var
	     (*~
	      (+~
	       (*~ (list lin-sig sine-sig (funcall dc-ugen 1.0) (funcall dc-ugen -1.0))
		   dif)
	       sum)
	      0.5))))

(defun smooth-clip-s.ar (in &optional (lo -1.0) (hi 1.0) (amount 0.5) (delta 0.00001))
  (multinew #'smooth-clip-s-gen :audio in lo hi amount delta))

(defun smooth-clip-s.kr (in &optional (lo -1.0) (hi 1.0) (amount 0.5) (delta 0.00001))
  (multinew #'smooth-clip-s-gen :control in lo hi amount delta))

(defun smooth-clip-q-gen (rate in lo hi amount delta)
  (let* ((dc-ugen (if (eql rate :control) #'dc.kr #'dc.ar))
	 (dif (-~ hi lo))
	 (sum (+~ hi lo))
	 (dif (*~ (max~ (abs~ dif) delta)
		 (-~ (*~ (>=~ dif (funcall dc-ugen 0)) 2) 1)))
	 (from-factor (/~ 2 dif))
	 (from-offset (+~ (*~ from-factor (neg hi)) 1))
	 (in (+~ (*~ from-factor in) from-offset))
	 (x0 (min~ (-~ 1.0 amount) (-~ 1.0 delta)))
	 (p (/~ 1.0 (-~ (*~ x0 x0) 1.0)))
	 (slope (*~ 2.0 p (-~ x0 1.0)))
	 (lin-sig (*~ slope in))
	 (parable-sig (*~ (+~ (*~ (squared (-~ (abs~ in) 1.0)) p) 1.0) (sign in)))
	 (case-var (+~ (>~ (abs~ in) x0) (>=~ (abs~ in) 1.0) (<=~ in -1)))
	 (select-ugen (if (eql rate :control) #'select.kr #'select.ar)))
    (funcall select-ugen case-var
	     (*~
	      (+~
	       (*~
		(list lin-sig parable-sig (funcall dc-ugen 1.0) (funcall dc-ugen -1.0))
		dif)
	       sum)
	      0.5))))

(defun smooth-clip-q.ar (in &optional (lo -1.0) (hi 1.0) (amount 0.5) (delta 0.00001))
  (multinew #'smooth-clip-q-gen :audio in lo hi amount delta))

(defun smooth-clip-q.kr (in &optional (lo -1.0) (hi 1.0) (amount 0.5) (delta 0.00001))
  (multinew #'smooth-clip-q-gen :control in lo hi amount delta))


(defun smooth-fold-s-gen (rate in lo hi fold-range smooth-amount delta)
  (let* ((fold-ugen (if (eql rate :control) #'fold.kr #'fold.ar))
	 (case-var (+~ (<~ in lo) (*~ (>~ in hi) 2.0)))
	 (fold-range-abs (*~ (-~ hi lo) fold-range))
	 (thr-1 (+~ lo fold-range-abs))
	 (thr-2 (-~ hi fold-range-abs))
	 (select-ugen (if (eql rate :control) #'select.kr #'select.ar))
	 (smooth-clip-ugen (if (eql rate :control) #'smooth-clip-s.kr #'smooth-clip-s.ar)))
    (funcall select-ugen case-var
	     (list (funcall smooth-clip-ugen in lo hi smooth-amount delta)
		   (funcall smooth-clip-ugen (funcall fold-ugen in lo thr-1) lo thr-1 smooth-amount delta)
		   (funcall smooth-clip-ugen (funcall fold-ugen in thr-2 hi) thr-2 hi smooth-amount delta)))))

(defun smooth-fold-s.ar (in &optional (lo -1.0) (hi 1.0) (fold-range 1.0) (smooth-amount 0.5) (delta 0.00001))
  (multinew #'smooth-fold-s-gen :audio in lo hi fold-range smooth-amount delta))

(defun smooth-fold-s.kr (in &optional (lo -1.0) (hi 1.0) (fold-range 1.0) (smooth-amount 0.5) (delta 0.00001))
  (multinew #'smooth-fold-s-gen :control in lo hi fold-range smooth-amount delta))

(defun smooth-fold-s2-gen (rate in lo hi fold-range-lo fold-range-hi smooth-amount delta)
  (let* ((fold-ugen (if (eql rate :control) #'fold.kr #'fold.ar))
	 (case-var (+~ (<~ in lo) (*~ (>~ in hi) 2.0)))
	 (range-abs (-~ hi lo))
	 (thr-1 (+~ lo (*~ range-abs fold-range-lo)))
	 (thr-2 (-~ hi (*~ range-abs fold-range-hi)))
	 (select-ugen (if (eql rate :control) #'select.kr #'select.ar))
	 (smooth-clip-ugen (if (eql rate :control) #'smooth-clip-s.kr #'smooth-clip-s.ar)))
    (funcall select-ugen case-var
	     (list (funcall smooth-clip-ugen in lo hi smooth-amount delta)
		   (funcall smooth-clip-ugen (funcall fold-ugen in lo thr-1) lo thr-1 smooth-amount delta)
		   (funcall smooth-clip-ugen (funcall fold-ugen in thr-2 hi) thr-2 hi smooth-amount delta)))))

(defun smooth-fold-s2.ar (in &optional (lo -1.0) (hi 1.0) (fold-range-lo 1.0) (fold-range-hi 1.0) (smooth-amount 0.5) (delta 0.00001))
  (multinew #'smooth-fold-s2-gen :audio in lo hi fold-range-lo fold-range-hi smooth-amount delta))

(defun smooth-fold-s2.kr (in &optional (lo -1.0) (hi 1.0) (fold-range-lo 1.0) (fold-range-hi 1.0) (smooth-amount 0.5) (delta 0.00001))
  (multinew #'smooth-fold-s2-gen :control in lo hi fold-range-lo fold-range-hi smooth-amount delta))


(defun smooth-fold-q-gen (rate in lo hi fold-range smooth-amount delta)
  (let* ((fold-ugen (if (eql rate :control) #'fold.kr #'fold.ar))
	 (case-var (+~ (<~ in lo) (*~ (>~ in hi) 2.0)))
	 (fold-range-abs (*~ (-~ hi lo) fold-range))
	 (thr-1 (+~ lo fold-range-abs))
	 (thr-2 (-~ hi fold-range-abs))
	 (select-ugen (if (eql rate :control) #'select.kr #'select.ar))
	 (smooth-clip-ugen (if (eql rate :control) #'smooth-clip-q.kr #'smooth-clip-q.ar)))
    (funcall select-ugen case-var
	     (list (funcall smooth-clip-ugen in lo hi smooth-amount delta)
		   (funcall smooth-clip-ugen (funcall fold-ugen in lo thr-1) lo thr-1 smooth-amount delta)
		   (funcall smooth-clip-ugen (funcall fold-ugen in thr-2 hi) thr-2 hi smooth-amount delta)))))

(defun smooth-fold-q.ar (in &optional (lo -1.0) (hi 1.0) (fold-range 1.0) (smooth-amount 0.5) (delta 0.00001))
  (multinew #'smooth-fold-q-gen :audio in lo hi fold-range smooth-amount delta))

(defun smooth-fold-q.kr (in &optional (lo -1.0) (hi 1.0) (fold-range 1.0) (smooth-amount 0.5) (delta 0.00001))
  (multinew #'smooth-fold-q-gen :control in lo hi fold-range smooth-amount delta))


(defun smooth-fold-q2-gen (rate in lo hi fold-range-lo fold-range-hi smooth-amount delta)
  (let* ((fold-ugen (if (eql rate :control) #'fold.kr #'fold.ar))
	 (case-var (+~ (<~ in lo) (*~ (>~ in hi) 2.0)))
	 (range-abs (-~ hi lo))
	 (thr-1 (+~ lo (*~ range-abs fold-range-lo)))
	 (thr-2 (-~ hi (*~ range-abs fold-range-hi)))
	 (select-ugen (if (eql rate :control) #'select.kr #'select.ar))
	 (smooth-clip-ugen (if (eql rate :control) #'smooth-clip-q.kr #'smooth-clip-q.ar)))
    (funcall select-ugen case-var
	     (list (funcall smooth-clip-ugen in lo hi smooth-amount delta)
		   (funcall smooth-clip-ugen (funcall fold-ugen in lo thr-1) lo thr-1 smooth-amount delta)
		   (funcall smooth-clip-ugen (funcall fold-ugen in thr-2 hi) thr-2 hi smooth-amount delta)))))

(defun smooth-fold-q2.ar (in &optional (lo -1.0) (hi 1.0) (fold-range-lo 1.0) (fold-range-hi 1.0) (smooth-amount 0.5) (delta 0.00001))
  (multinew #'smooth-fold-q2-gen :audio in lo hi fold-range-lo fold-range-hi smooth-amount delta))

(defun smooth-fold-q2.kr (in &optional (lo -1.0) (hi 1.0) (fold-range-lo 1.0) (fold-range-hi 1.0) (smooth-amount 0.5) (delta 0.00001))
  (multinew #'smooth-fold-q2-gen :control in lo hi fold-range-lo fold-range-hi smooth-amount delta))


(export '(smooth-clip-s.ar smooth-clip-s.kr smooth-clip-q.ar  smooth-clip-q.kr
	  smooth-fold-s.ar smooth-fold-s.kr smooth-fold-s2.ar smooth-fold-s2.kr
	  smooth-fold-q.ar smooth-fold-q.kr smooth-fold-q2.ar smooth-fold-q2.kr))

