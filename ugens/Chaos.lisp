
(in-package #:sc)

(defugen (quad-n "QuadN")
    (&optional (freq 22050) (a 1) (b -1) (c -0.75) (xi 0) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen freq a b c xi) mul add))))

(defugen (quad-l "QuadL")
    (&optional (freq 22050) (a 1) (b -1) (c -0.75) (xi 0) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen freq a b c xi) mul add))))

(defugen (quad-c "QuadC")
    (&optional (freq 22050) (a 1) (b -1) (c -0.75) (xi 0) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen freq a b c xi) mul add))))

(defugen (cusp-n "CuspN")
    (&optional (freq 22050) (a 1) (b 1.9) (xi 0) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen freq a b xi) mul add))))

(defugen (cusp-l "CuspL")
    (&optional (freq 22050) (a 1) (b 1.9) (xi 0) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen freq a b xi) mul add))))


(defugen (gbman-n "GbmanN")
    (&optional (freq 22050) (xi 1.2) (yi 2.1) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen freq xi yi) mul add))))

(defugen (gbman-l "GbmanL")
    (&optional (freq 22050) (xi 1.2) (yi 2.1) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen freq xi yi) mul add))))

(defugen (henon-n "HenonN")
    (&optional (freq 22050) (a 1.4) (b 0.3) (x0 0) (x1 0) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen freq a b x0 x1) mul add))))

(defugen (henon-l "HenonL")
    (&optional (freq 22050) (a 1.4) (b 0.3) (x0 0) (x1 0) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen freq a b x0 x1) mul add))))

(defugen (henon-c "HenonC")
    (&optional (freq 22050) (a 1.4) (b 0.3) (x0 0) (x1 0) &key (mul 1) (add 0))
  ((:ar (madd (multinew new 'ugen freq a b x0 x1) mul add))))

(defugen (latoocarfian-n "LatoocarfianN")
    (&optional (freq 22050) (a 1) (b 3) (c 0.5) (d 0.5) (xi 0.5) (yi 0.5)
	       &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq a b c d xi yi) mul add))))

(defugen (latoocarfian-l "LatoocarfianL")
    (&optional (freq 22050) (a 1) (b 3) (c 0.5) (d 0.5) (xi 0.5) (yi 0.5)
	       &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq a b c d xi yi) mul add))))

(defugen (latoocarfian-c "LatoocarfianC")
    (&optional (freq 22050) (a 1) (b 3) (c 0.5) (d 0.5) (xi 0.5) (yi 0.5)
	       &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq a b c d xi yi) mul add))))

(defugen (lincong-n "LinCongN")
    (&optional (freq 22050) (a 1.0) (c 0.13) (m 1.0) (xi 0) &key (mul 1.0) (add 0))
  ((:ar (madd (multinew new 'ugen freq a c m xi) mul add))))

(defugen (lincong-l "LinCongL")
    (&optional (freq 22050) (a 1.0) (c 0.13) (m 1.0) (xi 0) &key (mul 1.0) (add 0))
  ((:ar (madd (multinew new 'ugen freq a c m xi) mul add))))

(defugen (lincong-c "LinCongC")
    (&optional (freq 22050) (a 1.0) (c 0.13) (m 1.0) (xi 0) &key (mul 1.0) (add 0))
  ((:ar (madd (multinew new 'ugen freq a c m xi) mul add))))

(defugen (standard-n "StandardN")
    (&optional (freq 22050) (k 1.0) (xi 0.5) (yi 0) &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq k xi yi) mul add))))

(defugen (standard-l "StandardL")
    (&optional (freq 22050) (k 1.0) (xi 0.5) (yi 0) &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq k xi yi) mul add))))

(defugen (fbsine-n "FBSineN")
    (&optional (freq 22050) (im 1) (fb 0.1) (a 1.1) (c 0.5) (xi 0.1) (yi 0.1)
	       &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq im fb a c xi yi) mul add))))

(defugen (fbsine-l "FBSineL")
    (&optional (freq 22050) (im 1) (fb 0.1) (a 1.1) (c 0.5) (xi 0.1) (yi 0.1)
	       &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq im fb a c xi yi) mul add))))

(defugen (fbsine-c "FBSineC")
    (&optional (freq 22050) (im 1) (fb 0.1) (a 1.1) (c 0.5) (xi 0.1) (yi 0.1)
	       &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq im fb a c xi yi) mul add))))

(defugen (lorenz-l "LorenzL")
    (&optional (freq 22050) (s 10) (r 28) (b 2.667) (h 0.05) (xi 0.1) (yi 0)
	       (zi 0) &key (mul 1.0) (add 0.0))
  ((:ar (madd (multinew new 'ugen freq s r b h xi yi zi) mul add))))
