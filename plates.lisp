(in-package :wind-loads)

(defvar *type-place* "a")

(defclass tower ()
  ((dx :accessor dx :initarg :dx)  ;;размер ПО х
   (dy :accessor dy :initarg :dy)  ;; размер ПО у
   (Cxi :accessor Cxi :initarg :Cxi :initform 1.4)
   (Ax-sector :accessor Ax-sector :initarg :Ax-sector)
   (Ay-sector :accessor Ay-sector :initarg :Ay-sector)
   (Ak-sector-x :accessor Akx :initarg :Akx)
   (Ak-sector-y :accessor Aky :initarg :Aky)
   (z-list :accessor z-list :initarg :z-list)))

(defclass plates (tower)
  ((z :accessor z :initarg :z)
   (a-ogr-ekv :accessor a-ogr-ekv :initarg :a-ogr-ekv :initform 0.23)
   (step-beam-x :accessor step-beam-x :initarg :step-beam-x :initform 0.8)
   (step-beam-y :accessor step-beam-y :initarg :step-beam-y :initform 0.8)
   (beam-length-x :accessor beam-length-x :initarg :beam-length-x :initform 0)
   (beam-length-y :accessor beam-length-y :initarg :beam-length-y :initform 0)
   (h-beam-x :accessor h-beam-x :initarg :h-beam-x :initform 0.2)
   (h-beam-y :accessor h-beam-y :initarg :h-beam-y :initform 0.2)))
	
(defmethod wind-handrai ((obj1 plates)) ;;можно добавить ветер вдоль ограждения с h-ekv=0.05
  (let (k (c 1.4) wx wy)
    (setf  k (veter-k *tip-m* (z obj1))
	   wx (ocrugl-1.000 (* *w* k c (dy obj1) (a-ogr-ekv obj1) 0.5))
	   wy (ocrugl-1.000 (* *w* k c (dx obj1) (a-ogr-ekv obj1) 0.5)))
    ;;ветер по умолчанию, без вырезов под проходы и эстакады (для быстрого прототипирования)
    (format t "ветер на площадку с отметкой ~s м от ограждения, (тс)" (z obj1))
    (format t "~%~a~a~a~a~a~%" "wx " wx "(тс) wy " wy "(тс)")
	(format t "~a~%~%" "(ветер вдоль ограждения не считается, учитывается 2 ряда ограждения площадки)")))

;;(progn (defvar k) (defvar c 1.4) (defvar kx-shadowing) (defvar ky-shadowing) (defvar Lx) (defvar Ly)  (defvar Hx) (defvar Hy))
;;(dolist (x '(k c kx-shadowing ky-shadowing Lx Ly Hx Hy)) (defvar x))

(defmacro defvar-list (list-param) ;; (defvar-list '(k c kx-shadowing ky-shadowing Lx Ly Hx Hy))
  `(dolist (x ,list-param) (defvar x)))
(defvar-list '(k c kx-shadowing ky-shadowing Lx Ly Hx Hy))

(defmethod beam-wind ((obj1 plates))
  (setf c 1.4
   kx-shadowing (beam-shadowing (step-beam-x obj1) (h-beam-x obj1))
   ky-shadowing (beam-shadowing (step-beam-y obj1) (h-beam-y obj1))
   k (veter-k *tip-m* (z obj1))
   Lx (beam-length-x obj1) Ly (beam-length-y obj1)  ;; нет переменных в привязках let
   Hx (h-beam-x obj1) Hy (h-beam-y obj1)				;; --//--
   )
   
  (print* "ветер от балок по направлению Х ="  '(*w* k c kx-shadowing Lx Hx 0.25) " (тс)")  ;;переменные должны быть глобальными
  (print* "ветер от балок по направлению Y ="  '(*w* k c ky-shadowing Ly Hy 0.25) " (тс)"))  ;; --//--


; расчет площадок башни
(defun plate (&key dx dy z beam-length-x h-beam-x step-beam-x beam-length-y h-beam-y step-beam-y) 
;; (plate :z 2.5 :dx 4.5 :dy 4 :step-beam-x 0.8 :beam-length-x 10 :h-beam-x 0.22 :step-beam-y 0.8 :beam-length-y 10 :h-beam-y 0.22)
  (let (p)
    (setf p (make-instance 
	     'plates :z z :dx dx :dy dy 
	     :step-beam-x step-beam-x :beam-length-x beam-length-x :h-beam-x h-beam-x
	     :step-beam-y step-beam-y :beam-length-y beam-length-y :h-beam-y h-beam-y))
		(format t "~%~a~%~%" "ветер считается значение в один из 4-х узлов площадки")
    (wind-handrai p)  ;;запустили метод с расчетом нагрузок площадки
    (beam-wind p)))

(defmacro interval-value (p v- v+)  ;; (interval-value phi v 0.1 0.6)
  `(progn (if (> (eval ,p) ,v+)(set ,p ,v+))
	  (if (< (eval ,p) ,v-)(set ,p ,v-))))
		
(defmacro interval-value-list (&body list-param)
  `(dolist (x (list ,@list-param))  (interval-value (nth 0 x) (nth 1 x) (nth 2 x))))
	   ;;=>
;; (macroexpand '(interval-value-list '(phi-x 0.1 0.6)  '(phi-y 0.1 0.6) '(bh-x 0.5 6) (list bh-y 0.5 6) )
;;(dolist (x (list (list phi-x 0.1 0.6) (list phi-y 0.1 0.6) (list bh-x 0.5 6) (list bh-y 0.5 6))) 
;;		(interpol-value (nth 0 x) (nth 1 x) (nth 2 x)))

(defun etta (&key b-h phi)
  (let (x y massive etta)
    (setf x (vector 0.5 1 2 4 6)
	  y (vector 0.1 0.2 0.3 0.4 0.5 0.6)
	  massive (make-array 
		   '(6 5) :initial-contents  
		   '((0.93 0.99 1 1 1)
		     (0.75 0.81 0.87 0.9 0.93)
		     (0.56 0.65 0.73 0.78 0.83)
		     (0.38 0.48 0.59 0.65 0.72)
		     (0.19 0.32 0.44 0.52 0.61)
		     (0 0.15 0.3 0.4 0.5)))
	  etta (plosk-interpol x y massive b-h phi))
    (values etta)))

(defun dh (z-list)  ;; (dh (list 2.5 5 6))
  (let ((zi 0) m)
    (dolist (x z-list) (progn (push (- x zi) m) (setq zi x)))
    (values (reverse m))
    ))

(defmethod tower-ct ((obj1 tower))
  (let (cxx cxy cxi Akx Aky etta-x etta-y z-list ctx cty dh-list k-list mx my wx wy Ax Ay
	    bh-x bh-y
	    dx dy
	    phi-x phi-y
	    )
    (setf cxi (cxi obj1)
	  Akx (Akx obj1) Aky (Aky obj1)
	  Ax (Ax-sector obj1) Ay (Ay-sector obj1)
	  dx (dx obj1) dy (dy obj1)
	  z-list (z-list obj1)
	  cxx (* cxi Ax (expt Akx -1)) cxy  (* cxi Ay (expt Aky -1))
	  phi-x (/ Ax Akx) phi-y (/ Ay Aky)
	  bh-x (/ dx dy) bh-y (/ dy dx)
	  )
    (interval-value-list '(phi-x 0.1 0.6)  '(phi-y 0.1 0.6) '(bh-x 0.5 6) '(bh-y 0.5 6))
    (setf etta-x (etta :b-h bh-x :phi phi-x) etta-y (etta :b-h bh-y :phi phi-y))
    (setf ctx (* cxx (+ 1 etta-x)) cty (* cxy (+ 1 etta-y)))
    (setf dh-list (dh z-list) k-list (veter-k-list* *type-place* z-list))
    (let ((i 0) (max-i (length z-list)))
      (loop 
	 (setf wx (* *w* (nth i k-list) ctx (* dy (nth i dh-list)) 0.25)   ;; на 4 узла
	       wy (* *w* (nth i k-list) cty (* dx (nth i dh-list)) 0.25))
	 (push (ocrugl-1.00 wx) mx) (push (ocrugl-1.00 wy) my)
	 (incf i)
	 (if (= i max-i) (return))))
    
    (format t "~a~%" "Нагрузки по Х ")
    (format t "~a~a~%" (reverse mx) "тс")
    (format t "~a~%" "Нагрузки по Y ")
    (format t "~a~a~%" (reverse my) "тс")))

		; расчет ветра на каркас башни
(defun tower-run (&key dx dy z-list Ax-sector Ay-sector Akx Aky)
  ;; (tower-run :dx 6 :dy 6 :Ax-sector 6.36 :Ay-sector 6.76 :Akx (* 6 5) :Aky (* 6 5) :z-list '(5.46 10.46 15.46) )
  (let (t*)
    (setf t* (make-instance 'tower :dx dx :dy dy :z-list z-list :Ax-sector Ax-sector :Ay-sector Ay-sector :Akx Akx :Aky Aky ))
    (tower-ct t*)))
