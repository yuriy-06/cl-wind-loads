(in-package #:wind-loads)
;;(declare (optimize (debug 3)))

(defvar *tip-m* "a")  ;; здесь указывается тип местности по умолчанию
(defvar *vector-z* (vector 0.75 1 1.25 1.5 1.7 1.85 2 2.25 2.45 2.65 2.75 2.75 2.75))
(defvar *vector-h* (vector 5 10 20 40 60 80 100 150 200 250 300 350 480))
(defvar *veter* 0.038) ;тс/м2
(defvar *wall* nil)
(defvar *column* nil)
(defvar e nil)

(defun vector-z ()
  (cond 
   ((equal *tip-m* "a")
    (setf *vector-z*
          (vector 0.75 1 1.25 1.5 1.7 1.85 2 2.25 2.45 2.65 2.75 2.75 2.75)))
   ((equal *tip-m* "b")
    (setf  *vector-z*
           (vector 0.5 0.65 0.85 1.1 1.3 1.45 1.6 1.9 2.1 2.3 2.5 2.75 2.75)))
   ((equal *tip-m* "c")
    (setf *vector-z* 
          (vector 0.4 0.4 0.55 0.8 1 1.15 1.25 1.55 1.8 2 2.2 2.35 2.75)))))

(defun veter-k (a z);ветер , тип местности и координата
; (veter-k "a" 25)
  (cond
   ((> z 480)(setq z 480))
   ((< z 5)(setq z 5)))
  (cond 
   ((equal a "a")
    (lin-interpol 
     (vector 0.75 1 1.25 1.5 1.7 1.85 2 2.25 2.45 2.65 2.75 2.75 2.75) 
     (vector 5 10 20 40 60 80 100 150 200 250 300 350 480) z))
   ((equal a "b")
    (lin-interpol 
     (vector 0.5 0.65 0.85 1.1 1.3 1.45 1.6 1.9 2.1 2.3 2.5 2.75 2.75) 
     (vector 5 10 20 40 60 80 100 150 200 250 300 350 480) z))
   ((equal a "c")
    (lin-interpol 
     (vector 0.4 0.4 0.55 0.8 1 1.15 1.25 1.55 1.8 2 2.2 2.35 2.75)
     (vector 5 10 20 40 60 80 100 150 200 250 300 350 480) z))))

(defun veter-k-list (a list) 
;вызов вида (veter-k-list "a" (list 26.65 23.45 20.95 17.95 14.95 11.95 8.95 5.95 2.95))
  (dolist (x list)
    (print (float (/ (round (* 100 (veter-k a x)) ) 100)))))
	
(defun veter-k-list* (a list*)
	(let (m)
		(dolist (x list*) (push (float (/ (round (* 100 (veter-k a x)) ) 100)) m))
		(values (reverse m))))

(defun ct(&key h b balki-h kol-h a-svyaz shag-ferm n c-kolonna c-balka c-svyaz)
  (let (ak cx b-h fi x y px py etta ctv massive)
  ;;пример вызова 
  ;;(ct :h 4.8 :b 5.5 :balki-h 0.25 :kol-h 0.25 :a-svyaz (* 4.1 0.12 2) :shag-ferm 4 :n 1 :c-kolonna 1.4 :c-balka 1.4 :c-svyaz 1.4)
  ;; где h - высота этажа, b - ширина обдувания, kol-h - высота ПРОФИЛЯ колонны
  (setf ak (* b h) 
        cx (/ (+ (* balki-h b c-balka) 
                 (* kol-h h c-kolonna) 
                 (* a-svyaz c-svyaz))
              (* b h))); где n - количество затеняемых ферм
  (setf h b 
        b-h (/ shag-ferm h) 
        fi (/ (+ (* balki-h b) (* kol-h h) (* a-svyaz 1)) (* b h)))

  (cond
   ((> fi 0.6)(setq fi 0.6))
   ((< fi 0.1)(setq fi 0.1))
   ((> b-h 6)(setq b-h 6))
   ((< b-h 0.5)(setq b-h 0.5)))
  (setf x (vector 0.5 1 2 4 6)
        y (vector 0.1 0.2 0.3 0.4 0.5 0.6)
        massive (make-array 
                 '(6 5) :initial-contents  
                 '((0.93 0.99 1 1 1)
                   (0.75 0.81 0.87 0.9 0.93)
                   (0.56 0.65 0.73 0.78 0.83)
                   (0.38 0.48 0.59 0.65 0.72)
                   (0.19 0.32 0.44 0.52 0.61)
                   (0 0.15 0.3 0.4 0.5))))
  (setf px b-h 
        py fi
        etta (plosk-interpol x y massive px py))
  (setq ctv (* cx (+ 1 (* etta n)) 1))
  (values "cx/y" (ocrugl-1.00 cx) 
          "etta" (ocrugl-1.00 etta) 
          "Ct" (ocrugl-1.00 ctv))))

(defun noriya-diag (&key b-h alfa)
  ;;атлас-2003, табл. 3-9 стр 56 
  ;; (noriya-diag :b-h 2.5 :alfa 45) (noriya-diag :b-h (/ 553 305) :alfa 45)
  ;; Cn - поперек широкой часть ветер, Ct - поперек короткой стороны
  (let (m-cn m-ct cn ct x y)
  (setf m-cn (make-array '(2 3) :initial-contents  '((2.1 1.4 0)                                                       
                                                     (2.1 1.8 0)))
        m-ct (make-array '(2 3) :initial-contents  '((0 0.7 0.75)                                                       
                                                     (0 0.1 0.1))))
  (setf x (vector 0 45 90) y (vector 2 10))
  (cond
   ((> b-h 10)(setq b-h 10))
   ((< b-h 2)(setq b-h 2))
   ((> alfa 90)(setq alfa 90))
   ((< alfa 0)(setq alfa 0)))
  (setf cn (plosk-interpol x y m-cn alfa b-h)
        ct (plosk-interpol x y m-ct alfa b-h))
  (values (list "Cn"  cn  "Ct" (ocrugl-1.000 ct)))))
  
(defun noriya-wind-diag (&key h-list w-type h-ct h-cn)
	(let ()
		))

(defun beam-shadowing (shag h) ; (beam-shadowing 0.5 0.2)  ;;shading-beams
  (let (x y p v)
  (setf x (vector 4 6 8 10)
        y (vector 0.4 0.5 0.8 1)
        p (/ shag h)
        v (lin-interpol y x p))))

(defmacro zaten-balki (p1 p2)  ;;поддержка устаревшего названия
	`(beam-shadowing ,p1 ,p2))

(defun esta-balochnaya (&key rayon w z h-poyas h-prod-balka h-poper-balka h-svaz L-svaz-1
                             L-poper-balka L-prod-balka shag-prod-balok n-prod-balok)
  ;; значение в узел эстакады
  ;; (esta-balochnaya :rayon "a" :w (* 0.038 1.4) :z 11.4 :h-poyas 0.3 :h-prod-balka 0.2 :h-poper-balka 0.3 :h-svaz 0.11 :L-svaz-1 3.45 :L-poper-balka 4.64 :L-prod-balka 2.575 :shag-prod-balok 0.6 :n-prod-balok 6)
  (let* (
	 (k (veter-k rayon z))
        (w-p (* w k 1.4 h-poyas L-prod-balka))  ; ветер в пояс поперек (на один из двух)
        (w-prod-balka (/ (* w k 1.4 (zaten-balki shag-prod-balok h-prod-balka) n-prod-balok L-prod-balka) 2))  ;ветер на продольную балку поперек
        (w-poper-balka (/ (* w k 1.4 h-poper-balka L-poper-balka) 2))   ;ветер на поперечную балку 
        (w-svaz (* w k 1.4 0.5 L-svaz-1 h-svaz)) ; где L-svaz-1 - (половина всех длин связей на ячейку)
        (w-ogr (* w k 1.4 0.25 h-prod-balka))  ; ветер на ограждение (поперек)
        (w-prod (+ w-poper-balka w-svaz))
        (w-poper (+ w-p w-prod-balka w-svaz w-ogr)))
    
  ;;(+ w-poper w-prod)
  (format t  "~{~a~}~%" (list "w поперёк эстакады и вдоль в один узел (без конвейера и ножек) " (ocrugl-1.000 w-poper) " " (ocrugl-1.000 w-prod)))))

(defmacro sc-v-0 (vector) ; (sc-v-0 f1)
  `(progn 
     (setf v (lin-interpol ,vector b* beta))
     (print v)))

(defmacro sc-v (string vector) ; (sc-v "F" f1)
  `(progn (print ,string)
     (sc-v-0 ,vector)))


(defmacro split-interval (v v-min v-max)
  `(cond ((> ,v ,v-max)(setf ,v ,v-max))
         ((< ,v ,v-min)(setf ,v ,v-min))))

(defun scat-2 (&key alfa beta d b zh) ;(scat-2 :alfa 0 :beta 14.5 :d 33 :b 33 :zh 10)
  (let (b* f f1 f2 g g1 g2 h h1 h2 i j v e)
		(setf e (min (* zh 2) b))
    (cond 
      ((equal alfa 0)
       (progn 
	 (print "ветер на скат")
         (split-interval beta 15 75)
	 (setf b* (vector 15 30 45 60 75)
	       f1 (vector -0.9 -0.5  0.7 0.7 0.8)
	       f2 (vector  0.2  0.7  0.7 0.7 0.8)
	       g1 (vector -0.8 -0.5 0.7 0.7 0.8)
	       g2 (vector  0.2  0.7 0.7 0.7 0.8)
	       h1 (vector -0.3 -0.2 0.6 0.7 0.8)
	       h2 (vector  0.2  0.4 0.6 0.7 0.8)
	       i (vector -0.4 -0.4 -0.2 -0.2 -0.2)
	       j (vector -1 -0.5 -0.3 -0.3 -0.3))
	 (print "F")
	 (sc-v-0 f1)
         (sc-v-0 f2)
	 (print "G")
         (sc-v-0 g1)
         (sc-v-0 g2)
	 (print "H")
         (sc-v-0 h1)
	 (sc-v-0 h2)
         (sc-v "I" i)
         (sc-v "J" j)))       

       ((equal alfa 90)
	(progn 
	  (print "ветер в торец")
          (split-interval beta 0 75)
	  (setf b* (vector 0 15 30 45 60 75)
		f (vector -1.8 -1.3 -1.1 -1.1 -1.1 -1.1)
		g (vector -1.3 -1.3 -1.4 -1.4 -1.2 -1.2)
		h (vector -0.7 -0.6 -0.8 -0.9 -0.8 -0.8)
		i (vector -0.5 -0.5 -0.5 -0.5 -0.5 -0.5))
          (sc-v "F" f)
          (sc-v "G" g)
          (sc-v "H" h)
          (sc-v "I" i))))
      (format t "~%~a~%" "крыша")  
      (print "e/2")
      (format t "~%~a~%" (float (/ e 2)))
      (print "e/4")
      (format t "~%~a~%" (float (/ e 4)))
      (print "e/10")
      (format t "~%~a~%" (float (/ e 10)))
    (bok-stena :b b :d d :h zh)))

(defun bok-stena (&key b d h) ; (bok-stena :b 33 :d 108 :h 10)  ;; описывает участки стены и наполняет *wall* объектами 'wall
  (declare (optimize (debug 3)))
  ;;(break "~a" "отладка")
  (setf *wall* nil)
  (if (> b (* 2 h)) 
      (setf e (* 2 h)) 
      (setf e b))
  (format t "~a~a~%" "e=" e)
  
  (format t "~%~a" "участок стены А")
  (format t "~%~a" "L=")
  (format t "~a" (float (/ e 5)))
  (format t "~%~a" "c=")
  (format t "~a~%" -1)
  (push (make-instance 'wall :x1 -0.3 :x2 (/ e 5) :c -1) *wall*)
  (format t "~%~a" "участок стены B")
  (format t "~%~a" "L=")
  (format t "~a"   (float (- e (/ e 5))))
  (format t "~%~a" "c=")
  (format t "~a~%" -0.8)
  (push (make-instance 'wall :x1 (/ e 5) :x2 e :c -0.8) *wall*)
  (format t "~%~a" "все остальное С")
  (format t "~%~a" "c=")
  (format t "~a~%" -0.5)
  (if (> d e)
      (push (make-instance 'wall :x1 e :x2 (+ d 0.3) :c -0.5) *wall*))
  (setf *wall* (reverse *wall*))
  (format t "~%")
  (values "end"))



(defclass kolonna ()
  ((k5 :accessor k5 :initarg :k5 :initform 0.75)
   (k7 :accessor k7 :initarg :k7 :initform 0.85)
   (k10 :accessor k10 :initarg :k10 :initform 1)
   (z-list :accessor z-list :initarg :z-list :initform '(5 10))
   (x1 :accessor x1 :initarg :x1 :initform nil)
   (x2 :accessor x2 :initarg :x2 :initform nil)
   (x :accessor x :initarg :x :initform nil)
   (c :accessor c :initarg :c :initform nil)
   (wall-buffer :accessor wall-buffer :initarg :wall-buffer :initform nil)
   (h :accessor h :initarg :h :initform 0.3) ;; используется и в других функциях
   (b :accessor b :initarg :b :initform 0.15)
   ))

(defclass wall ()
  ((x1 :accessor x1 :initarg :x1 :initform 0)
   (x2 :accessor x2 :initarg :x2 :initform 0)
   (c :accessor c :initarg :c :initform 0)))

(defun b-stena (&key l  kol-step) ; перед выполнением проги надо выполнить bok-stena!!!!!!!!!!!!!!!! наполни чем-то *wall*
;(b-stena :l 33  :kol-step 6)
  (let ((x 0) (m (+ (floor (/ l kol-step)) 1)) (min* nil) (max* nil) (column-number 1))
    ;здесь выполняется проверка на правильность входных данных длины стенки в осях и шага колонн
    (if (equal (nth-value 1 (floor (/ l kol-step))) 0.0) ()
      (print "warning: l/kol-step is not an integer value"))
;очищение глобальных переменных
    (setf *column* nil)
;заполнение *column* объектами колонн
    (dotimes (i m)  
      (progn 
        (push (make-instance  
               'kolonna 
               :x x :x1 (- x (/ kol-step 2)) 
               :x2 (+ x (/ kol-step 2))) *column*)
        (setf x (+ x kol-step))))
    (setf *column* (reverse *column*))
;исправление первой и последней колонны, участки выступающего фахверка за начало и конец осей здания (колонн)
    (setf (x1 (first *column*)) -0.3
          (x2 (first (last *column*))) (+ l 0.3))
;выборка в слот wall-buffer объекта колонн участков стен с их аэродин. к-ми
    (dolist (x *column*) 
      (dolist (y *wall*) (progn 
;условие что стена и колонна пересекаются
                           (setf min* (max (x1 x)(x1 y)) max* (min (x2 x)(x2 y)))
                           (if (equal (min min* max*) min*) 
                               (push (make-instance 'wall :x1 min* :x2 max* :c (c y)) (wall-buffer x))))))
;расчеты осредненного с по колоннам и вывод, расчет нагрузки
    (dolist (x *column*) (progn 
                           (format t "~a~a~%~%" "колонна №" column-number)
                           (print-veter x)
                           (incf column-number)))))

(defmethod print-veter ((obj kolonna))
  (let (l c l-list lc-list c*)
    (dolist (x (wall-buffer obj)) (progn 
				    (setf l (- (x2 x) (x1 x))
					  c (c x))
				    (push l l-list)
				    (push (* c l) lc-list)))
    (sum-list l l-list)
    (sum-list c lc-list)
    (setf l (ocrugl-1.00 l)
	  c (ocrugl-1.00 c))
    (setf c* (/ c l)
	  (c obj) c*) ;фигачим его в слот колонны
    (format t "~a~%" "усредненный аэродинамический к-т С")
    (format t "~a~%"  (ocrugl-1.000 c*))
    (format t "~a~%" "длина стены на колонну, м")
    (format t "~a~%" l)
    ;;(format t "~a~%" "ветровая нагрузка на высоту до 5м, тс/м")
    ;;(format t "~{~a~}" (list *veter* " *1.4* " (k5 obj) " * " (ocrugl-1.000 c*) " * " l "="))
    ;;(format t "~a~%" (ocrugl-1.00 (* *veter* 1.4 (k5 obj) c* l)))
	(format t "~a~%" "ветровая нагрузка на высоту до 7м, тс/м")
    (format t "~{~a~}" (list *veter* " *1.4* " (k7 obj) " * " (ocrugl-1.000 c*) " * " l "="))
    (format t "~a~%" (ocrugl-1.00 (* *veter* 1.4 (k7 obj) c* l)))
    ;;(format t "~a~%" "ветровая нагрузка на высоту до 10м, тс/м")
    ;;(format t "~{~a~}" (list *veter* " *1.4* " (k10 obj) " * " (ocrugl-1.00 c*) " * " l "="))
    ;;(format t "~a~%~%" (ocrugl-1.00 (* *veter* 1.4 (k10 obj) c* l)))
	))
  
(defun bok-top-veter (&key  b h l kol-step)  ; (bok-top-veter :b 108 :l 33 :h 10 :kol-step 6.6)
  ;; нагрузка на покрытие в этом модуле не считается
  (bok-stena :b b :d l :h h)
  (b-stena :l l  :kol-step kol-step))
  
(defun wind-elem (&key z type c b w) ;; (wind-elem :z 15.5 :type "a" :c 1.4 :b 0.3 :w 0.038)
  (let (w-value k)
    (setf k (veter-k type z)
	  w-value (ocrugl-1.000 (* w 1.4 k c b)))
    (format t "~{~a~}~%" (list "Нагрузка на элемент равна W = " w " * 1.4 *" k " * " c " * " b " = " w-value " тс/м"))
    (values w-value)
    ))

;;-------------------------------------------------------------------------------------------------------------
(defclass wind* ()
  (
   (w :accessor w :initarg :w :initform 0.038) ;; тс/м
   (type-m :accessor type-m :initarg :tipwind :initform "a")
   ))
(defclass column ()
  (
   (z-list :accessor z-list :initarg :z-list :initform nil)
   (colname :accessor colname :initarg :colname :initform "a")
   ;;()
   ))
(defclass frontal-column (column)
  (
   (L-left :accessor L-left :initarg :L-left)  ;; половина расстояния до другой колонны
   (L-right :accessor L-right :initarg :L-right) 
   (alfa-left :accessor alfa-left :initarg :alfa-left :initform 0)
   (alfa-right :accessor alfa-right :initarg :alfa-right :initform 0)
   ;;(name :accessor name :initarg :name :initform "a")
   ))

(defmethod fr-col-w-z ((obj frontal-column) (obj2 wind*) z cc)
  (declare (optimize (debug 3)))
  ;;(break)
  (let (w w2 k wn b1 b2 b3 b4 type name)
    (setf wn (w obj2) type (type-m obj2) name (colname obj))
    (setf b1 (*   (L-left obj)   (cos (/ (* (alfa-left obj) pi) 180)))    ;; в одном направлении
	  b2 (*   (L-right obj)   (cos (/ (* (alfa-right obj) pi) 180)))
	  b3 (*   (L-left obj)   (sin (/ (* (alfa-left obj) pi) 180)))    ;; в другом направлении
	  b4 (*   (L-right obj)   (sin (/ (* (alfa-right obj) pi) 180)))
	  k (veter-k type z)
	  w (+                       ;; плюсуем нагрузку слева и справа
	     (* wn 1.4 k cc b1)
	     (* wn 1.4 k cc b2)
	     )
	  w2 (+                       ;; плюсуем нагрузку слева и справа
	     (* wn 1.4 k cc b3)
	     (* wn 1.4 k cc b4)
	     )
	  )
    ;;(format t "~a~a~%" "Колонна " name)
    (format t "~{~a~}~%" (list "Ветровая нагрузка (продольная) на высоте z = " z " равна W=" (ocrugl-1.000 wn) "*" 1.4 "*" k "*"  cc "*" "("(ocrugl-1.000 b1) "+" (ocrugl-1.000 b2) ")=" (ocrugl-1.000 w) "тс/м"))
    (format t "~{~a~}~%" (list "Ветровая нагрузка (поперечная) на высоте z = " z " равна W=" (ocrugl-1.000 wn) "*" 1.4 "*" k "*"  cc "*" "("(ocrugl-1.000 b3) "+" (ocrugl-1.000 b4) ")=" (ocrugl-1.000 w2) "тс/м"))
    (values w w2)
  ))

(defmethod frontal-column-wind ((obj-f-col frontal-column) (obj-wind wind*) c)
  (let (z-list)
    (if (equal (z-list obj-f-col) nil) (setf z-list '(5 10 15)) (setf z-list (z-list obj-f-col))) ;; если z-list не задан - то мы задаем его по-умолчанию
    (dolist (x z-list) (fr-col-w-z obj-f-col obj-wind x c))  ;; итерируем лист и применяем к нему метод fr-col-w-z
    ))

(defun val-frontal-col (&key c L-left L-right alfa-left alfa-right) ;; (val-frontal-col :c 1.4 :L-left 2 :L-right 3 :alfa-left 15 :alfa-right 30)
  (declare (float c))
  (let (wind fr-col )
    (setq wind (make-instance 'wind* :w 0.038 :tipwind "a"))
    (setq fr-col (make-instance 'frontal-column :L-left L-left :L-right L-right :alfa-left alfa-left :alfa-right alfa-right :z-list '(0 11 13.3)))
    (frontal-column-wind fr-col wind c)
    ))

(defmethod val-frontal-col* ((wind-obj wind*)(col frontal-column) &key c)
  (let ()
    (frontal-column-wind col wind-obj c)
    ))
;; а теперь в массиве зададим характеристики колонн и применим к ним в цикле эти методы
(defvar *column-list*
  (list (list "a" 0 5.16 0 21.6 '(5 6 ))  ;; column-number L-left L-right alfa-left alfa-right
	(list "b" 5.16 (- 5.02) 21.6 22.22 '(5 6))
	(list "c" 5.02 3.44 22.22 22.22 '(5 6))
	(list "d" 3.44 2.26 22.22 22.22 '(5 6))
	(list "e" 2.26 3.09 22.22 22.22 '(4.5))
	(list "f" 3.09 0 22.22 22.22 '(4.5))))
(defun frontal-col-global (&key tipwind c w) ;; (frontal-col-global :tipwind "a" :c 0.8 :w 0.038)  ;; w -- нормативное
  ;; программа ограниченно работоспособная, т.к. не реализовано поведение когда ветер по У взаимоуничтожается
  (declare (optimize (debug 3)))
  ;;(break)
  (let (ww)
    (setq ww (make-instance 'wind* :tipwind tipwind :w w))
	;;(break)
    (dolist (x *column-list*)
      (progn
	(format t "~a~a~%" "Колонна \"" (first x) #\")
	(val-frontal-col* ww
			  (make-instance 'frontal-column :L-left (second x) :L-right (third x)
					 :alfa-left (fourth x) :alfa-right (fifth x) :colname (first x) :z-list (sixth x)) :c c)))))
