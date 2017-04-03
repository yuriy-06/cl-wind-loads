(in-package :wind-loads)

(defvar w (* 0.048 1.4))
(defvar k* nil)
(defvar *string* (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character))

;;Программа делит 
;;(defmacro method-trace (name-method obj &rest arg)
;;  `(defun ,name-method (message list* ,'obj ,@arg)   ;; пример вызова (print-expand "расчет ветра W=" '(* w c 1.4))
;;     (let (m f-m m-out (i 0))
;;       (format t "~a" message) ;;выводим сообщение "расчет ветра W="
;;       (setf m (rest list*)  ;; отбираем аргументы функции
;;	     f-m (first list*))  ;; отбираем наименование операции
;;       (dolist (x m) (progn (push f-m m-out)(push x m-out))) ;; собираем массив вида (* 1,4 * с * w)
;;       (setf m-out (reverse m-out)  ;; берем rest ,  (rest (* 1,4 * с * w)) => (1,4 * с * w)
;;	     m-out (rest m-out)) ;;  (reverse (1,4 * с * w)) => (w * c * 1.4)
;;       (format t "~{~a~}" m-out)  ;; печатаем все
;;       (format t "~a" "=")
;;       (dolist (x m-out) 
;;	 (progn (incf i)
;;		(if (evenp i) (format t "~a" x) (format t "~a" (eval x)))))
;;       (format t "~a~a" "=" (eval list*))    
;;       (format t "~a" #\Newline)
;;       (values (eval list*))
;;       )))

;;(method-trace  wind-osnovn-p-trace '(obj1 wind))

(defclass wind ()
  ((w-poper :accessor w-poper :initarg :w-poper :initform 0)
   (w-prod :accessor w-prod  :initarg :w-prod :initform 0)
   (z :accessor z :initarg :z)
   (tip-m :accessor tip-m :initarg :tip-m)
   (k :accessor k :initarg :k)
   (w :accessor w
      :initarg :w)))

(defmethod wind-wind ((obj1 wind))
		(setf (k obj1) (veter-k (tip-m obj1)(z obj1))))

(defclass esta ()   ;клас ЭСТАКАДА
	((L-pan :accessor L-pan
      :initarg :L-pan
      :initform nil) ;;длина панели эстакады
	(b :accessor b
      :initarg :b
	  :initform nil);;ширина эстакады
	(osnovn-poyas :accessor osnovn-poyas
				:initarg :osnovn-poyas)
	(wind :accessor wind
		:initarg :wind)))
				
(defclass poyas ()
  ((h-sech-poyas :accessor h-sech-poyas
		 :initarg :h-sech-poyas
		 :initform nil)
   (type-g-svyaz :accessor type-g-svyaz
		 :initarg :type-g-svyaz
		 :initform "diag");;тип горизонтальных связей ("krest" "diag" "elka")
   (h-sech-svyaz :accessor h-sech-svyaz
		 :initarg :h-sech-svyaz
		 :initform 0)))
		
(defun rad-to-grad (rad)
  (values (* rad (/ 180 pi))))
	
(defclass nacl-truba()
  ((sin2-poper :accessor sin2-poper
	       :initarg :sin2-poper)
   (sin2-prod :accessor sin2-prod
	      :initarg :sin2-prod)
   (L-poper :accessor L-poper
	    :initarg :L-poper)
   (L-prod :accessor L-prod
	   :initarg :L-prod)))
	   

(defun goriz-svyaz (&key L-pan B tip-sv) ;;(goriz-svyaz :L-pan 2.2 :B 2 :tip-sv "diag") => (obj 'nacl-truba)
  (let (alfa L-poper L-prod tan* sin2-poper sin2-prod (r90 (/ pi 2))
	     ;;br
	     )
    ;;(break "1")
    (cond
      ((equal tip-sv "krest")(setf tan* (/ L-pan B)
				   alfa (atan tan*)
				   sin2-poper (expt (sin alfa) 2)
				   sin2-prod (expt (sin (- r90 alfa)) 2)
				   L-prod (* 2 L-pan)
				   L-poper (* 2 B)))
      ((equal tip-sv "diag")(setf 
			     tan* (/ L-pan B)
			     alfa (atan tan*)
			     sin2-poper (expt (sin alfa) 2)
			     sin2-prod (expt (sin (- r90 alfa)) 2)
			     L-prod  L-pan
			     L-poper B))
      ((equal tip-sv "elka")(setf 
			     tan* (/ L-pan (/ B 2))
			     alfa (atan tan*)
			     sin2-poper (expt (sin alfa) 2)
			     sin2-prod (expt (sin (- r90 alfa)) 2)
			     L-prod  L-pan
			     L-poper B)))
    (format *string* "~a~%" "Квадраты синусов для горизонтальных связей (для ветра поперек и вдоль эстакады)")
	(format *string* "~a~a~a~%" (ocrugl-1.000 sin2-poper) " " (ocrugl-1.000 sin2-prod))
	(format *string* "~a~%" "Длины связей (для ветра поперек и вдоль эстакады)")
	(format *string* "~a~a~a~%"  (ocrugl-1.000 L-poper) " " (ocrugl-1.000 L-prod))
    (values (make-instance 'nacl-truba :sin2-poper sin2-poper :sin2-prod sin2-prod :L-poper L-poper :L-prod L-prod)))) 
		
(defclass osnovn-p (poyas)
  ((h-sech-poper-balka :accessor h-sech-poper-balka
		       :initarg :h-sech-poper-balka 
		       :initform nil)
   (h-sech-prod-balka :accessor h-sech-prod-balka
		      :initarg :h-sech-prod-balka 
		      :initform nil)
   (number-prod-balok :accessor number-prod-balok
		      :initarg :number-prod-balok
		      :initform nil)
   (h-ekv-ogr :accessor h-ekv-ogr
	      :initarg :h-ekv-ogr
	      :initform 0.25)
   (c-prod-balok :accessor c-prod-balok
		  :initarg :c-prod-balok
		  :initform nil)
   ))
		
(defmethod wind-osnovn-p ((obj1 osnovn-p) &key k w L-pan B)  ;; (wind-osnovn-p obj1 :k k :w w :L-pan L-pan :B B) => (values w-poper w-prod)
  (let (w-poper-poyas k-zat-balok w-poper-balka w-poper-ogr w-prod-balka sv w-poper-sv w-prod-sv w-poper w-prod)
    (if (equal (c-prod-balok obj1) nil)
	(setf  k-zat-balok (zaten-balki (/ B (number-prod-balok obj1))  (h-sech-prod-balka obj1))
	       w-poper-balka (* w k 1.4 k-zat-balok (number-prod-balok obj1) (h-sech-prod-balka obj1) L-pan 0.5)) ;;0.5 - разделили на 2)
	(setf w-poper-balka (* w k (c-prod-balok obj1) (h-sech-prod-balka obj1) (number-prod-balok obj1) L-pan 0.5)
	      k-zat-balok 0))
    (setf w-poper-poyas (* w k 1.4 (h-sech-poyas obj1) L-pan)		
	  ;;на один узел пояса ;; балки продольные, а нагрузка -- поперечная
	  w-poper-ogr (* w k 1.4 (h-ekv-ogr obj1) L-pan)
	  w-prod-balka (* w k 1.4 B (h-sech-poper-balka obj1) 0.5)
	  sv (goriz-svyaz :L-pan L-pan :B B :tip-sv (type-g-svyaz obj1))
	  w-poper-sv (* w k 1.4 (h-sech-svyaz obj1) (sin2-poper sv) (L-poper sv) 0.5) ;;на два не забываем разделить
	  w-prod-sv (* w k 1.4 (h-sech-svyaz obj1) (sin2-prod sv) (L-prod sv) 0.5)
	  w-poper (+ w-poper-poyas w-poper-balka w-poper-ogr w-poper-sv)
	  w-prod (+ w-prod-balka w-prod-sv))
	  ;;(print-expand "нагрузка поперек пояса W=" '(* w k* 1.4 (h-sech-poyas obj1) L-pan))


    (format *string* "~a~%" "============================================")
    (format *string* "~a~%" "Нагрузка от основной части эстакады")
    (format *string* "~a~%" "============================================")
    (format *string* "~a~%" "Нагрузки поперек эстакады:")
    (format *string* "~a~%" "--------------------------------------------")
    (format *string* "~a~%" "Нагрузка поперек пояса")
    (format *string* "~a~%" (ocrugl-1.000 w-poper-poyas))	  
    (format *string* "~a~%" "Нагрузка поперек от продольных балок")
    (format *string* "~a~%" "Коэффициент затенения балок")
    (format *string* "~a~%" (ocrugl-1.000 k-zat-balok))
    (format *string* "~a~%" "Непосредственно от продольных балок")
    (format *string* "~a~%" (ocrugl-1.000 w-poper-balka))
    (format *string* "~a~%" "Поперечная на связь")
    (format *string* "~a~%" (ocrugl-1.000 w-poper-sv))   
    (format *string* "~a~%" "Нагрузка на ограждение")
    (format *string* "~a~%" (ocrugl-1.000 w-poper-ogr))
    (format *string* "~a~%" "--------------------------------------------")
    (format *string* "~a~%" "Нагрузки вдоль эстакады")
    (format *string* "~a~%" "--------------------------------------------")
    (format *string* "~a~%" "Нагрузка вдоль эстакады от поперечных балок")
    (format *string* "~a~%" (ocrugl-1.000 w-prod-balka))
    (format *string* "~a~%" "Продольная на связь")
    (format *string* "~a~%" (ocrugl-1.000 w-prod-sv))    
    (format *string* "~a~%" "--------------------------------------------")
    (format *string* "~a~%" "Суммарные нагрузки")
    (format *string* "~a~%" "--------------------------------------------")
    (format *string* "~a~%" "Сумма поперек от основной балочной части эстакады")
    (format *string* "~a~%" (ocrugl-1.000 w-poper))
    (format *string* "~a~%" "Сумма вдоль на основной балочной части эстакады")
    (format *string* "~a~%~%" (ocrugl-1.000 w-prod))
    (format *string* "~a~%" "============================================")
    (values w-poper w-prod)
    ))

(defclass neosn-p (poyas wind)
  ((h-sech-rasp :accessor h-sech-rasp
		:initarg :h-sech-rasp
		:initform :nil)))
		
(defmethod wind-neosn-p ((obj1 neosn-p) &key k w L-pan B)  ;;(wind-neosn-p obj1 :k k :w w :L-pan L-pan :B B) => (values w-poper w-prod)
  (let (w-poper-poyas w-prod-rasp sv  w-poper-sv w-prod-sv w-poper w-prod)
    
    (setf 
     w-poper-poyas (* w k 1.4 (h-sech-poyas obj1) L-pan)	
     w-prod-rasp (* w k 1.4 B (h-sech-rasp obj1) 0.5)
     sv (goriz-svyaz :L-pan L-pan :B B :tip-sv (type-g-svyaz obj1))
     w-poper-sv (* w k 1.4 (h-sech-svyaz obj1) (sin2-poper sv) (L-poper sv))
     w-prod-sv (* w k 1.4 (h-sech-svyaz obj1) (sin2-prod sv) (L-prod sv) )
     w-poper (+ w-poper-poyas w-poper-sv)
     w-prod (+ w-prod-rasp w-prod-sv))
    
    (format *string* "~a~%" "Нагрузка от неосновной части эстакады")
    (format *string* "~a~%" "Нагрузка поперек пояса")
    (format *string* "~a~%" (ocrugl-1.000 w-poper-poyas))
    (format *string* "~a~%" "Поперечная на связь")
    (format *string* "~a~%" (ocrugl-1.000 w-poper-sv))
    (format *string* "~a~%" "Продольная на связь")
    (format *string* "~a~%" (ocrugl-1.000 w-prod-sv))
    (format *string* "~a~%" "Продольная на распорку")
    (format *string* "~a~%" (ocrugl-1.000 w-prod-rasp))
    (format *string* "~a~%" "Сумма поперек от неосновной части эстакады")
    (format *string* "~a~%" (ocrugl-1.000 w-poper))
    (format *string* "~a~%" "Сумма вдоль на от неосновной части эстакады")
    (format *string* "~a~%~%" (ocrugl-1.000 w-prod))
    (format *string* "~a~%" "===========================================")
    (values w-poper w-prod)
    ))

(defclass conveer ()
  ((H :accessor H
      :initarg :H
      :initform nil)
   (B :accessor B
      :initarg :B
      :initform nil)
   (C :accessor C
      :initarg :C
      :initform nil)
   (H-nog :accessor H-nog
	  :initarg :H-nog
	  :initform 0)
   (h-sech-nog :accessor h-sech-nog
	       :initarg :h-sech-nog
	       :initform 0.05)
   (shag-nog :accessor shag-nog
	     :initarg :shag-nog
	     :initform 3)))
		
(defmethod wind-conveer ((obj1 conveer) &key k w L-pan)  ;; (wind-conveer obj1 :k k :w w :L-pan L-pan)
  (let (w-poper-konv w-prod-konv)
    (setf w-poper-konv (* w k (C obj1) (H obj1) L-pan)
	  w-prod-konv (* w k (C obj1) (h-sech-nog obj1) (H-nog obj1) 2 (/ L-pan (shag-nog obj1))))
			  
    (format *string* "~a~%" "Нагрузка от конвейера")
    (format *string* "~a~%" "Нагрузка поперек конвейера")
    (format *string* "~a~%" (ocrugl-1.000 w-poper-konv))
    (format *string* "~a~%" "Нагрузка вдоль конвейера")
    (format *string* "~a~%~%" (ocrugl-1.000 w-prod-konv))
    (format *string* "~a~%" "===========================================")
    (values w-poper-konv w-prod-konv)))
	  
	  
(defclass esta-ferma (esta)   ;клас ЭСТАКАДА ФЕРМЕННАЯ
  ((type-f :accessor type-f
	   :initarg :type-f
	   :initform "n") ;;может принимать "v", "n" (тело эстакады приподнято или опущено)
   (continuity :accessor continuity
	       :initarg :continuity ;;сплошность - t или nil (зашита или нет)
	       :initform nil)
   (osnovn-p :accessor osnovn-p
	     :initarg :osnovn-p)
   (neosn-p :accessor neosn-p
	    :initarg :neosn-p)
   (h-sech-rehetka :accessor h-sech-rehetka  ;;надо ли считать продольный ветер от решетки?!!!!!!!!!
		   :initarg :h-sech-rehetka
		   :initform nil)
   (h-sech-stoyka-poper :accessor h-sech-stoyka-poper
			:initarg :h-sech-stoyka-poper
			:initform nil)
   (h-sech-stoyka-prod :accessor h-sech-stoyka-prod
		       :initarg :h-sech-stoyka-prod
		       :initform nil)
   (h-sech-diafragm :accessor h-sech-diafragm
		    :initarg :h-sech-diafragm
		    :initform 0)
   (shag-diafragm :accessor shag-diafragm
		  :initarg :shag-diafragm
		  :initform 0)
   (type-formi-diafragm :accessor type-formi-diafragm
			:initarg :type-formi-diafragm
			:initform "diag") ;; может быть "krest" "diag" "elka"
   (H-ferm :accessor H-ferm
	   :initarg :H-ferm)
   ))

(defmethod diafr-l ((obj1 esta-ferma)) ;; возвращает длину диафрагм (diafr-l obj1) => L
  (let (L)
    (cond
      ((equal (type-formi-diafragm obj1) "krest")
       (setf L (* 2
		  (expt (+ (expt (H-ferm obj1) 2)
			   (expt (b obj1) 2)) 0.5))))
      ((equal (type-formi-diafragm obj1) "diag")
       (setf L (* 1
		  (expt
		   (+
		    (expt (H-ferm obj1) 2)
		    (expt (b obj1) 2)) 0.5))))
      ((equal (type-formi-diafragm obj1) "elka")
       (setf L (* 2
		  (expt
		   (+ (expt (H-ferm obj1) 2)
		      (expt (/ (b obj1) 2) 2)) 0.5)))))
      (format t "~a~%" "Общая длина стержней диафрагмы")
      (format t "~a~%" (ocrugl-1.000 L))
      (values L)))
	  
(defmethod wind-out-ferma ((obj1 esta-ferma) (obj2 conveer))
  (let (k* v1-p v1-pr v2-p v2-pr v3-p v3-pr A v4-p v-prod-o v-p-n v-prod-n v5-prod v-p-o)
    ;;(setf  (wind obj1) (wind-wind (wind obj1))) ;;возможно присваивание не надо делать, просто вызвать метод класса
    (setf k* (wind-wind (wind obj1))) ;; например так
    ;;здесь забираются данные из метода класса osnovn-p
    (multiple-value-setq (v1-p v1-pr) (wind-osnovn-p (osnovn-p obj1) :k k* :w w :L-pan (L-pan obj1) :B (B obj1)))
    ;;здесь забираются данные из метода класса neosn-p
    (multiple-value-setq (v2-p v2-pr)(wind-neosn-p (neosn-p obj1) :k k* :w w :L-pan (L-pan obj1) :B (B obj1)))
    ;;здесь забираются данные из метода класса conveer
    (multiple-value-setq (v3-p v3-pr)(wind-conveer obj2 :k k* :w w :L-pan (L-pan obj1)))
    ;;здесь считаем остаточный ветер от решетки фермы
    (setf A (+ (* (H-ferm obj1) (h-sech-stoyka-poper obj1)) (* (h-sech-rehetka obj1) (expt (+ (expt (L-pan obj1) 2) (expt (H-ferm obj1) 2)) 0.5)) (* 2 0.3 0.3))
	  v4-p (* w k* 1.4 A 0.5)) ;;  0.5 - разделили на 2 пояса
    ;; не забываем ветер на возможные диафрагмы посчитать + решетка
    (setf v5-prod (* w k* 1.4 (+ (* (diafr-l obj1) (h-sech-diafragm obj1))  (* 2 (H-FERM obj1) (H-sech-stoyka-prod obj1))) 0.25)) ;;0.25 - на 4 узла
    
    ;;а теперь вычислим сумму всего что у нас получилось
    (setf v-p-o (+ v1-p v4-p)
	  v-p-n (+ v2-p v4-p)
	  v-prod-o (+ v1-pr v5-prod)
	  v-prod-n (+ v2-pr v5-prod))
    ;;а теперь выведем все это нахер
    (format *string* "~a~%" "===========================================")
    (format *string* "~a~%" "Собираем нагрузки с фермы")
    (format *string* "~a~%" "-------------------------------------------")
    (format *string* "~a~%" "Часть от решетки поперек (вдоль не учитываем, учитываем стойки)")
    (format *string* "~a~%" (ocrugl-1.000 v4-p))
    (format *string* "~a~%" "от диафрагм вдоль + стойки")
    (format *string* "~a~%" (ocrugl-1.000 v5-prod))				
    (format *string* "~a~%" "--------------------------------")
    (format *string* "~a~%" "Коэффициент по высоте")
    (format *string* "~a~%" k*)
    (format *string* "~a~%" "--------------------------------")

    
    (format *string* "~a~%" "Конвейер")
    (format *string* "~a~%" "Нагрузка поперек")
    (format *string* "~a~%" (ocrugl-1.000 v3-p))
    (format *string* "~a~%" "Нагрузка вдоль")
    (format *string* "~a~%" (ocrugl-1.000 v3-pr))
    (format *string* "~a~%" "--------------------------------")
    (format *string* "~a~%" "Суммарные нагрузки (выборка с повтором)")				
    (format *string* "~a~%" "Нагрузка поперек")
    (format *string* "~a~%" "Основной пояс")
    (format *string* "~a~%" (ocrugl-1.000 v-p-o))
    (format *string* "~a~%" "Неосновной пояс")
    (format *string* "~a~%" (ocrugl-1.000 v-p-n))
    (format *string* "~a~%" "--------------------------------")
    (format *string* "~a~%" "Нагрузка вдоль")
    (format *string* "~a~%" "Основной пояс")
    (format *string* "~a~%" (ocrugl-1.000 v-prod-o))
    (format *string* "~a~%" "Неосновной пояс")
    (format *string* "~a~%" (ocrugl-1.000 v-prod-n))
    (format *string* "~a~%" "--------------------------------")
    ))
	  
(defun main-est (&key z tip-m  L-pan  b  h-sech-poper-balka  h-sech-prod-balka 
				number-prod-balok  h-ekv-ogr osnovn.h-sech-poyas 
				osn-p.h-sech-svyaz neosn-p.h-sech-svyaz 
				neosnovn-p.type-g-svyaz  osnovn-p.type-g-svyaz  
				c-prod-balok 
				h-sech-rasp  neosn.h-sech-poyas 
				h-sech-rehetka  h-sech-stoyka-poper   h-sech-stoyka-prod 
				h-sech-diafragm  
				H-FERM  H-konv  B-konv  H-nog-konv  h-sech-nog-konv 
				shag-nog-konv  
				C-konv )
;;(main-est :z :tip-m (gp "place-type*") :L-pan (gp "L-pan" ) :b (gp "B") :h-sech-poper-balka (gp "h-sech-poper-balka") :h-sech-prod-balka (gp "h-sech-prod-balka") 
;;				:number-prod-balok (gp "number-prod-balok") :h-ekv-ogr (gp "h-ekv-ogr") :osnovn.h-sech-poyas (gp "osnovn.h-sech-poyas") 
;;				:osn-p.h-sech-svyaz (gp  "osn-p.h-sech-svyaz" )
;;				:h-sech-svyaz (gp ) :neosnovn-p.type-g-svyaz (gp "neosnovn-p.type-g-svyaz" ) :osnovn-p.type-g-svyaz (gp "osnovn-p.type-g-svyaz" ) 
;;				:c-prod-balok (gp "c-prod-balok" )
;;				:h-sech-rasp (gp "h-sech-rasp") :neosn.h-sech-poyas (gp "neosnovn.h-sech-poyas") :neosn-p.h-sech-svyaz (gp "neosn-p.h-sech-svyaz") 
;;				:h-sech-rehetka (gp "h-sech-rehetka") :h-sech-stoyka-poper (gp "h-sech-stoyka-poper")  :h-sech-stoyka-prod (gp "h-sech-stoyka-prod" ) 
;;				:h-sech-diafragm (gp  "h-sech-diafragm") 
;;				:H-FERM (gp "H-FERM") :H-konv (gp "H.konv") :B-konv (gp "B.konv") :H-nog-konv (gp "H-nog") :h-sech-nog-konv (gp "h-sech-nog") 
;;				:shag-nog-konv (gp  "shag-nog") 
;;				:C-konv (gp "C.konv"))
  (let (w* konv)
    (setf w* (make-instance 'esta-ferma 
			    :wind 	(make-instance 'wind :z z :tip-m tip-m )
			    :type-f "n" :continuity nil :L-pan L-pan :b b
			    :osnovn-p (make-instance 'osnovn-p :h-sech-poper-balka h-sech-poper-balka :h-sech-prod-balka h-sech-prod-balka 
						     :number-prod-balok number-prod-balok :h-ekv-ogr h-ekv-ogr :h-sech-poyas osnovn.h-sech-poyas 
							 :h-sech-svyaz osn-p.h-sech-svyaz :type-g-svyaz osnovn-p.type-g-svyaz :c-prod-balok c-prod-balok)
			    :neosn-p (make-instance 'neosn-p :h-sech-rasp h-sech-rasp :h-sech-poyas neosn.h-sech-poyas :h-sech-svyaz neosn-p.h-sech-svyaz 
							:type-g-svyaz neosnovn-p.type-g-svyaz)
			    :h-sech-rehetka h-sech-rehetka
			    :h-sech-stoyka-poper h-sech-stoyka-poper
			    :h-sech-stoyka-prod h-sech-stoyka-prod
			    :h-sech-diafragm h-sech-diafragm
                            :H-FERM 2.2
			    ))
    (setf konv (make-instance 'conveer :H H-konv :B B-konv :H-nog H-nog-konv :h-sech-nog h-sech-nog-konv :shag-nog shag-nog-konv :C C-konv))
    (wind-out-ferma w* konv)
	(format t *string*)
	(setf *string* (make-array 5 :fill-pointer 0 :adjustable t :element-type 'character))
	))

(defun main-est-work ()  ;; (main-est-work)
  (let (w* konv)
    (setf w*
	  (make-instance
	   'esta-ferma 
	   :wind (make-instance 'wind :z 17.3 :tip-m "a")
	   :type-f "n" :continuity nil :L-pan 2.2 :b 2.25
	   :osnovn-p
	   (make-instance
	    'osnovn-p :h-sech-poper-balka 0.14 :h-sech-prod-balka 0.12 
	    :number-prod-balok 4 :h-ekv-ogr 0.25 :h-sech-poyas 0.1 :h-sech-svyaz 0.05
	    :type-g-svyaz "diag" :c-prod-balok 1.22)
	   :neosn-p (make-instance 'neosn-p :h-sech-rasp 0 :h-sech-poyas 0.075 :h-sech-svyaz 0)
	   :h-sech-rehetka 0.063
	   :h-sech-stoyka-poper 0.063
	   :h-sech-stoyka-prod (* 0.063 2)
	   :h-sech-diafragm 0
	   :H-FERM 2
	   ))
    (setf konv (make-instance 'conveer :H 0.86 :B 1.2 :H-nog 0.5 :h-sech-nog 0.06 :shag-nog 2.2 :C 1.7))
    (wind-out-ferma w* konv)))
	
(defun main-yahkino ()
  (let (w* konv)
    (setf w*
	  (make-instance
	   'esta-ferma 
	   :wind (make-instance 'wind :z 17.3 :tip-m "a")
	   :type-f "n" :continuity nil :L-pan 2.2 :b 2.25
	   :osnovn-p
	   (make-instance
	    'osnovn-p :h-sech-poper-balka 0.14 :h-sech-prod-balka 0.12 
	    :number-prod-balok 4 :h-ekv-ogr 0.25 :h-sech-poyas 0.1 :h-sech-svyaz 0.05
	    :type-g-svyaz "diag" :c-prod-balok 1.22)
	   :neosn-p (make-instance 'neosn-p :h-sech-rasp 0 :h-sech-poyas 0.075 :h-sech-svyaz 0)
	   :h-sech-rehetka 0.063
	   :h-sech-stoyka-poper 0.063
	   :h-sech-stoyka-prod (* 0.063 2)
	   :h-sech-diafragm 0
	   :H-FERM 2
	   ))
    (setf konv (make-instance 'conveer :H 0.86 :B 1.2 :H-nog 0.5 :h-sech-nog 0.06 :shag-nog 2.2 :C 1.7))
    (wind-out-ferma w* konv)))

(let (x)
  (defmacro two-value (x)
    `(values (list ',x ,x))))

(let (message list*)
  (defmacro print-expand (message list*)   ;; пример вызова (print-expand "расчет ветра W=" '(* w c 1.4))
    `(let (m f-m m-out (i 0))
       (format t "~a" message) ;;выводим сообщение "расчет ветра W="
       (setf m (rest list*)  ;; отбираем аргументы функции
	     f-m (first list*))  ;; отбираем наименование операции
       (dolist (x m) (progn (push f-m m-out)(push x m-out))) ;; собираем массив вида (* 1,4 * с * w)
       (setf m-out (reverse m-out)  ;; берем rest ,  (rest (* 1,4 * с * w)) => (1,4 * с * w)
	     m-out (rest m-out)) ;;  (reverse (1,4 * с * w)) => (w * c * 1.4)
       (format t "~{~a~}" m-out)  ;; печатаем все
       (format t "~a" "=")
       (dolist (x m-out) 
	 (progn (incf i)
		(if (evenp i) (format t "~a" x) (format t "~a" (eval x)))))
       (format t "~a~a" "=" ,list*)    
       (format t "~a" #\Newline))))
