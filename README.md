# cl-wind-loads
Расчет ветровых нагрузок, мне стыдно за этот код, но он был написан давно, и он работает

Основные программы и команды:

* считаем нагрузку на боковые стены

    (bok-top-veter :b 108 :l 33 :h 10 :kol-step 6.6)

* считаем нагрузку на скатное покрытие (для всех примеров не забываем в СНиПе
  подсматривать основные параметры на рисунках)
  
    (scat-2 :alfa 0 :beta 14.5 :d 33 :b 33 :zh 10)

* простой расчет балочной эстакады

    (esta-balochnaya :rayon "a" :w (* 0.038 1.4) :z 11.4 :h-poyas 0.3 :h-prod-balka 0.2 	:h-poper-balka 0.3 :h-svaz 0.11 :L-svaz-1 3.45 :L-poper-balka 4.64 :L-prod-balka 	2.575 :shag-prod-balok 0.6 :n-prod-balok 6)

* затенение балки

	 (beem-shadowing 0.5 0.2)  
	 
	 (функц* шаг высота-балки)
* диагональный ветер на норию, 
  атлас-2003, табл. 3-9 стр 56 
  
	(noriya-diag :b-h 2.5 :alfa 45) 
	
	(noriya-diag :b-h (/ 553 305) :alfa 45)
	
	//Cn - поперек широкой часть ветер, Ct - поперек короткой стороны
	
* считаем ветровые коэффициенты пачкой

	(veter-k-list "a" (list 26.65 23.45 20.95 17.95 14.95 11.95 8.95 5.95 2.95))

* считаем плоскую опору

	(plosc-opora 
	
	   :z-list (list 2.53 5 7.4) 
	   
	   :b 2 :h-kolonna 0.25 :b-kolonna 0.175 :h-svayz 0.09)
	   
* расчет площадок башни

	(plate :z 2.5 :dx 4.5 :dy 4 :step-beam-x 0.8 :beem-length-x 10 
	
	   :h-beem-x 0.22 :step-beam-y 0.8 :beem-length-y 10 :h-beem-y 0.22)

* расчет нагрузок на каркас башни

	(tower-run :dx 6 :dy 6 :Ax-sector 6.36 :Ay-sector 6.76 
		:Akx (* 6 5) :Aky (* 6 5) :z-list '(5.46 10.46 15.46) )
