(in-package :wind-loads)
(defmacro ocrugl-float (name-func number-float)
  `(defun ,name-func (x)
     (if (equal x nil) (values nil) (values (float (/ (round (* x ,number-float)) ,number-float))))))
(ocrugl-float ocrugl-1.00 100) ;создает функции округления (имя функции и еденицы после дроби)
(ocrugl-float ocrugl-1.000 1000)
(ocrugl-float ocrugl-1.00000 100000)

(defmacro sum-list (val list*)
  `(progn (setf ,val 0)
    (dolist (x ,list*) (setf ,val (+ x ,val)))))

	
;; из print-expand :analiz
(defun eval-list (l)  ;;   (eval-list '(w c))
  (let (m) 
    (dolist (x l) (push (eval x) m))
    (values (reverse m)))
  )

(defun print** (l) ;; (print** '(w c 1.4))
  (let (m)
    (dolist (x l) (setf m (append m (list x "*"))))
    (values (nbutlast m))))	

(defun print* (message list-param units) ;; (print* "hi!="  '(w c 1.4) "тс")
  (let (out-val)
    (setf out-val (apply #'* (eval-list list-param)))
    (format t "~a~%" message)
    (format t "~{~a~}" (append (print** list-param) (list "=")))
	(format t "~a" (print** (eval-list list-param)))
    (format t "~a~a~a~%" "=" (ocrugl-1.000 out-val) units)
    ))