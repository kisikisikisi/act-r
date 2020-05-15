;; ACT-R Model: Distraction Sub-system
;; starter.lisp
;; Last updated: 20/02/2020
;; Start the ACT-R model and Lisp server

(load "C:/Users/bi17030/Documents/Research/actr6/actr6/load-act-r-6.lisp")
(defvar *directory* "C:/Users/bi17030/Documents/Research/actr6/actr6/distr-model/")
(setf stream (open (format nil "~Alogs/results.txt" *directory*)
		:direction :output :if-exists :append))
(format stream "time noise chunk~%")
(close stream)
;(close stream)
;(load "C:/Users/bi17030/Documents/Research/actr6/actr6/distr-model/pic-semantic.lisp")
(load "C:/Users/bi17030/Documents/Research/actr6/actr6/distr-model/distr-model.lisp")
;(ql:quickload :usocket)
;(load "C:/Users/bi17030/Documents/Research/actr6/actr6/distr-model/usocket/command-server.lisp")
(run-pic 1)
;(server-start)
;(run 10)
;(server-start)
;(let ((user (second sb-ext:*posix-argv*))	
;      (port (parse-integer (third sb-ext:*posix-argv*))))
;  (load-user user)


;(run-pic 1)
;(quit)