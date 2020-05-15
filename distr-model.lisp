;; ACT-R Model: Distraction Sub-system
;; distr-model.lisp
;; Adapted from Itabashi's model
;; Last updated: 20/02/2020
;; Run the ACT-R model to return an image chunk

;; 1. Declaring global variables
(defvar *window* nil)
(defvar *disp-hi* 480)
(defvar *disp-wi* 640)
(defvar *debg* nil)
(defvar *real-time* t)
(defvar *duration* 5)
(defvar *isi* 3)
(defvar *current-image*)
(defvar *image-struct*)
(defvar *directory*)
(defvar *assoc-list* nil)
(defvar *directory* "C:/Users/bi17030/Documents/Research/actr6/actr6/distr-model/")
(defvar *extension* "C:/Users/bi17030/Documents/Research/distraction/")
(defvar *time*)
(defconstant +posix-epoch+ (encode-universal-time 0 0 0 1 1 1970 0))
(defun posix-time-to-utime (time)
	(- time +posix-epoch+))
(defvar *ltime* (* (posix-time-to-utime (get-universal-time)) 1000))
(defvar *start-time* (* (posix-time-to-utime (encode-universal-time 0 0 0 1 1 2016 0)) 1000))
(defvar *born-time* (* (posix-time-to-utime (encode-universal-time 0 0 0 1 1 1977 0)) 1000))
(setf *time* *ltime*)
(clear-all)


;; 2. Running the model
;; 2.1 Starting the model
(defun run-next (coun)
  ;(output-latest-pic (format nil "~Aparam/next-pic.lisp" *directory*))
  (let ((chunks))
    (dotimes (i coun)
      (with-meta-process p1
      	(if (no-output (dm))
   	      (unless chunks
	            (setf chunks (no-output (dm)))))
         (set-chunk-slot-value g1-0 nimage nil)
         (set-chunk-slot-value g1-0 state start)
         (set-chunk-slot-value g1-0 phase nil)
         (set-chunk-slot-value g1-0 label nil)
		 (set-chunk-slot-value g1-0 current nil))
      (run-pic 'no))
  ;(clear-all) 
    ;(with-meta-process p1  (save-parameters chunks))
    )
)

(defun run-pic (num)
  (setf *assoc-list* nil)
  (unless (equal num 'no) (define-meta-process p1))
  (with-meta-process p1
    (unless (equal num 'no)
      (setf (meta-p-time (current-mp)) 
	    (+ (meta-p-time (current-mp)) *time*))
      (define-model-fct 'm1 *model-code*)
      (load (format nil "~Aparam/g-param.lisp" *directory*)) ;load global parameters
      (load-parameters)
      (add-word-characters #\+ #\/ #\% #\& #\-))

    (load (format nil "~Aparam/g-param.lisp" *directory*)) ;load global parameters
    (load (format nil "~Aparam/trigger.lisp" *directory*)) ;load global parameters
    (load (format nil "~Aparam/noise.lisp" *directory*)) ;load dynamic parameters
    (format t "~%~%------------~%ANS is set to [~A]~%" (car (no-output (sgp :ans))))
	(format t "~%RT is set to [~A]~%" (car (no-output (sgp :rt))))
    (format t "~%Bias strength of Label/Time is set to ~A~%------------~%" 
	    (list (car (car (no-output (spp RT-FROM-LABEL :u))))
		       (car (car (no-output (spp RT-FROM-TIME :u))))))
    (penable start-perceive perceive-label perceive-image)
    (pdisable rt-previous rt-following nimage-previous nimage-following)

    (if (equal num 1)
	(progn
	  (pdisable start-perceive perceive-label perceive-image)
	  (penable nimage1 nimage2))
	(progn
	    (setf *window*
		  (open-exp-window "Photo" 
				   :visible *debg* ;display appear or not
				   :width *disp-wi* 
				   :height *disp-hi*))
	  (install-device *window*)
	  (pdisable nimage1 nimage2)       	
	  (remove-all-items-from-rpm-window *window*)
	  (prepare-photo)    
	  (proc-display)))
    (let ((st (* (posix-time-to-utime (get-universal-time)) 1000))
	  (chunks (no-output (dm))))
      (run *duration* :real-time *real-time*)
      (if *window*
	  (unless (equal *isi* 0)
	    (remove-all-items-from-rpm-window *window*)
	    (proc-display)
	    (run *isi* :real-time *real-time*)))
#|
      (if (chunk-slot-value g1-0 nimage)
	  (if (equal (chunk-slot-value g1-0 nimage)
		     (chunk-slot-value g1-0 current))
	      (trigger-reward 0)
	      (trigger-reward 10))
	  (trigger-reward 0))
|#
      ;;output pic param
      (output-results (meta-p-time (current-mp)) st)
      ;;(sgp :mas)
      ;for simulation porpose don't save
      ;;(save-g-params (format nil "~Aparam/g-param.lisp" *directory*))
      ;;(save-productions (format nil "~Aparam/spp.lisp" *directory*))
	  (progn 
	    ;(save-parameters chunks)
	    (output-next-img (format nil "~Aparam/next-pic.lisp" *directory*))
		(output-result (format nil "~Aresult.txt" *extension*))
	    (format t (format nil "~A" (chunk-slot-value g1-0 nimage)))
	    (chunk-slot-value g1-0 nimage)))))

;; Preparing image data from idata
(defun prepare-photo ()
	(load (format nil "~Aparam/next-pic.lisp" *directory*))
	(load (format nil "~Aparam/idata/~A.lisp" *directory* *current-image*))  
	(dolist (i *image-struct*)
		(if (equal (nth 3 i) 'BLUE)
		(set-chunk-slot-value-fct 'g1 'current (car i)))
		(add-text-to-exp-window :text (format nil "~A" (car i))
								:x (nth 1 i) 
								:y (nth 2 i)
								:width (nth 3 i) 
								:height (nth 4 i)
								:color (nth 5 i))
	)
)

;; Resulting an output for next images
(defun output-next-img(file)
	(let ((nimage (chunk-slot-value g1-0 nimage)))
		(if nimage
			(let ((output-stream 
				(open file :direction :output :if-exists :supersede)))
				(format output-stream "(setf *current-image* ~S)" 
				(format nil "~A" nimage))	
				(close output-stream))
		)
	)
)

(defun output-next-img-ini(file)
	(let ((nimage (chunk-slot-value g1-0 nimage)))
		(if nimage
			(let ((output-stream 
				(open file :direction :output :if-exists :supersede)))
				(format output-stream "(setf *current-image* ~S)" 
				(format nil "~A" nimage))
				(close output-stream))
		)
	)
)

(defun output-result(file)
	(let ((nimage (chunk-slot-value g1-0 nimage)))
		(if nimage
			(let ((output-stream
				(open file :direction :output :if-exists :supersede)))
				(format output-stream "~A" nimage)
				(close output-stream))
		)
	)
)
			

;(defun output-latest-pic(file)
;	(let ((output-stream 
;		(open file :direction :output :if-exists :supersede)))
;		(format output-stream "(setf *current-image* 'NOD2O8JTQUECCYT6EBE72G)")
;		(close output-stream)
;	)
;)

;; Loading parameters
(defun load-parameters ()
	(load (format nil "~Aparam/g-param.lisp" *directory*)) ;load grobal parameters
	(load (format nil "~Aparam/img-dm.lisp" *directory*))  ;load declarative memory
	(load (format nil "~Aparam/img-param.lisp" *directory*)) ;load chunk parameters
	(extra-fan-sji)
	(load (format nil "~Aparam/spp.lisp" *directory*)) ;load production parameters
)

(defun save-parameters (chunks)
	(save-pic-params (format nil "~Aparam/img-param.lisp" *directory*) chunks)
	(save-productions (format nil "~Aparam/spp.lisp" *directory*))
)

;; Saving parameters
(defun output-results(model-time st)
	(let ((nimage (chunk-slot-value g1-0 nimage)))
		(if nimage
			(let ((output-stream 
				(open (format nil "~Alogs/results.txt" *directory*) :direction :output :if-exists :append)))
				(format output-stream "~A ~A ~A~%" (posix-time-to-utime (get-universal-time)) (car (no-output (sgp :ans))) nimage)
				(close output-stream))
		)
	)
	(setf stream (open (format nil "~Alogs/assoc-list.txt" *directory*)
		:direction :output :if-exists :append))
	(format stream "~A ~A ~A ~A " 
		model-time
		st
		(* (posix-time-to-utime (get-universal-time)) 1000) 
		(chunk-slot-value g1-0 nimage))
	(dolist (wd *assoc-list*) (format stream "~A " wd)) 
	(format stream "~%") 
	(close stream)
	(setf stream (open (format nil "~Alogs/time.txt" *directory*)
		:direction :output :if-exists :overwrite))
	(setf *time* (meta-p-time (current-mp))) ;comment out when simulation is runnning
	(format stream "~A" *time*)
	(close stream)
)

;;2.3 customized spreading activation
(defun extra-fan-sji ()
  (let ((phda (no-output (sdm label photo))))
    (dolist (ph phda) 
      (dolist (p (no-output (sdm-fct (list 'idata ph))))
	(aif (chunk-slot-value-fct p 'label)
	     (push (cons it 1) (chunk-fan-in ph)))   
	(aif (chunk-slot-value-fct p 'time)
	     (push (cons it 1) (chunk-fan-in ph)))))))

;; 3. Model Code
(defparameter *model-code* 
	'((sgp :esc t ;enable simbolic computation
		:show-focus t ;show focus
		;:ol t ;simplified base-level calcuration
		;:ol 5 ;simplified base-level calcuration
		:enable-inhibition t ;nil at jsai 2015
		:ul t ;enable utility learning
		:epl nil   ;enable production learning
		:pct nil ;enable production learning trace
		;:act t
		;;:nsji t  ;nil at jsai 2015
		:ol nil
		;:v nil
		:buffer-trace t
		:buffer-trace-step 1 
		:traced-buffers (production imaginal retrieval visual-location visual)
		;;:sji-hook compute-sji2  ;nil at jsai 2015
	)
	;; 3.1 Chunk definition
	;; 3.1.2 DM definition
    (chunk-type property object attribute value)
    (chunk-type order-data first second)
    ;; (chunk-type site-data idata site)
    (chunk-type time-data idata time)
    (chunk-type inc-label idata label)
    ;;(chunk-type photo-mood idata mood)
    (chunk-type label-data text)
    (chunk-type image-data text type)
	;; 3.1.2 Goal definition
    (chunk-type assoc current nimage label time state phase)
	;; 3.1.3 Create basic chunks
    (add-dm
     (image isa chunk)
     ;;(mood isa chunk)
     (searching isa chunk)
     (encoding-image isa chunk)
     (encoding-label isa chunk)
     (encoded-label isa chunk)
     ;;(remembering-place isa chunk)
     (retrieve isa chunk)
     (start isa chunk)
     (none isa chunk)
     (pending isa chunk)
     (g1 ISA assoc state start))

 ;; 3.2 Production Rules
(p nimage1
	=goal>
		ISA			assoc
		state 		start
==>
	=goal>
		state    	searching
	+retrieval>
		ISA			image-data
		type 		image
)

(p nimage2
	=goal>
		ISA			assoc
		;current 	=pd
	=retrieval>
		ISA			image-data
		text  		=pd1
==>
	=goal>
		nimage   	=pd1
)

(p start-perceive
	=goal>
		ISA         assoc
		state 		start
	  - phase 		retrieve
	?visual-location>
		state 		free
		;- state 	error
==>
	=goal>
		state		searching
	+visual-location>
		isa			visual-location
		:attended	nil
)

;for data that have only photoID
(p start-perceive-error
	=goal>
		ISA			assoc
		state 		searching
      - phase		retrieve
	?visual-location>
      state			error
==>
   =goal>
		state		searching
   +visual-location>
		isa			visual-location
		;:attended	t
)

(p perceive-image
	=goal>
		ISA         assoc
		state 		searching
	=visual-location>
		isa      	visual-location
		kind     	text
		color    	blue
	?visual>
		state    	free
==>
	=goal>
		state    	encoding-image
	+visual>
		ISA         move-attention
		screen-pos  =visual-location
)

(p perceive-label
	=goal>
		ISA         assoc
		state 		searching
	=visual-location>
		isa      	visual-location
		kind     	text
		color    	green
	?visual>
		state    	free
==>
	=goal>
		state    	encoding-label
	+visual>
		ISA			move-attention
		screen-pos	=visual-location
)

(p image-to-goal
	=goal>
		ISA         assoc
		state 		encoded-image
	=retrieval>
		ISA			image-data
==>
	=goal>
		state    	start
		current   	=retrieval
)

(p label-to-goal
	=goal>
		ISA         assoc
		state 		encoded-label
	=retrieval>
		ISA			label-data
==>
	=goal>
		state    	start
		label   	=retrieval
)

(p recognize-image
	=goal>
		ISA         assoc
		state 		encoding-image
	=visual>
		ISA         text
		value       =text
==>
	=goal>
		state    	encoded-image
	+retrieval>
		isa 		image-data
		text  		=text
)

(p recognize-label
	=goal>
		ISA         assoc
		state 		encoding-label
	=visual>
		ISA         text
		value       =text
==>
	=goal>
		state    	encoded-label
	+retrieval>
		isa 		label-data
		text  		=text
)

(p remember-time
	=goal>
		ISA         assoc
		state 		start
		time 		nil
		current 	=pd
==>
	=goal>
		state    	remembering-time
	+retrieval>
		isa 		time-data
		idata  		=pd
      - time 		nil
)

(p time-to-goal
	=goal>
		ISA         assoc
		state 		remembering-time
	=retrieval>
		ISA         time-data
		time       	=at
==>
	=goal>
		state    	start
		time   		=at
)


;; 3.2.2 Retrieval rules
(p rt-from-label
	=goal>
		ISA         assoc
		label    	=at
      - label  		nil
		current 	=pd
		state 		start
==>
	=goal>
		state    	retrieve
	+retrieval>
		ISA			inc-label
		label 		=at
      - idata 		=pd
		:recently-retrieved nil
)

;; rt-from-site

(p rt-from-time
	=goal>
		ISA         assoc
		current 	=pd
		state		start
		time 		=at
==>
	=goal>
		state    	retrieve
	+retrieval>
		isa 		time-data
		time 		=at
      - idata 		=pd
		:recently-retrieved nil
)

(p rt-previous
	=goal>
		ISA         assoc
		current 	=pd
		state	 	start
==>
	=goal>
		state    	retrieve
	+retrieval>
		isa 		order-data
		second 		=pd
)

(p rt-following
	=goal>
		ISA         assoc
		current 	=pd
		state 		start
==>
	=goal>
		state    	retrieve
	+retrieval>
		isa 		order-data
		first 		=pd
)

(p nimage-time
	=goal>
		ISA         assoc
		state 		retrieve
	=retrieval>
		isa 		time-data
		idata  		=pd1
		time    	=at
==>
	=goal>
		state    	start
		nimage  	=pd1      
		!eval! (push =at *assoc-list*)
)

(p nimage-previous
	=goal>
		ISA         assoc
		state 		retrieve
	=retrieval>
		isa 		order-data
		first 		=pd
		second  	=pd1
==>
   =goal>
		state    	start
		nimage   	=pd1      
		!eval! (push 'prev *assoc-list*)
)

(p nimage-following
	=goal>
		ISA         assoc
		state 		retrieve
	=retrieval>
		isa 		order-data
		first  		=pd1
		second 		=pd
==>
	=goal>
		state    	start
		nimage   	=pd1      
		!eval! (push 'follow *assoc-list*)
)

(p nimage-label
	=goal>
		ISA         assoc
		state 		retrieve
	=retrieval>
		ISA			inc-label
		idata  		=pd1
		label   	=at
==>
	=goal>
		state    	start
		nimage   	=pd1      
		!eval! (push =at *assoc-list*)
)

(P perceive-fail
	=goal>
		ISA         assoc
      - state 		wait
      - state 		start
      - state    	searching
    ?visual>
		state       error
==>
	=goal>
		state       wait
)

(P retrieve-fail
	=goal>
		ISA         assoc
		state       retrieve  
      - state searching
      - state encoding-photo
      - state encoding-scene
    ?retrieval>
		state       error
==>
	=goal>
		state       start
      
)

(p retrieve-same-photo
	=goal>
		ISA         assoc
		state 		retrieve
		current 	=pd
	=retrieval>
		ISA			inc-label
		idata  		=pd
==>
	=goal>
		state    	start
)


;;3.2 Rule parameters
(spp rt-from-label :u 5 :at 0.05)
(spp rt-from-time :u 5 :at 0.05)
;(spp retrieve-from-scene :u 0)
(goal-focus g1)
))