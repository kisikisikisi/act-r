;;; It will also write out the current :seed parameter in a comment so that it
;;; could be used to reproduce the same "future" after the save point as the 
;;; model which was saved i.e. if you run a model, save it, then run it some more
;;; uncommenting the seed in the saved version then loading that and running it
;;; will produce the same results as the you had with the run after the save.

;;; The appropriate declarative parameters among the following will be written out based 
;;; on the :mp, :bll, and :ol settings using sdp:

:creation-time
:reference-count
:reference-list
:similarities

;;; When writing out the creation-time and reference-list the times will be
;;; adjusted to a zero reference since the model time will be back to 0 when
;;; reloaded.  If one wants to avoid the re-referencing of those times then
;;; the optional parameter to save-chunks-and-productions needs to be specified
;;; as nil.

;;; For productions the :u and :at values will always be written out.  A non-nil 
;;; :reward will be saved if utility learning is enabled.  If production 
;;; compilation is enabled then an additional production parameter which is not
;;; part of spp will be set to indicate which productions were learned.  The
;;; reason for that last setting is because the utility learning differs for
;;; productions which were "original" vs learned and without the setting all
;;; the productions loaded would be marked as originals.




(defparameter *critical-params* '((:MD -1.0) (:RT 0.0) (:LE 1.0) (:MS 0.0) 
				  (:MP NIL) (:PAS NIL) (:MAS NIL) (:ANS NIL)
                                  (:BLC 0.0) (:LF 1.0) (:BLL NIL) 
				  (:ER NIL) (:OL T) (:IU 0) 
                                  (:ALPHA 0.2) (:UT NIL) (:NU 0) (:EGS 0.0) 
				  (:EPL NIL) (:TT 2.0) (:DAT 0.05) (:PPM NIL)))
  

(defun order-chunk-types (type-list)
  (let* ((root-types (remove-if 
		      (lambda (x) (> (length (chunk-type-supertypes-fct x)) 1)) type-list))
         (families (mapcar 'list root-types))
         (others (set-difference type-list root-types)))
    (dolist (x others)
      (nconc (find-if (lambda (y) (chunk-type-subtype-p-fct x (car y))) families) (list x)))
    (do ((res nil)
         (fams families (cdr fams)))
        ((null fams) (flatten res))
      (push (sort (car fams) #'< :key (lambda (x) 
					(length (chunk-type-supertypes-fct x)))) res))))

(defun save-g-params (g-param-name)    
  (sgp-fct (list :cmdt g-param-name))  
  (command-output "(sgp ")  
  (dolist (param *critical-params*)
    (unless (equalp (second param) (car (no-output (sgp-fct (list (first param))))))
      (command-output "~s ~s" (first param) 
		      (car (no-output (sgp-fct (list (first param))))))))
  ;(command-output " :seed ~s~%" (no-output (car (sgp :seed))))
  (command-output ")")
  (sgp-fct (list :cmdt t)))



(defun save-pic-params (pic-param-name chunks &optional (zero-ref nil))         
  (sgp-fct (list :cmdt pic-param-name))    
  (let ((esc (no-output (car (sgp :esc))))
	(mp (no-output (car (sgp :mp))))
	(bll (no-output (car (sgp :bll))))
	(ol (no-output (car (sgp :ol))))
	(params nil))
    
    (when (and esc (or mp bll))      
      (when mp (push :similarities params))
      (cond ((null bll)
               ;;; no extra params needed
	     )
	    ((null ol)
               ;;; need creation and list
	     (push :reference-list params)
	     (push :creation-time params))
	    ((numberp ol)
	     (push :reference-list params)
	     (push :reference-count params)
	     (push :creation-time params))
	    (t ;;; :ol is t
	     (push :reference-count params)
	     (push :creation-time params)))
      
      (dolist (c chunks)
	(command-output "(sdp ~a" c)
	(dolist (param params)
	  (let ((val (caar (no-output (sdp-fct (list c param))))))
	    (case param
	      (:similarities 
	       (command-output "  ~s (~{~s~})" param val))
                (:creation-time
                 (command-output "  ~s ~f" param (if zero-ref (- val (mp-time)) val)))
                (:reference-count
                 (command-output "  ~s ~d" param val))
                (:reference-list
                 (command-output "  ~s (~{~F~^ ~})" 
				 param (if zero-ref 
					   (mapcar (lambda (x) 
						     (- x (mp-time))) val) val))))))
	(command-output ")"))))
  (sgp-fct (list :cmdt t)))
    
    ;; write out the productions      
    

    
      ;;; Write out the production parameters
(defun save-productions (production-name)      
  (sgp-fct (list :cmdt production-name))
  (let ((productions (no-output (pp)))
	(params (no-output (spp :name :u :at :reward)))
	(ul (car (no-output (sgp :ul)))))
      
    ;;; always :u and :at then :reward if utility learning is enabled
    
    ;(pp)
    (dolist (x params)
      (command-output "(spp ~a :u ~f :at ~f)" (first x) (second x) (third x))
      (when (and ul (fourth x))
	(command-output "(spp ~a :reward ~s)" (first x) (fourth x))))
    
        ;;; If utility learning and production compilation are on save which
        ;;; productions were learned along the way
      
    (when (and ul (car (no-output (sgp :epl))))
      (dolist (x productions)
	(unless (production-user-created x)
	  (command-output "(setf (production-user-created '~a) nil)" x)))))
  (sgp-fct (list :cmdt t)))

#|
(defun output-pic-param (file)
  (let ((chunks (no-output (dm)))
	;(outp)
	(output-stream 
	  (open file
		:direction :output :if-exists :supersede)))
    (dotimes (i (length chunks))
#|
      (setf outp (format nil "~S~S" outp 
			 (format nil "(sdp ~S :creation-time ~S :reference-count ~S)~%" 
				 (nth i chunks)
				 (car (nth i (no-output (sdp :creation-time))))
				 (car (nth i (no-output (sdp :reference-count))))))))
    (format output-stream "~S" outp)
|#

      (format output-stream "(sdp ~S :creation-time ~S :reference-count ~S)~%" 
	      (nth i chunks)
	      ;(- (car (nth i (no-output (sdp :creation-time)))) (+ *isi* *duration*))
	      (car (nth i (no-output (sdp :creation-time))))
	      (car (nth i (no-output (sdp :reference-count))))))
   (close output-stream)))

(defun output-g-param (file)
  (let ((output-stream 
	  (open file
		:direction :output :if-exists :supersede)))
    (format output-stream 
	    "(sgp :lf ~S :alpha ~S :blc ~S :mas ~S :mp ~S :bll ~S :egs ~S)~%" 
	    (car (no-output (sgp :lf))) ;latency factor
	    (car (no-output (sgp :alpha))) ;learning rate for produtions
	    (car (no-output (sgp :blc))) ;base-level calculation
	    (car (no-output (sgp :mas))) ;maximam associative strength
	    (car (no-output (sgp :mp)))  ;mismatch penalty
	    (car (no-output (sgp :bll))) ;dacay parameter
	    (car (no-output (sgp :egs))))
   (close output-stream)))
|#
