;;; a memo function for an incremental cogen

(define (multi-memo level fn bts args)
  (let*
      ((full-pp (cons fn args))
       (pp (top-project-static full-pp bts))
       (dynamics (top-project-dynamic full-pp bts))
       (actuals (apply append dynamics))
       (found
	(or (assoc pp *memolist*)
	    (let*
		((new-name (gensym fn))
		 (cloned-pp (top-clone-dynamic full-pp bts))
		 (new-formals (apply append (top-project-dynamic cloned-pp bts)))
		 (new-entry (add-to-memolist! (cons pp new-name)))
		 (generate
		  (lambda actuals
		    (let* ((new-def `(DEFINE (,new-name ,@new-formals)
				       ,(reset (apply (eval fn (interaction-environment))
						      (cdr cloned-pp)))))
			   (dummy (add-to-residual-program! new-def))
			   (dummy (eval new-def (interaction-environment))))
		      (apply (eval new-name (interaction-environment)) actuals)))))
	      (set! *dummy-definition-buffer* generate)
	      (eval `(DEFINE ,new-name *DUMMY-DEFINITION-BUFFER*)
		    (interaction-environment))
	      (cons pp new-name))))
       (res-name (cdr found)))
    (if (= level 1)
	;; generate call to fn with actual arguments
	`(,res-name ,@actuals)
	;; reconstruct multi-memo
	`(MULTI-MEMO ,(- level 1)
		     ',res-name
		     ',(binding-times dynamics)
		     (LIST ,@actuals)))))

(define *dummy-definition-buffer*)