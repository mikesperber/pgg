;;; $Id $
;;; memo function stuff: standard implementation

(define-record memolist-entry (name))

(define-syntax start-memo
  (syntax-rules ()
    ((_ level fn bts args)
     (start-memo-internal level 'fn fn bts args))
    ((_ level fn bts args new-goal)
     (start-memo-internal level 'fn fn bts args))))

(define (specialize memo-template args . new-goal)
  ;; memo-template = (goal bt ...)
  (let* ((bts (cdr memo-template))
	 (level (apply max (cons 1 bts)))
	 (goal-proc (car memo-template)))
    (apply start-memo-internal
	   level
	   goal-proc
	   (eval goal-proc (interaction-environment))
	   bts
	   args
	   new-goal)))

(define (nextlevel memo-template args . new-goal)
  (let ((level (list-ref memo-template 1))
	(goal-proc (list-ref memo-template 3))
	(bts (cadr (list-ref memo-template 4))))
    (apply start-memo-internal level
                               goal-proc
			       (eval goal-proc (interaction-environment))
			       bts
			       args
			       new-goal)))

(define (start-memo-internal level fname fct bts args . new-goal)
  (clear-residual-program!) 
  (clear-memolist!)
  (clear-support-code!)
  (gensym-local-reset!)
  (gensym-reset!)
  (creation-log-initialize!)
  (let* ((initial-scope (gensym-local-push!))
	 (initial-static-store (initialize-static-store!))
	 (result (reset (multi-memo level fname fct bts args)))
	 (result (if (and (pair? result) (eq? (car result) 'LET))
		     (car (cdaadr result))
		     result))
	 (drop-scope (gensym-local-pop!))
	 (goal-proc (car *residual-program*))
	 (defn-template (take 2 goal-proc))
	 ;; kludge alert
	 (defn-template
	   (if (null? new-goal)
	       defn-template
	       (list (car defn-template)
		     (cons (car new-goal) (cdadr defn-template)))))
	 (defn-body (list-tail goal-proc 2)))
    (set-residual-program!
     (if *generate-flat-program*
	 (cons (append defn-template defn-body)
	       (cdr *residual-program*))
	 (list (append defn-template
		       (cdr *residual-program*)
		       defn-body))))
    result))

;;; the memo-function
;;; - fn is the name of the function to memoize
;;; - args are the free variables of the function's body
;;; - bts are their binding times
(define (multi-memo level fname fct bts args)
  (let*
      ((enter-scope (gensym-local-push!))
       (full-pp (cons fname args))
       (pp (top-project-static full-pp bts))
       (dynamics (top-project-dynamic full-pp bts))
       ; (compressed-dynamics (map remove-duplicates dynamics))
       (actuals (apply append dynamics))
       (found
	(or (lookup-memolist pp)
	    (let*
		((new-name (gensym-trimmed fname))
		 ; (clone-map (map (lambda (arg)
		 ; 		   (cons arg (if (symbol? arg)
		 ; 				 (gensym-local arg)
		 ; 				 (gensym-local 'clone))))
		 ; 		 actuals))
		 (cloned-pp (top-clone-dynamic full-pp bts))
		 ; (new-formals (map cdr clone-map))
		 (new-formals (apply append (top-project-dynamic cloned-pp bts)))
		 (new-entry (add-to-memolist! pp (make-memolist-entry
						  new-name)))
		 (enter (creation-log-push!))
		 (new-def  (make-residual-definition! new-name
						      new-formals
						      (reset (apply fct (cdr cloned-pp)))))
		 (leave (creation-log-pop!)))
	      (make-memolist-entry new-name))))
       (res-name (memolist-entry->name found))
       (exit-scope (gensym-local-pop!)))
    (if (= level 1)
	;; generate call to fn with actual arguments
	(_complete-serious
	 (apply make-residual-call res-name actuals))
	;; reconstruct multi-memo
	(_complete-serious
	 (make-residual-call 'MULTI-MEMO
			     (- level 1)
			     `',res-name
			     res-name
			     `',(binding-times dynamics)
			     `(LIST ,@actuals))))))
