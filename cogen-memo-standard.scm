;;; cogen-memo-standard.scm

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; memo function stuff: standard implementation

(define-record memolist-entry
  (name) (count 0) (fct #f) (var #f) (serial #f) (bts #f))

(define-syntax start-memo
  (syntax-rules ()
    ((_ level fn bts args)
     (start-memo-internal level 'fn fn bts args))
    ((_ level fn bts args new-goal)
     (start-memo-internal level 'fn fn bts args))))

(define (specialize goal-proc memo-template args . new-goal)
  ;; memo-template = (goal-proc-name bt ...)
  (let* ((bts (cdr memo-template))
	 (level (apply max (cons 1 bts))))
    (apply start-memo-internal
	   level
	   (car memo-template)
	   goal-proc
	   bts
	   args
	   new-goal)))

(define (nextlevel goal-proc memo-template args . new-goal)
  (let ((level (list-ref memo-template 1))
	(goal-proc-name (list-ref memo-template 3))
	(bts (cadr (list-ref memo-template 4))))
    (apply start-memo-internal level
                               goal-proc-name
			       goal-proc
			       bts
			       args
			       new-goal)))

(define (start-memo-internal level fname fct bts args . new-goal)
  (clear-residual-program!) 
  (clear-memolist!)
  (clear-support-code!)
  (clear-deferred-list!)
  (gensym-local-reset!)
  (gensym-reset!)
  (creation-log-initialize!)
  (let* ((initial-scope (gensym-local-push!))
	 (initial-static-store (initialize-static-store!))
	 (result (reset (multi-memo level fname fct #f bts args)))
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

(define (continue var value)
  (let ((deferred (get-deferred-list)))
    (clear-deferred-list!)
    ;; what else must be cleared?
    (for-each
     (lambda (kv)
       (let ((key (car kv))
	     (entry (cdr kv)))
	 (if (equal? (memolist-entry->var entry) var)
	     ((memolist-entry->fct entry) value)
	     (add-to-deferred-list! key entry))))
     deferred)))

(define (resurrect var value file-contents)
  (let loop ((file-contents file-contents))
    (if (pair? file-contents)
	(begin
	  (let* ((new-name (list-ref file-contents 0))
		 (waiting-for (list-ref file-contents 1))
		 (frozen-pp (list-ref file-contents 2))
		 (bts (list-ref file-contents 3)))

	    (if (equal? var waiting-for)
		(let
		    ;; might have to move this to module cogen-library
		    ((fresh-pp (eval frozen-pp (interaction-environment)))
		     ;; ... or into the genext!
		     (fct (eval (car frozen-pp) (interaction-environment))))
		  (gensym-local-push!)
		  (set-car! (cdr fresh-pp) value)
		  (let* ((cloned-pp (top-clone-dynamic fresh-pp bts))
			 (new-formals (map car (top-project-dynamic cloned-pp bts))))
		    (creation-log-push!)
		    (make-residual-definition! new-name
					       new-formals
					       (reset (apply fct (cdr cloned-pp))))
		    (creation-log-pop!)
		    (gensym-local-pop!)))
		;; waiting for someone else
		(let ((new-entry (make-memolist-entry new-name)))
		  (memolist-entry->var! new-entry var)
		  (memolist-entry->serial! new-entry frozen-pp)
		  (memolist-entry->bts! new-entry bts)
		  (add-to-deferred-list! pp new-entry)))

	    (loop (list-tail file-contents 4)))))))

;;; the memo-function
;;; - fn is the name of the function to memoize
;;; - args are the free variables of the function's body
;;; - bts are their binding times
(define (multi-memo level fname fct maybe-var bts args)
  (multi-memo-internal level fname fct maybe-var bts args #t))

(define (multi-memo-no-result level fname fct maybe-var bts args)
  (multi-memo-internal level fname fct maybe-var bts args #f))

(define (multi-memo-internal level fname fct maybe-var bts args result-needed)
  (let*
      ((special (and maybe-var (zero? (car bts))))
       (%garbage% (if special (set-car! args maybe-var)))
       (full-pp (cons fname args))
       (pp (top-project-static full-pp bts))
       (dynamics (top-project-dynamic full-pp bts))
       (actuals (map car dynamics))
       (dyn-bts (map cdr dynamics))
       (specialize
	(lambda (new-name)
	  (gensym-local-push!)
	  (creation-log-push!)
	  (let* ((cloned-pp (top-clone-dynamic full-pp bts))
		 (new-formals (map car (top-project-dynamic cloned-pp bts))))
	    (make-residual-definition! new-name
				       new-formals
				       (reset (apply fct (cdr cloned-pp))))
	    (creation-log-pop!)
	    (gensym-local-pop!))))
       (found
	(if special
	    (or (lookup-deferred-list pp)
		(let*
		    ((new-name (gensym-trimmed fname))
		     (new-entry (make-memolist-entry new-name)))
		  (memolist-entry->var! new-entry maybe-var)
		  (memolist-entry->fct! new-entry (lambda (val)
						    (set-car! args val)
						    (specialize new-name)))
		  (memolist-entry->serial! new-entry (serialize full-pp bts))
		  (memolist-entry->bts! new-entry bts)
		  (add-to-deferred-list! pp new-entry)
		  new-entry))
	    (or (lookup-memolist pp)
		(let*
		    ((new-name (gensym-trimmed fname))
		     (new-entry (make-memolist-entry new-name))
		     (register-entry (add-to-memolist! pp new-entry)))
		  (specialize new-name)
		  new-entry))))
       (seen (memolist-entry->count! found (+ 1 (memolist-entry->count
						 found))))
       (res-name (memolist-entry->name found)))
    (if (= level 1)
	;; generate call to fn with actual arguments
	(if result-needed
	    (_complete-serious
	     (apply make-residual-call res-name actuals))
	    (_complete-serious-no-result
	     (apply make-residual-call res-name actuals)))
	;; reconstruct multi-memo
	(if result-needed
	    (_complete-serious
	     (make-residual-call 'MULTI-MEMO
				 (- level 1)
				 `',res-name
				 res-name
				 maybe-var
				 `',(map pred dyn-bts)
				 `(LIST ,@actuals)))
	
	    (_complete-serious-no-result
	     (make-residual-call 'MULTI-MEMO
				 (- level 1)
				 `',res-name
				 res-name
				 maybe-var
				 `',(map pred dyn-bts)
				 `(LIST ,@actuals)))))))
