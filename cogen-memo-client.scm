;;; $Id $
;;; memoization client for distributed PE
;;;

;;; memo function stuff
(define-syntax start-memo
  (syntax-rules ()
    ((_ level fn bts args)
     (start-memo-internal level 'fn fn bts args))
    ((_ level fn bts args new-goal)
     (start-memo-internal level 'fn fn bts args))))

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
  (let* ((initial-scope (gensym-local-push!))
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
	  (list (append defn-template
			(cdr *residual-program*)
			defn-body)))
    result))

;;; the memo-function
;;; - fn is the name of the function to memoize
;;; - args are the free variables of the function's body
;;; - bts are their binding times
(define (multi-memo level fname fct bts args)
  (let*
      ((full-pp (cons fname args))
       (pp (top-project-static full-pp bts))
       (dynamics (top-project-dynamic full-pp bts))
       (actuals (apply append dynamics))
       (entry (local-cache-enter! full-pp pp bts fct))
       (res-name (entry->name entry)))
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; distributed stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (spec-client-initialize!)
  (local-cache-initialize!)
  (local-resid-initialize!))

(define (spec-client-process memo-space client-id entry)
  (let loop ((entry entry))		;assume memo-driver knows about this specialization
    (specialize-entry entry)
    (let inner-loop ()
      (if (local-cache-empty?)
	  'terminate
	  (let* ((entry (local-cache-advance!))
		 (token (remote-apply memo-space memo-driver (entry->static entry))))
	    (if token
		(loop entry)
		(inner-loop)))))))


(define (specialize-entry entry)
  (let*
      ((full-pp (entry->pp entry))
       (res-name (entry->name entry))
       (bts (entry->bts entry))
       (fct (entry->fct entry))

       (enter-scope (gensym-local-push!))
       (cloned-pp (top-clone-dynamic full-pp bts))
       (new-formals (apply append (top-project-dynamic cloned-pp bts))))
    (make-residual-definition! new-name
			       new-formals
			       (reset (apply fct (cdr cloned-pp))))
    (gensym-local-pop!)))

;;; cache entries:
;;; ->pp	- the full program point
;;; ->static	- the static skeleton of pp
;;; ->name	- the generated unique name
;;; ->bts	- the list of toplevel binding times
;;; ->fct	- the function to be applied
;;; ->mark	- local mark for advance
(define *local-cache* (make-proxy #f))		;I guess we need a proxy here
(define (local-cache-initialize!)
  (set-proxy-value *local-cache* '()))
(define (local-cache-enter! full-pp pp bts fct)
  (let* ((*memolist* (proxy-value *local-cache*))
	 (fname (car full-pp))		;name of the function; a symbol
	 (found
	  (or (assoc pp *memolist*)
	      (let ((new-item (cons pp (make-entry full-pp pp (gensym fname) bts fct #f))))
		(set-proxy-value *local-cache* (cons new-item *memolist*))
		new-item))))
    (cdr found)))
(define (local-cache-advance!)
  ; this is horrendously inefficient; the whole cache should be differently implemented
  (let loop ((*memolist* (proxy-value *local-cache*)))
    (if (null? *memolist*)
	'terminate-this-client
	(let* ((item (car *memolist*))
	       (entry (cdr item)))
	  (if (entry->mark entry)
	      (loop (cdr *memolist*))
	      (begin
		(entry->mark! entry #t)
		entry))))))

;;; residual code
(define *local-resid* (make-proxy #f))
(define (local-resid-initialize!)
  (set-proxy-value *local-resid* '()))
(define (make-residual-definition! name formals body)
  (let ((item `(DEFINE (,name ,@formals) ,body)))
    (set-proxy-value *local-resid* (cons item (proxy-value *local-resid*)))))

;;; import interface:
;;; (define (memo-driver static-skeleton) ...)
;;; 
;;; export interface:
;;; spec-client-initialize!
;;; spec-client-process
