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

(define (spec-client-process master-pending-lookup client-id entry)
  (let loop ((entry entry))		;assume master-cache knows about this specialization
    (specialize-entry entry)
    (let inner-loop ()
      (if (local-cache-empty?)
	  'terminate
	  (let* ((entry (local-cache-advance!))
		 (token (master-pending-lookup (entry->static))))
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
  (local-proxy-set! *local-cache* '()))
(define (local-cache-enter! full-pp pp bts fct)
  (let* ((*memolist* (local-proxy-ref *local-cache*))
	 (fname (car full-pp))		;name of the function; a symbol
	 (found
	  (or (assoc pp *memolist*)
	      (let ((new-item (cons pp (make-entry full-pp pp (gensym fname) bts fct #f))))
		(local-proxy-set! *local-cache* (cons new-item *memolist*))
		new-item))))
    (cdr found)))
(define (local-cache-advance!)
  ; this is horrendously inefficient; the whole cache should be differently implemented
  (let loop ((*memolist* (local-proxy-ref *local-cache*)))
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
  (local-proxy-set! *local-resid* '()))
(define (make-residual-definition! name formals body)
  (let ((item `(DEFINE (,name ,@formals) ,body)))
    (local-proxy-set! *local-resid* (cons item (local-proxy-ref *local-resid*)))))

;;; export interface:
;;; spec-client-initialize!
;;; spec-client-process

;;; utilities
(define (encap proc)
  (let ((aspace (local-aspace)))
    (lambda args
      (apply remote-apply aspace proc args))))

(define (with-lock lock thunk)
  (obtain-lock lock)
  (let ((value (thunk)))
    (release-lock lock)
    value))

;;; the pending list
(define *master-pending* 'undefined)
(define (master-pending-initialize!)
  (set! *master-pending* (cons '***HEAD*** '())))
(define (master-pending-empty?)
  (null? (cdr *master-pending*))))
(define (master-pending-add! key entry)
  (set-cdr! *master-pending* (cons (cons key entry) (cdr *master-pending*))))
(define (master-pending-lookup! key)
  (let loop ((previous *master-pending*)
	     (current (cdr *master-pending*)))
    (if (equal? key (caar current))
	(begin
	  (set-cdr! previous (cdr current))
	  (cdar current))
	(loop current (cdr current)))))
(define (master-pending-advance!)
  (if (master-pending-empty?)
      #f
      (let ((entry (cdadr *master-pending*)))
	(set-cdr! *master-pending* (cddr *master-pending*))
	entry)))

;;; the cache
(define *master-cache* 'undefined)
(define (master-cache-initialize!)
  (set! *master-cache* '()))
(define (master-cache-add! key entry)
  (set! *master-cache* (cons (cons key entry) *master-cache*)))
(define (master-cache-lookup key)
  (cond ((assoc key *master-cache*) => cdr)
	(else #f)))
(define (spec-master spec-clients
		     level fname fct bts args)
  
  (let ((client-ids (map (lambda (aspace) (genint)) spec-clients))
	(master-cache-lock (make-lock))
	(master-pending-lock (make-lock))) ;obtain-lock release-lock
    ;; the following proc should only run on the master server
    (define master-cache-register
      (encap
       (lambda (program-point local-name aspace)
	 (let ((master-cache *master-cache*)
	       (static-skeleton (top-project-static program-point))
	       (master-cache-add-record
		(lambda ()
		  (let ((entry
			 (make-mc-entry program-point 			; key
					local-name			; global name
					(list (list local-name aspace))	; an alist 
					#f 				; processed?
					)))
		    (master-cache-add! static-skeleton entry)
		    (master-pending-add! static-skeleton entry)))))
	   (cond
	    ((master-cache-lookup static-skeleton)
	     => (lambda (entry)
		  (mc-entry->add-local-name entry local-name aspace)))
	    (else
	     (obtain-lock master-cache-lock) ;;;;;
	     (if (eq? master-cache *master-cache*)
		 (master-cache-add-record)
		 (let loop ((current-cache *master-cache*))
		   (if (eq? current-cache master-cache)
		       (master-cache-add-record)
		       (if (equal? (caar current-cache) static-skeleton)
			   'nothing-to-do
			   (loop (cdr current-cache))))))
	     (release-lock master-cache-lock)))))))

    ;;; body of spec-master
    (master-cache-initialize!)
    (master-pending-initialize!)
    
    ;; initialize specialization clients
    (for-each (lambda (aspace)
		(remote-run aspace spec-client-initialize!)) spec-clients)
    ;; all of them must have terminated before the first spec-client-process starts
    ;; maybe condvars help?

    (let* ((program-point (cons fname args))
	   (static-skeleton (top-project-static program-point))
	   (new-name (gensym fname)))
      (master-cache-register program-point new-name (car spec-clients)))

    (for-each (lambda (aspace client-id)
		(spawn
		 (lambda ()
		   ;; implements busy waiting: very bad
		   ;; again, what is the semantics of condvars?
		   (let loop ()
		     (let ((entry (with-lock master-pending-lock master-pending-advance!)))
		       (if entry
			   (remote-apply! aspace
					  spec-client-process master-pending-lookup! client-id
					  (make-entry))))
		     (loop)))))
	      spec-clients client-ids)))


