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
  (let* ((full-pp (cons fname args))
	 (pp (top-project-static full-pp bts))
	 (dynamics (top-project-dynamic full-pp bts))
	 (actuals (apply append dynamics))
	 (entry (local-cache-enter! full-pp pp bts fct))
	 (res-name (entry->name entry)))
    (I-register-memo-point! full-pp res-name bts fct)
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

;; This assumes all clients are running on different Kalis!

(define client-server-aspace)

(define (I-am-unemployed)
  (remote-run! client-server-aspace
	       client-is-unemployed
	       (local-aspace)))

(define (I-register-memo-point! program-point local-name bts fct)
  (remote-run! client-server-aspace
	       client-registers-memo-point!
	       (local-aspace) program-point local-name bts fct))

(define (can-I-work-on? local-name)	;synchronous
  (remote-apply client-server-aspace
		can-client-work-on? (local-aspace) local-name))

(define (I-am-working-on local-name)	;asynchronous
  (remote-run! client-server-aspace
	       client-working-on (local-aspace) local-name))

(define (client-initialize! aspace)
  (set! client-server-aspace aspace)
  (local-cache-initialize!)
  (local-pending-initialize!)
  (local-resid-initialize!)
  (set! client-status-lock (make-lock))
  (I-am-unemployed))

;; messages sent from client to master
(define (client-working-on aspace local-name) ;async
  ; make sure that local-name is already registered with the master
  (if (can-client-work-on? aspace local-name)
      'nothing-to-do
      (remote-run! aspace spec-client-process-kill local-name)))

(define (can-client-work-on? aspace local-name)	;sync
  (let loop ((master-cache *master-cache*) (final? null?))
    (cond
     ((master-cache-lookup-by-local-name local-name final?)
      => (lambda (entry)
	   (master-entry->add-local-name! entry local-name aspace)
	   (with-lock master-cache-lock
		      (lambda ()
			(if (master-entry->mark entry)
			    #f
			    (begin
			      (master-entry->mark! entry #t)
			      #t))))))
     (else				;wait until local name registered
      (relinquish-timeslice)
      (loop *master-cache* (lambda (item) (eq? item master-cache)))))))

;; finfo-table is a mapping from function names to the binding times
;; of their arguments and the function itself, e.g. a list of lists:
;; (list 'fname '(0 1 0) fname)
;; !!! THIS STRUCTURE DOES NOT EXIST SO FAR !!!
;; [does it pay off?]
(define *finfo-table* (make-proxy #f))
(define (proxy-assoc key proxy-alist)
  (assoc key (proxy-local-ref proxy-alist)))
(define (finfo->bts finfo)
  (cadr finfo))
(define (finfo->fct finfo)
  (caddr finfo))

(define (spec-client-process master-entry)
  (let* ((res-name (master-entry->name master-entry))
	 (program-point (master-entry->program-point master-entry))
	 (fname (car program-point))
	 (finfo (proxy-assoc fname *finfo-table*))
	 (bts (finfo->bts finfo))
	 (fct (finfo->fct finfo))
	 (static-skeleton (top-project-static program-point bts)))
    ;;assume master-cache knows about this specialization
    (let loop ((entry
		(local-cache-insert! res-name program-point static-skeleton bts fct)))
      (specialize-entry entry)
      (let inner-loop ()
	(if (local-pending-empty?)
	    'terminate
	    (let* ((entry (local-cache-advance!))
		   (token (can-i-work-on? (entry->name entry))))
	      (if token
		  (loop entry)
		  (inner-loop))))))))

(define *client-status* #f)
(define client-status-lock)

(define (set-client-status! status)
  (with-lock client-status-lock
	     (lambda () (set! *client-status* status))))

(define (spec-client-process-async entry)
  (set-client-status! #f)
  (let loop ((entry entry))		;assume master-cache knows about this specialization
    (specialize-entry entry)
    (let inner-loop ()
      (set-client-status! #f)
      (if (local-pending-empty?)
	  'terminate
	  (let* ((entry (local-cache-advance!))
		 (local-name (entry->name entry)))
	    (set-client-status! (make-status local-name (current-thread) inner-loop))
	    (i-am-working-on local-name)
	    (specialize-entry entry)
	    (inner-loop))))))

(define (spec-client-process-kill local-name)
  (obtain-lock client-status-lock)
  (if *client-status*
      (if (eq? local-name (status->name *client-status*))
	  (begin
	    (kill-thread! (status->thread *client-status*))
	    (release-lock client-status-lock)
	    ((status->thunk *client-status*)))
	  (release-lock client-status-lock))
      (release-lock client-status-lock)))


(define (specialize-entry entry)
  (let*
      ((full-pp (entry->pp entry))
       (res-name (entry->name entry))
       (bts (entry->bts entry))
       (fct (entry->fct entry))

       (enter-scope (gensym-local-push!))
       (cloned-pp (top-clone-dynamic full-pp bts))
       (new-formals (apply append (top-project-dynamic cloned-pp bts))))
    (make-residual-definition! res-name
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
(define *local-cache*)		;no proxy necessary
(define *local-pending*)
(define (local-cache-initialize!)
  (set! *local-cache* '()))
(define (local-pending-initialize!)
  (set! *local-pending* '()))
(define (local-pending-empty?)
  (null? *local-pending*))
(define (local-cache-enter! full-pp pp bts fct)
  (let* ((fname (car full-pp))		;name of the function; a symbol
	 (found
	  (or (assoc pp *local-cache*)
	      (assoc pp *local-pending*)
	      (let ((new-item (cons pp (make-entry full-pp pp (gensym fname) bts fct #f))))
		(set! *local-pending*
		      (cons new-item *local-pending*))
		new-item))))
    (cdr found)))
(define (local-cache-insert! res-name full-pp pp bts fct)
  (let ((entry (make-entry full-pp pp res-name bts fct #t)))
    (set! *local-cache*
	(cons entry *local-cache*))
    entry))
(define (local-cache-advance!)
  (let ((pending *local-pending*))
  (if (null? pending)
      'terminate-this-client
      (let* ((item (car pending))
	     (entry (cdr item)))
	(set! *local-pending* (cdr pending))
	(set! *local-cache* (cons item *local-cache*))
	entry))))

;;; residual code
(define *local-resid* (make-proxy #f))
(define (local-resid-initialize!)
  (proxy-local-set! *local-resid* '()))
(define (make-residual-definition! name formals body)
  (let ((item `(DEFINE (,name ,@formals) ,body)))
    (proxy-local-set! *local-resid* (cons item (proxy-local-ref *local-resid*)))))

;;; export interface:
;;; spec-client-initialize!
;;; spec-client-process
;;; spec-client-process-async

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
(define *master-pending-placeholders*)
(define (master-pending-initialize!)
  (set! *master-pending* (cons '***HEAD*** '()))
  (set! *master-pending-placeholders* '()))
(define (master-pending-empty?)
  (null? (cdr *master-pending*)))
(define (master-pending-add! key entry)
  (if (null? *master-pending-placeholders*)
      (set-cdr! *master-pending*
		(cons (cons key entry) (cdr *master-pending*)))
      (begin				;locking?
	(let ((placeholder (car *master-pending-placeholders*)))
	  (set! *master-pending-placeholders*
		(cdr *master-pending-placeholders*))
	  (placeholder-set! placeholder entry)))))
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
(define (master-pending-sign-up! placeholder)
  (set! *master-pending-placeholders*
	(cons placeholder *master-pending-placeholders*))
  (length *master-pending-placeholders*))

;;; the cache
(define *master-cache* 'undefined)
(define (master-cache-initialize!)
  (set! *master-cache* '()))
(define (master-cache-add! key entry)
  (set! *master-cache* (cons (cons key entry) *master-cache*)))
(define (master-cache-lookup key)
  (cond ((assoc key *master-cache*) => cdr)
	(else #f)))
(define (master-cache-lookup-by-local-name local-name final?)
  (let loop ((master-cache *master-cache*))
    (and (not (final? master-cache))
	 (let* ((master-entry (cdar master-cache))
		(names+aspaces (master-entry->names+aspaces master-entry)))
	   (or (and (assoc local-name names+aspaces) master-entry)
	       (loop (cdr master-cache)))))))
(define master-cache-lock (make-lock))
(define master-pending-lock (make-lock))
(define master-placeholders-lock (make-lock)) ;necessary?

(define *unemployed-clients*)

(define (start-specialization client-aspaces
			      level fname fct bts args)
    (master-cache-initialize!)
    (master-pending-initialize!)
    (set! *unemployed-clients* (make-placeholder))
    
    (for-each (lambda (id aspace)
		(remote-run! aspace client-initialize! aspace))
	      client-aspaces)

    (let* ((program-point (cons fname args))
	   (static-skeleton (top-project-static program-point bts))
	   (new-name (gensym fname)))
      (client-registers-memo-point! (car client-aspaces) program-point new-name bts fct))

    ;; detect termination
    (let loop ((nr-unemployed (placeholder-value *unemployed-clients*))
	       (nr-of-clients (length client-aspaces)))
      (if (not (= nr-unemployed nr-of-clients))
	  (loop (placeholder-value *unemployed-clients*)
	  nr-of-clients)))

    ;; collect residual program
    (apply append
	   (map (lambda (aspace) (remote-apply aspace proxy-local-ref *local-resid*))
		client-aspaces)))

(define (client-registers-memo-point! client-aspace
				      program-point local-name
				      bts fct)
  (let ((static-skeleton (top-project-static program-point bts)))
    (cond
     ((master-cache-lookup static-skeleton)
      => (lambda (entry)
	   (master-entry->add-local-name! entry local-name client-aspace)))
     (else
      (let ((entry
	     (make-master-entry program-point 			; key
				local-name			; global name
				(list (list local-name client-aspace))	; an alist 
				#f 				; processed?
				)))
	(with-lock
	 master-cache-lock
	 (lambda () (master-cache-add! static-skeleton entry)))
	(with-lock
	 master-pending-lock
	 (lambda () (master-pending-add! static-skeleton entry))))))))

(define (client-is-unemployed client-aspace)
  (let ((entry (with-lock master-pending-lock
			  (lambda () (master-pending-advance!)))))
    (if entry
	(remote-run! client-aspace
		     spec-client-process
		     entry)
	(let* ((entry-placeholder (make-placeholder))
	       (unemployed-clients
		(with-lock master-pending-lock
			   (lambda () (master-pending-sign-up!
			   entry-placeholder)))))

	  (placeholder-set! *unemployed-clients* unemployed-clients)
	  (set! *unemployed-clients* (make-placeholder))

	  ;; wait for next task
	  (remote-run! client-aspace
		       spec-client-process
		       (placeholder-value entry-placeholder))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data structures

;;; entries in the local cache
(define-record entry
  (pp static name bts fct mark))

;;; entries in the global cache
(define-record master-entry
  (program-point name names+aspaces mark))

(define (master-entry->add-local-name! master-entry local-name aspace)
  (let ((names+aspaces (master-entry->names+aspaces master-entry)))
    (master-entry->names+aspaces! master-entry (cons (list local-name aspace) names+aspaces))))

;;; a status record
(define-record status
  (name thread thunk))
