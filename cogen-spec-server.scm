;;; Specialization server for distributed PE
;;;
;;; A server performs actual specialization work, initiated by the
;;; memoization master.

(define-record server-entry
  (program-point static-skeleton name bts fct))

(define *local-cache*)
(define *local-pending*)
(define (local-cache-initialize!)
  (set! *local-cache* '()))
(define (local-pending-initialize!)
  (set! *local-pending* '()))
(define (local-pending-empty?)
  (null? *local-pending*))
(define (local-cache-enter! pp static-skeleton bts fct)
  (let ((fname (car pp)))
    (cond ((assoc pp *local-cache*) => cdr)
	  ((assoc pp *local-pending*) => cdr)
	  (else
	   (let ((entry (make-server-entry
			 pp static-skeleton (gensym fname) bts fct)))
		(set! *local-pending* (cons (cons pp entry) *local-pending*))
		entry)))))
(define (local-cache-insert! res-name pp static-skeleton bts fct)
  (let ((entry (make-server-entry pp static-skeleton res-name bts fct)))
    (set! *local-cache* (cons entry *local-cache*))
    entry))
(define (local-cache-advance!)
  (let ((pending *local-pending*))
    (let* ((item (car pending))
	   (entry (cdr item)))
      (set! *local-pending* (cdr pending))
      (set! *local-cache* (cons item *local-cache*))
      entry)))

;;; residual code
(define *local-resid*)
(define (local-resid-initialize!)
  (set! *local-resid* '()))
(define (make-residual-definition! name formals body)
  (let ((item `(DEFINE (,name ,@formals) ,body)))
    (set! *local-resid* (cons item *local-resid*))))
(define (collect-local-residual-program)
  *local-resid*)

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
	 (res-name (server-entry->name entry)))
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

;; This assumes all servers are running on different Kalis!

(define server-master-aspace)

(define (I-am-unemployed)
  (remote-run! server-master-aspace
	       server-is-unemployed
	       (local-aspace)))

(define (I-register-memo-point! program-point local-name bts fct)
  (remote-run! server-master-aspace
	       server-registers-memo-point!
	       (local-aspace) program-point local-name bts fct))

(define (can-I-work-on? local-name)	; synchronous
  (remote-apply server-master-aspace
		can-server-work-on? (local-aspace) local-name))

(define (I-am-working-on local-name)	; asynchronous
  (remote-run! server-master-aspace
	       server-working-on (local-aspace) local-name))

(define (server-initialize! aspace)
  (display "Initializing, uid ") (display (aspace-uid (local-aspace))) (newline)
  (set! server-master-aspace aspace)
  (local-cache-initialize!)
  (local-pending-initialize!)
  (local-resid-initialize!)
  (set! server-status-lock (make-lock))
  (I-am-unemployed))

;;; Specialization work

(define (server-specialize res-name program-point bts fct)
  (display "Specializing ") (display program point) (newline)
  (let ((fname (car program-point))
	(static-skeleton (top-project-static program-point bts)))
    ;; assume master-cache knows about this specialization
    (let loop ((entry
		(local-cache-insert! res-name program-point static-skeleton bts fct)))
      (specialize-entry entry)
      (let inner-loop ()
	(if (local-pending-empty?)
	    (I-am-unemployed)
	    (let ((entry (local-cache-advance!)))
	      (if (can-I-work-on? (server-entry->name entry))
		  (loop entry)
		  (inner-loop))))))))

(define (specialize-entry entry)
  (let* ((pp (server-entry->program-point entry))
	 (res-name (server-entry->name entry))
	 (bts (server-entry->bts entry))
	 (fct (server-entry->fct entry))
	 (enter-scope (gensym-local-push!))
	 (cloned-pp (top-clone-dynamic pp bts))
	 (new-formals (apply append (top-project-dynamic cloned-pp bts))))
    (make-residual-definition! res-name
			       new-formals
			       (reset (apply fct (cdr cloned-pp))))
    (gensym-local-pop!)))

;; Async variant


(define-record status
  (name thread thunk))

(define *server-status* #f)
(define server-status-lock)

(define (set-server-status! status)
  (with-lock server-status-lock
	     (lambda () (set! *server-status* status))))

(define (server-specialize-async entry)
  (set-server-status! #f)
  (let loop ((entry entry))		; assume master-cache knows about this specialization
    (specialize-entry entry)
    (let inner-loop ()
      (set-server-status! #f)
      (if (local-pending-empty?)
	  'terminate
	  (let* ((entry (local-cache-advance!))
		 (local-name (server-entry->name entry)))
	    (set-server-status! (make-status local-name (current-thread) inner-loop))
	    (I-am-working-on local-name)
	    (specialize-entry entry)
	    (inner-loop))))))

(define (server-kill-specialization local-name)
  (obtain-lock server-status-lock)
  (if *server-status*
      (if (eq? local-name (status->name *server-status*))
	  (begin
	    (kill-thread! (status->thread *server-status*))
	    (release-lock server-status-lock)
	    ((status->thunk *server-status*)))
	  (release-lock server-status-lock))
      (release-lock server-status-lock)))


