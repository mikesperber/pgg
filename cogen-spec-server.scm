;;; Specialization server for distributed PE
;;;
;;; A server performs actual specialization work, initiated by the
;;; memoization master.

(define-record server-entry
  (program-point static-skeleton name local-id bts fct))

(define *local-cache* #f)
(define *local-pending* #f)
(define *local-id-shift* 8)
(define *local-id-count* #f)

(define (generate-local-id)
  (let ((count *local-id-count*))
    (set! *local-id-count* (+ 1 *local-id-count*))
    (bitwise-ior (local-aspace-uid)
		 (arithmetic-shift count *local-id-shift*))))
(define (generate-local-symbol f)
  (concatenate-symbol (gensym f) "-" (local-aspace-uid)))

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
			 pp static-skeleton
			 (generate-local-symbol fname)
			 (generate-local-id)
			 bts fct)))
		(set! *local-pending* (cons (cons pp entry) *local-pending*))
		entry)))))
(define (local-cache-insert! res-name pp static-skeleton bts fct)
  (let ((entry (make-server-entry pp static-skeleton res-name #f bts fct)))
    (set! *local-cache* (cons (cons pp entry) *local-cache*))
    entry))
(define (local-cache-advance!)
  (let ((pending *local-pending*))
    (let* ((item (car pending))
	   (entry (cdr item)))
      (set! *local-pending* (cdr pending))
      (set! *local-cache* (cons item *local-cache*))
      entry)))

;;; residual code
(define *local-resid* #f)
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
	 (res-name (server-entry->name entry))
	 (local-id (server-entry->local-id entry)))
    
    (I-register-memo-point! full-pp res-name local-id bts fct)
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

(define *server-master-aspace* #f)

(define (I-am-unemployed)
  (remote-run! *server-master-aspace*
	       server-is-unemployed
	       (local-aspace-uid)))

(define (I-register-memo-point! program-point name local-id bts fct)
  (remote-run! *server-master-aspace*
	       server-registers-memo-point!
	       (local-aspace-uid) program-point name local-id bts fct))

(define (can-I-work-on? local-id)	; synchronous
  (remote-apply *server-master-aspace*
		can-server-work-on? (local-aspace-uid) local-id))

(define (I-am-working-on local-id)	; asynchronous
  (remote-run! *server-master-aspace*
	       server-working-on (local-aspace-uid) local-id))

(define (server-initialize! uid)
  (display "Initializing, uid ") (display uid) (newline)
  (set! *server-master-aspace* (uid->aspace uid))
  (set! *local-id-count* 0)
  (local-cache-initialize!)
  (local-pending-initialize!)
  (local-resid-initialize!)
  (set! server-status-lock (make-lock))
  (I-am-unemployed))

;;; Specialization work

(define (server-specialize res-name program-point bts fct)
  (let ((fname (car program-point))
	(static-skeleton (top-project-static program-point bts)))
    ;; assume master-cache knows about this specialization
    (let loop ((entry
		(local-cache-insert! res-name program-point static-skeleton bts fct)))
      (display "Specializing ") (display (server-entry->program-point entry)) (newline)
      (specialize-entry entry)
      (let inner-loop ()
	(if (local-pending-empty?)
	    (I-am-unemployed)
	    (let ((entry (local-cache-advance!)))
	      (if (can-I-work-on? (server-entry->local-id entry))
		  (begin
		    (display "Master says I can work on ") (display (server-entry->local-id entry)) (newline)
		    
		    (loop entry))
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
(define server-status-lock #f)

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
