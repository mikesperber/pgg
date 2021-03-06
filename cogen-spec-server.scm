;;; $Id $
;;; Specialization server for distributed PE
;;;
;;; A server performs actual specialization work, initiated by the
;;; memoization master.

(define-record server-entry
  (program-point static-skeleton name local-id bts fct killed?))

(define *local-cache* #f)
(define *local-pending* #f)
(define *local-preferred-procedure* #f)
(define *local-id-shift* 8)
(define *local-id-count* #f)

(define (generate-local-id)
  (let ((count *local-id-count*))
    (set! *local-id-count* (+ 1 *local-id-count*))
    (bitwise-ior (local-aspace-uid)
		 (arithmetic-shift count *local-id-shift*))))
(define (generate-local-symbol f)
  f ;; lose, lose!
  ;;(concatenate-symbol (gensym f) "-" (local-aspace-uid))
  )

(define *local-kill-count* #f)
(define *local-kill-count-lock* #f)
(define (get-local-kill-count)
  *local-kill-count*)

(define (local-cache-initialize!)
  (set! *local-cache* '()))

(define (local-pending-initialize!)
  (set! *local-pending* (make-queue)))

(define (local-cache-enter! pp static-skeleton bts fct)
  (cond ((or (assoc static-skeleton *local-cache*)
	     (queue-assoc static-skeleton *local-pending*))
	 => (lambda (p)
	      (let ((entry (cdr p)))
		(values (server-entry->name entry)
			(server-entry->local-id entry)
			#t))))
	(else
	 (let ((name (generate-local-symbol (car pp)))
	       (local-id (generate-local-id)))
	   (enqueue!
	    *local-pending*
	    (cons static-skeleton (make-server-entry
				   pp static-skeleton
				   name
				   local-id
				   bts fct
				   #f)))
	   (values name local-id #f)))))

(define (local-cache-insert! res-name pp static-skeleton bts fct)
  (let ((entry 
	 (make-server-entry pp static-skeleton res-name #f
			    bts fct
			    #f)))
    (set! *local-cache* (cons (cons static-skeleton entry) *local-cache*))
    entry))

(define (local-cache-advance!)
  (if (queue-empty? *local-pending*)
      #f
      (let ((item (or (dequeue-first!
		       (lambda (item)
			 (let ((entry (cdr item)))
			   (eq? (car (server-entry->program-point entry))
				*local-preferred-procedure*)))
		       *local-pending*)
		      (dequeue! *local-pending*))))
	(let ((entry (cdr item)))
	  (set! *local-cache* (cons item *local-cache*))
	  (if (server-entry->killed? entry)
	      (local-cache-advance!)
	      entry)))))

(define (local-pending-lookup local-id)
  (cond
   ((queue-any (lambda (item)
		 (let ((entry (cdr item)))
		   (eqv? local-id (server-entry->local-id entry))))
	       *local-pending*)
    => cdr)
   (else #f)))

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
  (gensym-reset!)
  (let* ((result
	  (with-fresh-gensym-local
	   (lambda ()
	     (reset (multi-memo level fname fct bts args)))))
	 (result (if (and (pair? result) (eq? (car result) 'LET))
		     (car (cdaadr result))
		     result))
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
	 (actuals (apply append dynamics)))
    (call-with-values
     (lambda () (local-cache-enter! full-pp pp bts fct))
     (lambda (res-name local-id already-registered?)
       ;; (display "Registering memo point ") (display local-id) (newline)
       (if (not already-registered?)
	   (let ((wrapped-pp
		  (wrap-similar-program-point
		   full-pp bts last-unwrapped-pp last-wrapped-pp)))
	     ;; ### how do we pass the async option?
	     (I-register-memo-point! #f wrapped-pp res-name local-id bts fct)))
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
				`(LIST ,@actuals))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; distributed stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This assumes all servers are running on different Kalis!

(define *server-master-aspace* #f)

(define (I-am-idle async?)
  ;; (display "I am idle") (newline)
  (remote-run! *server-master-aspace*
	       server-is-idle
	       (local-aspace-uid)
	       async?))

(define (I-register-memo-point! async? program-point name local-id bts fct)
  (remote-run! *server-master-aspace*
	       server-registers-memo-point!
	       (local-aspace-uid) async? program-point name local-id bts fct))

(define (can-I-work-on local-id)	; synchronous
  (call-with-values
   (lambda ()
     (remote-apply *server-master-aspace*
		   can-server-work-on (local-aspace-uid) local-id))
   (lambda (maybe-can-I? killed)
     (for-each server-kill-local-id! killed)
     maybe-can-I?)))

(define (I-am-working-on local-id)	; asynchronous
  (remote-run! *server-master-aspace*
	       server-working-on (local-aspace-uid) local-id))

(define (server-initialize! uid async?)
  ;; (display "Initializing, uid ") (display uid) (newline)
  (set! *server-master-aspace* (uid->aspace uid))
  (set! *local-id-count* 0)
  (set! *local-kill-count* 0)
  (set! *local-kill-count-lock* (make-lock))
  (local-cache-initialize!)
  (local-pending-initialize!)
  (local-resid-initialize!)
  (set! *local-preferred-procedure* #f)
  (set! *server-status-lock* (make-lock))
  (I-am-idle async?))

;;; Specialization work
;;; receives wrapped program points
(define (server-specialize res-name program-point bts fct)
  (let ((fname (car program-point))
	(static-skeleton (top-project-static (unwrap-program-point program-point) bts)))
    ;; assume master-cache knows about this specialization
    (let loop ((entry
		(local-cache-insert! res-name program-point static-skeleton bts fct)))
      (set! *local-preferred-procedure* (car static-skeleton))
      ;; (display "Specializing ") (display (server-entry->program-point entry)) (newline)
      (specialize-entry entry)
      (let inner-loop ()
	(let ((maybe-entry (local-cache-advance!)))
	  (if maybe-entry
	      (begin
		;; (display "Asking master if I can work on ") (display (server-entry->local-id maybe-entry)) (newline)
		(cond
		 ((can-I-work-on (server-entry->local-id maybe-entry))
		  => (lambda (maybe-local-id)
		       (if (number? maybe-local-id)
			   (let ((entry (local-pending-lookup maybe-local-id)))
			     (server-entry->killed?! entry #t)
			     (loop entry))
			   (loop maybe-entry))))
		 (else (inner-loop))))
	      (I-am-idle #f)))))))

(define last-wrapped-pp #f)
(define last-unwrapped-pp #f)
(define (specialize-entry entry)
  (let* ((wrapped-pp (server-entry->program-point entry))
	 (pp (unwrap-program-point wrapped-pp))
	 (res-name (server-entry->name entry))
	 (bts (server-entry->bts entry))
	 (fct (server-entry->fct entry)))
    (with-fresh-gensym-local
     (lambda ()
       (let* ((cloned-pp (top-clone-dynamic pp bts))
	      (new-formals (apply append (top-project-dynamic cloned-pp
							      bts))))
	 (set! last-wrapped-pp wrapped-pp)
	 (set! last-unwrapped-pp pp)
	 (make-residual-definition! res-name
				    new-formals
				    (reset (apply fct (cdr cloned-pp))))
	 (set! last-wrapped-pp #f)
	 (set! last-unwrapped-pp #f))))))

(define (server-kill-local-id! local-id)
  (let ((maybe-entry (local-pending-lookup local-id)))
    (if maybe-entry
	(begin
	  ;; (display "Master killed local id ") (display local-id) (newline)
	  (with-lock
	   *local-kill-count-lock*
	   (lambda ()
	     (set! *local-kill-count* (+ 1 *local-kill-count*))))
	  (server-entry->killed?! maybe-entry #t)))))

;; Async variant

(define *server-current-local-id* #f)
(define *server-current-thread* #f)
(define *server-status-lock* #f)

(define (server-specialize-async res-name program-point bts fct)
  (let* ((fname (car program-point))
	 (static-skeleton (top-project-static program-point bts))
	 (entry
	  (local-cache-insert! res-name program-point static-skeleton bts fct)))
    ;; (display "Specializing ") (display (server-entry->program-point entry)) (newline)
    (with-lock
     *server-status-lock*
     (lambda () (set! *server-current-local-id* #f)))
    (set! *local-preferred-procedure* (car static-skeleton))
    (specialize-entry entry)
    (server-async-loop)))

(define (server-async-loop)
  (obtain-lock *server-status-lock*)
  (let ((maybe-entry (local-cache-advance!)))
    (if maybe-entry
	(begin
	  (set! *server-current-local-id* (server-entry->local-id maybe-entry))
	  (set! *server-current-thread* (current-thread))
	  (I-am-working-on (server-entry->local-id maybe-entry))
	  (release-lock *server-status-lock*)
	  (set! *local-preferred-procedure* (car (server-entry->program-point maybe-entry)))
	  ;; (display "Specializing local id ") (display (server-entry->local-id maybe-entry)) (display (server-entry->program-point maybe-entry)) (newline)
	  (specialize-entry maybe-entry)
	  (server-async-loop))
	(begin
	  (set! *server-current-local-id* #f) 
	  (release-lock *server-status-lock*)
	  (I-am-idle #t)))))

(define (server-kill-specialization! local-id)
  ;; (display "Trying to kill local id ") (display local-id) (newline)
  (obtain-lock *server-status-lock*)
  (if (eqv? local-id *server-current-local-id*)
      (begin
	;; (display "Killing local id " local-id) (newline)
	(with-lock
	 *local-kill-count-lock*
	 (lambda ()
	   (set! *local-kill-count* (+ 1 *local-kill-count*))))
	(kill-thread! *server-current-thread*)
	(release-lock *server-status-lock*)
	;; (display "Killed local id " local-id) (newline)
	(server-async-loop))
      (release-lock *server-status-lock*)))
