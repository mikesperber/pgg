;;; cogen-memo-standard.scm

;;; copyright © 1996, 1997, 1998, 1999, 2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; memo function stuff: standard implementation

(define-record memolist-entry
  (name) (value #f) (bt #f) (count 0) (fct #f) (var #f) (pp #f) (bts #f))

(define-syntax start-memo
  (syntax-rules ()
    ((_ level fn bts args)
     (begin (prepare!)
	    (start-memo-internal level 'fn fn bts args)))
    ((_ level fn bts args new-goal)
     (begin (prepare!)
	    (start-memo-internal level 'fn fn bts args)))))

(define (specialize goal-proc memo-template args . new-goal)
  ;; memo-template = (goal-proc-name bt ...)
  (prepare!)
  (apply specialize-after-prepare goal-proc memo-template args new-goal))

(define (specialize-after-prepare goal-proc memo-template args . new-goal)
  (let* ((bts (cdr memo-template))
	 (level (apply max 1 bts)))
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
    (prepare!)
    (apply start-memo-internal level
                               goal-proc-name
			       goal-proc
			       bts
			       args
			       new-goal)))

(define (prepare!)
  (clear-residual-program!) 
  (clear-memolist!)
  (clear-support-code!)
  (clear-deferred-list!)
  (gensym-reset!)
  (creation-log-initialize!)
  (poly-registry-reset!)
  (initialize-static-store!))

(define (start-memo-internal level fname fct bts args . new-goal)
  (let* ((result
	  (with-fresh-gensym-local
	   (lambda ()
	     (reset (multi-memo level level fname fct #f bts args)))))
	 ;; ####massive kludge
	 (result (if (and (pair? result) (eq? (car result) 'LET))
		     (car (cdaadr result))
		     result))
	 (goal-proc
	  (if (null? new-goal)
	      (first-residual-procedure)
	      (residual-definition-replace-name (first-residual-procedure)
						(car new-goal)))))
    (set-residual-program!
     (if *generate-flat-program*
	 (cons goal-proc
	       (rest-residual-procedures))
	 (list (residual-wrap-internal-definitions
		goal-proc
		(rest-residual-procedures)))))
    result))

(define (continue var value)
  (let ((static-deferred '())
	(dynamic-deferred '()))
    (for-each-deferred-list
     (lambda (key value)
       (if (> (car (memolist-entry->var value)) 0)
	   (set! dynamic-deferred (cons (cons key value) dynamic-deferred))
	   (set! static-deferred (cons (cons key value) static-deferred)))))
    (clear-deferred-list!)
    (for-each
     (lambda (kv)
       (let ((key (car kv))
	     (entry (cdr kv)))
	 (add-to-deferred-list! key entry)))
     static-deferred)
    (for-each
     (lambda (kv)
       (let ((entry (cdr kv)))
	 (generate-def entry (list var value))))
     dynamic-deferred)
    (let loop ((flag #f))
      (for-each-deferred-list
       (lambda (key entry)
	 (let ((entry-var (memolist-entry->var entry)))
	   (if (and (zero? (car entry-var))
		    (equal? (cadr entry-var) var))
	       (begin
		 (set! flag #t)
		 (set-car! entry-var -1)
		 (generate-def entry (list var value)))))))
      (if flag (loop #f)))))

(define memo-version-stamp "pgg-memo-dump-1")
(define deferred-version-stamp "pgg-deferred-dump-1")

(define (dump-entry key entry)
  (write `',key)
  (display #\space)
  (write (memolist-entry->name entry))
  (display #\space)
  (write (memolist-entry->bt entry))
  (display #\space)
  (let ((v (memolist-entry->value entry)))
    (if v
	(serialize v (list (memolist-entry->bt entry)))
	(write #f)))
  (display #\space)
  (write (memolist-entry->count entry))
  ;;(fct #f)
  (display #\space)
  (write (memolist-entry->var entry))
  (display #\space)
  (let ((pp (memolist-entry->pp entry))
	(bts (memolist-entry->bts entry)))
    (write (and pp (serialize pp bts)))
    (display #\space)
    (write bts))
  (newline))

(define (suspend filename)
  (with-output-to-file filename
    (lambda ()
      (write (gensym ""))
      (newline)
      (write memo-version-stamp)
      (newline)
      (for-each-memolist dump-entry)
      (newline)
      (write deferred-version-stamp)
      (newline)
      (for-each-deferred-list dump-entry)
      (newline))))

(define (read-entries add-to!)
  (let loop ((key (read)))
    (or (eof-object? key)
	(and (not (pair? key)) key)
	(let* ((name (read))
	       (bt (read))
	       (value (read))
	       (count (read))
	       (var (read))
	       (serialized-pp (read))
	       (bts (read)))
	  (let* ((entry (make-memolist-entry name))
		 ;; might have to move this to module cogen-library or into the genext
		 (fresh-pp (and serialized-pp
				(eval serialized-pp (interaction-environment))))
		 (fct (and serialized-pp
			   (eval (car fresh-pp) (interaction-environment)))))
	    (add-to! key entry)
	    (memolist-entry->bt! entry bt)
	    (memolist-entry->value! entry value) ;might be serialized, too!
	    (memolist-entry->count! entry count)
	    (memolist-entry->fct! entry fct)
	    (memolist-entry->var! entry var)
	    (memolist-entry->pp! entry fresh-pp)
	    (memolist-entry->bts! entry bts))
	  (loop (read))))))

(define (resurrect filename)
  (clear-residual-program!) 
  (clear-memolist!)
  (clear-support-code!)
  (clear-deferred-list!)
  (gensym-reset!)
  (creation-log-initialize!)
  (with-input-from-file filename
    (lambda ()
      (let ((old-count (read)))
	(gensym-reset! (- old-count)))
      (and (equal? (read) memo-version-stamp)
	   (equal? (read-entries add-to-memolist!) deferred-version-stamp)
	   (read-entries add-to-deferred-list!)))))

(define (generate-def entry extra-arg)
  (let ((bts (memolist-entry->bts entry))
	(bt (memolist-entry->bt entry))
	(full-pp (memolist-entry->pp entry))
	(fct (memolist-entry->fct entry)))
    (with-fresh-gensym-local
     (lambda ()
       (creation-log-push!)
       (let* ((cloned-pp (top-clone-dynamic full-pp bts))
	      (new-formals (map car (top-project-dynamic cloned-pp bts)))
	      (old-body-statics #f))

	 (let ((body
		(reset
		 (let* ((v0 (apply fct (cdr cloned-pp)))
			(v (if extra-arg (apply v0 extra-arg) v0)))
		   (if (not (zero? bt))
		       v
		       (let* ((return-v (list 'return v))
			      (body-statics
			       (top-project-static return-v (list bt)))
			      (body-dynamics
			       (top-project-dynamic return-v (list bt)))
			      (body-actuals
			       (map car body-dynamics)))
			 (if old-body-statics
			     (if (not (equal? body-statics old-body-statics))
				 (error "return type mismatch"))
			     (set! old-body-statics body-statics))
			 (memolist-entry->value!
			  entry
			  (top-clone-dynamic return-v))
			 (memolist-entry->bt! entry bt)
			 (apply make-residual-primop 'VALUES body-actuals)))))))
	   (make-residual-definition!
	    (memolist-entry->name entry)
	    new-formals
	    body))

;;;	    (make-residual-definition! (memolist-entry->name entry)
;;;				       new-formals
;;;				       (reset (apply fct (cdr cloned-pp))))

	 (creation-log-pop!))))))

;;; the memo-function
;;; - fn is the name of the function to memoize
;;; - args are the free variables of the function's body
;;; - bts are their binding times
(define (multi-memo level bt fname fct maybe-special bts args)
  (multi-memo-internal level bt fname fct maybe-special bts args #t))

(define (multi-memo-no-result level bt fname fct maybe-special bts args)
  (multi-memo-internal level bt fname fct maybe-special bts args #f))

(define (multi-memo-internal level bt fname fct maybe-special bts args result-needed)
  (let*
      ((full-pp (cons fname args))
       (static-pp (top-project-static full-pp bts))
       (dynamics (top-project-dynamic full-pp bts))
       (actuals (map car dynamics))
       (dyn-bts (map cdr dynamics))
       
       (found
	(if maybe-special
	    (or (lookup-deferred-list static-pp maybe-special)
		(let*
		    ((new-name (gensym-trimmed fname))
		     (new-entry (make-memolist-entry new-name)))
		  (memolist-entry->var! new-entry maybe-special)
		  (memolist-entry->fct! new-entry fct)
		  (memolist-entry->pp! new-entry full-pp)
		  (memolist-entry->bt! new-entry bt)
		  (memolist-entry->bts! new-entry bts)
		  (add-to-deferred-list! static-pp new-entry maybe-special)
		  new-entry))
	    (or (lookup-memolist static-pp)
		(let*
		    ((new-name (gensym-trimmed fname))
		     (new-entry (make-memolist-entry new-name))
		     (register-entry (add-to-memolist! static-pp new-entry)))
		  (memolist-entry->pp! new-entry full-pp)
		  (memolist-entry->bt! new-entry bt)
		  (memolist-entry->bts! new-entry bts)
		  (memolist-entry->fct! new-entry fct)
		  (generate-def new-entry #f)
		  new-entry))))
       (seen (memolist-entry->count! found (+ 1 (memolist-entry->count
						 found))))
       (res-name (memolist-entry->name found)))
    (call-with-values
     (lambda ()
       (if (= level 1)
	   (values res-name actuals)
	   (values 'MULTI-MEMO
		   (list (- level 1)
			 (- bt 1)
			 `',res-name
			 res-name
			 (and maybe-special
			      (make-residual-primop 'LIST
						    (- (car maybe-special) 1)
						    (cadr maybe-special)))
			 `',(map pred dyn-bts)
			 (apply make-residual-primop 'LIST actuals)))))
     (lambda (proc args)
       (if result-needed
	   (if (zero? bt)
	       (let* ((** (let loop () ;; busy waiting: not with true concurrency
			    (if (not (memolist-entry->value found))
				(begin
				  (relinquish-timeslice)
				  (loop)))))
		      (cloned-return-v (top-clone-dynamic (memolist-entry->value found) (list bt)))
		      (dynamics (top-project-dynamic cloned-return-v (list bt)))
		      (formals (map car dynamics)))
		 (shift
		  k
		  (make-residual-primop
		   'CALL-WITH-VALUES
		   (make-residual-closed-lambda '() 'FREE (make-residual-call proc args))
		   (make-residual-closed-lambda formals 'FREE (k (cadr cloned-return-v))))))
	       (_complete-serious proc args))
	   (_complete-serious-no-result proc args))))))

;;;was:
;;; 		 `(CALL-WITH-VALUES
;;; 		   (LAMBDA ()
;;; 		     ,call-expr)
;;; 		   (LAMBDA ,formals
;;; 		     ,(k (cadr cloned-return-v))))
