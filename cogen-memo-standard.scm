;;; cogen-memo-standard.scm

;;; copyright © 1996, 1997, 1998, 1999 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; memo function stuff: standard implementation

(define-record memolist-entry
  (name) (value '()) (bt #f) (count 0) (fct #f) (var #f) (pp #f) (bts #f))

(define-syntax start-memo
  (syntax-rules ()
    ((_ level fn bts args)
     (start-memo-internal level 'fn fn bts args))
    ((_ level fn bts args new-goal)
     (start-memo-internal level 'fn fn bts args))))

(define (specialize goal-proc memo-template args . new-goal)
  ;; memo-template = (goal-proc-name bt ...)
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
	 (result (reset (multi-memo level level fname fct #f bts args)))
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
  (let ((deferred '()))
    (for-each-deferred-list
     (lambda (key value)
       (set! deferred (cons (cons key value) deferred))))
    (clear-deferred-list!)
    ;; what else must be cleared?
    (for-each
     (lambda (kv)
       (let ((key (car kv))
	     (entry (cdr kv)))
	 (if (equal? (memolist-entry->var entry) var)
	     (begin
	       (set-car! (cdr (memolist-entry->pp entry)) value)
	       (generate-def entry))
	     (add-to-deferred-list! key entry))))
     deferred)))

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
    (if (null? v)
	(write '())
	(serialize v (list (memolist-entry->bt entry)))))
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
  (gensym-local-reset!)
  (gensym-reset!)
  (creation-log-initialize!)
  (with-input-from-file filename
    (lambda ()
      (let ((old-count (read)))
	(gensym-reset! (- old-count)))
      (and (equal? (read) memo-version-stamp)
	   (equal? (read-entries add-to-memolist!) deferred-version-stamp)
	   (read-entries add-to-deferred-list!)))))

(define (generate-def entry)
  (let ((bts (memolist-entry->bts entry))
	(bt (memolist-entry->bt entry))
	(full-pp (memolist-entry->pp entry))
	(fct (memolist-entry->fct entry)))
    (gensym-local-push!)
    (creation-log-push!)
    (let* ((cloned-pp (top-clone-dynamic full-pp bts))
	   (new-formals (map car (top-project-dynamic cloned-pp bts))))

      (let ((body
	     (reset
	      (let ((v (apply fct (cdr cloned-pp))))
		(if (not (zero? bt))
		    v
		    (let* ((return-v (list 'return v))
			   (body-dynamics
			    (top-project-dynamic return-v (list bt)))
			   (body-actuals
			    (map car body-dynamics)))
		      (memolist-entry->value!
		       entry
		       (top-clone-dynamic return-v))
		      (memolist-entry->bt! entry bt)
		      `(VALUES ,@body-actuals)))))))
	(make-residual-definition!
	 (memolist-entry->name entry)
	 new-formals
	 body))

;;;	    (make-residual-definition! (memolist-entry->name entry)
;;;				       new-formals
;;;				       (reset (apply fct (cdr cloned-pp))))

      (creation-log-pop!)
      (gensym-local-pop!))))

;;; the memo-function
;;; - fn is the name of the function to memoize
;;; - args are the free variables of the function's body
;;; - bts are their binding times
(define (multi-memo level bt fname fct maybe-var bts args)
  (multi-memo-internal level bt fname fct maybe-var bts args #t))

(define (multi-memo-no-result level bt fname fct maybe-var bts args)
  (multi-memo-internal level bt fname fct maybe-var bts args #f))

(define (multi-memo-internal level bt fname fct maybe-var bts args result-needed)
  (let*
      ((special (and maybe-var (zero? (car bts))))
       (%garbage% (if special (set-car! args maybe-var)))
       (full-pp (cons fname args))
       (static-pp (top-project-static full-pp bts))
       (dynamics (top-project-dynamic full-pp bts))
       (actuals (map car dynamics))
       (dyn-bts (map cdr dynamics))
       
       (found
	(if special
	    (or (lookup-deferred-list static-pp)
		(let*
		    ((new-name (gensym-trimmed fname))
		     (new-entry (make-memolist-entry new-name)))
		  (memolist-entry->var! new-entry maybe-var)
		  (memolist-entry->fct! new-entry fct)
		  (memolist-entry->pp! new-entry full-pp)
		  (memolist-entry->bt! new-entry bt)
		  (memolist-entry->bts! new-entry bts)
		  (add-to-deferred-list! static-pp new-entry)
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
		  (generate-def new-entry)
		  new-entry))))
       (seen (memolist-entry->count! found (+ 1 (memolist-entry->count
						 found))))
       (res-name (memolist-entry->name found)))
    (let ((call-expr
	   (if (= level 1)
	       (apply make-residual-call res-name actuals)
	       (make-residual-call 'MULTI-MEMO
				   (- level 1)
				   ( - bt 1)
				   `',res-name
				   res-name
				   maybe-var
				   `',(map pred dyn-bts)
				   `(LIST ,@actuals)))))
      (if result-needed
	  (if (zero? bt)
	      (let* ((cloned-return-v (top-clone-dynamic (memolist-entry->value found)))
		     (dynamics (top-project-dynamic cloned-return-v))
		     (formals (map car dynamics)))
		(shift
		 k
		 `(CALL-WITH-VALUES
		   (LAMBDA ()
		     ,call-expr)
		   (LAMBDA ,formals
		     ,(k (cadr cloned-return-v))))))
	      (_complete-serious call-expr))
	  (_complete-serious-no-result call-expr)))))
