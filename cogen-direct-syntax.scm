;;; $Id$
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;;

(define-syntax maybe-reset		;exchange if needed
  (syntax-rules ()
    ((_ x) x)
    ((_ x) (reset x))))

;;; interface to create generating extensions
;;; syntax constructors
(define (make-ge-var l v)
  v)
(define (make-ge-const c)
  `(_LIFT0 1 ',c))
(define (make-ge-cond l c t e)
  `(_IF ,l ,c ,t ,e))
(define (make-ge-op l o args)
  `(_OP ,l ,o ,@args))
(define (make-ge-call f args)
  `(,f ,@args))
(define (make-ge-let l v e body)
  `(_LET ,l ((,v ,e)) ,body))
(define (make-ge-begin l e1 e2)
  `(_BEGIN ,l ,e1 ,e2))
(define (make-ge-lambda-memo l vars btv label fvars bts body)
  `(_LAMBDA_MEMO ',l ',vars ',label (LIST ,@fvars) ',bts
		 (LAMBDA ,fvars (LAMBDA ,vars ,body))))
(define (make-ge-vlambda-memo l fixed-vars var btv label fvars bts body)
  `(_VLAMBDA_MEMO ',l ',fixed-vars ',var ',label (LIST ,@fvars) ',bts
		 (LAMBDA ,fvars (LAMBDA (,@fixed-vars . ,var) ,body))))
(define (make-ge-app-memo l f btv args)
  `(_APP_MEMO ,l ,f ,@args))
(define (make-ge-lambda l vars btv body)
  `(_LAMBDA ',l ',vars (LAMBDA ,vars ,body)))
(define (make-ge-app l f args)
  `(_APP ,l ,f ,@args))
(define (make-ge-ctor-memo l bts ctor args)
  `(_CTOR_MEMO ,l ,bts ,ctor ,@args))
(define (make-ge-sel-memo l sel a)
  `(_S_T_MEMO ,l ,sel ,a))
(define (make-ge-test-memo l tst a)
  `(_S_T_MEMO ,l ,tst ,a))
(define (make-ge-ctor l ctor args)
  `(_OP ,l ,ctor ,@args))
(define (make-ge-sel l sel a)
  `(_OP ,l ,sel ,a))
(define (make-ge-test l tst a)
  `(_OP ,l ,tst ,a))
(define (make-ge-lift l diff a)
  `(_LIFT ,l ,diff ,a))
(define (make-ge-eval l diff a)
  `(_EVAL ,l ,diff ,a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an implementation using macros

(define-syntax _app
  (syntax-rules ()
    ((_app 0 f arg ...)
     (f arg ...))
    ((_app 1 f arg ...)
     `(,(maybe-reset f) ,(maybe-reset arg) ...))
    ((_app lv f arg ...)
     `(_APP ,(pred lv) ,(maybe-reset f) ,(maybe-reset arg) ...))))

(define-syntax _app_memo
  (syntax-rules ()
    ((_app_memo 0 f arg ...)
     ((f 'VALUE) arg ...))
    ((_app_memo lv f arg ...)
     `(_APP_MEMO ,(pred lv) ,(maybe-reset f) ,(maybe-reset arg) ...))))

(define (_lambda lv arity f)
  (let* ((vars (map gensym-local arity))
	 (fun `(LAMBDA ,vars ,(reset (apply f vars)))))
    (if (= lv 1)
	fun
	`(_LAMBDA ,(pred lv) ',arity ,fun))))

(define-syntax _lambda_memo
  (syntax-rules ()
    ((_ 0 arity label vvs bts f)
     (static-constructor label f vvs bts))
    ((_ arg ...)
     (_lambda_memo-internal arg ...))))

(define (_lambda_memo-internal lv arity label vvs bts f)
  (let* ((formals (map gensym-local arity))
	 (lambda-pp (cons label vvs))
	 (dynamics (project-dynamic lambda-pp bts))
	 (compressed-dynamics (map remove-duplicates dynamics))
	 (actual-fvs (apply append compressed-dynamics))
	 (clone-map (map (lambda (arg)
			   (cons arg (if (symbol? arg)
					 (gensym-local arg)
					 (gensym-local 'clone))))
			 actual-fvs))
	 (cloned-pp (clone-with clone-map lambda-pp bts))
	 (cloned-vvs (cdr cloned-pp))
	 (new-bts (binding-times compressed-dynamics))
	 (formal-fvs (map cdr clone-map)))
    ;; (> lv 0)
    `(_LAMBDA_MEMO
      ,(- lv 1)
      ',arity
      ',(gensym 'cls)
      (LIST ,@actual-fvs)
      ',new-bts
      (LAMBDA ,formal-fvs
	(LAMBDA ,formals
	  ,(reset (apply (apply f cloned-vvs) formals)))))))

(define (_vlambda lv arity var f)
  (let* ((vars (map gensym-local arity))
	 (vvar (gensym-local var))
	 (fun `(LAMBDA ,(append vars vvar)
		 ,(reset (apply f vars))))) ;unclear what to do with vvar
    (if (= lv 1)
	fun
	`(_VLAMBDA ,(pred lv) ',arity ',var ,fun))))

(define-syntax _let
  (syntax-rules ()
    ((_ 0 ((?v e)) body)
     (let ((?v e)) body))
    ((_ lv ((?v e)) body)
     (_let-internal lv '?v (maybe-reset e) (lambda (?v) body)))))

(define (_let-internal lv orig-var e f)
  (let ((var (gensym-local orig-var)))
    (cond
     ((zero? lv)
      (f e))
     ((= lv 1)
      (if (and (pair? e)
	       (not (equal? 'QUOTE (car e))))
	  (shift k (make-residual-let var e (reset (k (f var)))))
	  (f e)))
     (else
      (shift k (make-ge-let (pred lv) var e (reset (k (f var)))))))))

(define-syntax _begin
  (syntax-rules ()
    ((_ 0 e1 e2)
     (begin e1 e2))
    ((_ 1 e1 e2)
     (shift k (make-residual-begin (maybe-reset e1) (reset (k e2)))))
    ((_ lv e1 e2)
     (shift k `(_BEGIN ,(pred lv) ,(maybe-reset e1) ,(reset (k e2)))))))

(define-syntax _ctor_memo
  (syntax-rules ()
    ((_ 0 bts ctor arg ...)
     (static-constructor 'ctor ctor (list arg ...) 'bts))
    ((_ lv (bt ...) ctor arg ...)
     `(_CTOR_MEMO ,(pred lv) (,(pred bt) ...) ctor ,(maybe-reset arg) ...))))

(define-syntax _s_t_memo
  (syntax-rules ()
    ((_ 0 sel v)
     (sel (v 'VALUE)))
    ((_sel_memo lv sel v)
     `(_S_T_MEMO ,(pred lv) sel ,(maybe-reset v)))))

(define-syntax _if
  (syntax-rules ()
    ((_if 0 e1 e2 e3)
     (if e1 e2 e3))
    ((_if 1 e1 e2 e3)
     (shift k `(IF ,(maybe-reset e1)
		   ,(reset (k e2))
		   ,(reset (k e3)))))
    ((_if lv e1 e2 e3)
     (shift k `(_IF ,(pred lv)
		    ,(maybe-reset e1)
		    ,(reset (k e2))
		    ,(reset (k e3)))))))

(define-syntax _op
  (syntax-rules (apply)
    ((_op 0 op arg ...)
     (op arg ...))
    ((_op 1 apply arg ...)
     (_apply (list arg ...)))
    ((_op 1 op arg ...)
     `(op ,(maybe-reset arg) ...))
    ((_op lv op arg ...)
     `(_OP ,(pred lv) op ,(maybe-reset arg) ...))))

(define (_apply args)
  (let ((fn (car args))
	(fa (cadr args)))
    (let loop ((fa fa) (acc '()))
      (if (pair? fa)
	  (if (equal? (car fa) 'CONS)
	      (loop (caddr fa) (cons (cadr fa) acc))
	      (if (and (equal? (car fa) 'QUOTE)
		       (equal? (cadr fa) '()))
		  `(,fn ,@(reverse acc))
		  `(APPLY ,@args)))
	  `(APPLY ,@args)))))

(define-syntax _lift0
  (syntax-rules ()
    ((_ 1 val)
     (if (or (number? val) (string? val) (boolean? val))
	 val
	 `(QUOTE ,val)))
    ((_ lv val)
     `(_LIFT0 ,(pred lv) ',val))))

(define-syntax _lift
  (syntax-rules ()
    ((_ 1 diff value)
     `(_LIFT0 ,diff ,value))
    ((_ lv diff value)
     `(_LIFT ,(pred lv) ,diff ,value))))

(define-syntax _eval
  (syntax-rules ()
    ((_ 0 0 body)
     (eval body (interaction-environment)))
    ((_ 0 1 body)
     body)
    ((_ 0 diff body)
     `(_EVAL 0 ,(pred diff) ',body))
    ((_ 1 0 body)
     `(EVAL ,body (INTERACTION-ENVIRONMENT)))
    ((_ 1 1 body)
     body)
    ((_ lv diff body)
     `(_EVAL ,(pred lv) ,diff ,body))))

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
  (gensym-local-reset!)
  (gensym-reset!)
  (let* ((result (multi-memo level fname fct bts args))
	 (goal-proc (car *residual-program*))
	 (defn-template (take 2 goal-proc))
	 ;; kludge alert
	 (defn-template
	   (if (null? new-goal)
	       defn-template
	       (list (car defn-template)
		     (cons (car new-goal) (cdadr defn-template)))))
	 (defn-body (list-tail goal-proc 2)))
    (set! *residual-program*
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
      ((enter-scope (gensym-local-push!))
       (full-pp (cons fname args))
       (pp (project-static full-pp bts))
       (dynamics (project-dynamic full-pp bts))
       ; (compressed-dynamics (map remove-duplicates dynamics))
       (actuals (apply append dynamics))
       (found
	(or (assoc pp *memolist*)
	    (let*
		((new-name (gensym fname))
		 ; (clone-map (map (lambda (arg)
		 ; 		   (cons arg (if (symbol? arg)
		 ; 				 (gensym-local arg)
		 ; 				 (gensym-local 'clone))))
		 ; 		 actuals))
		 (cloned-pp (clone-dynamic full-pp bts))
		 ; (new-formals (map cdr clone-map))
		 (new-formals (apply append (project-dynamic cloned-pp bts)))
		 (new-entry (add-to-memolist! (cons pp new-name)))
		 (new-def  `(DEFINE (,new-name ,@new-formals)
			      ,(reset (apply fct (cdr cloned-pp))))))
	      (add-to-residual-program! new-def)
	      (cons pp new-name))))
       (res-name (cdr found))
       (exit-scope (gensym-local-pop!)))
    (if (= level 1)
	;; generate call to fn with actual arguments
	`(,res-name ,@actuals)
	;; reconstruct multi-memo
	`(MULTI-MEMO ,(- level 1)
		     ',res-name ,res-name
		     ',(binding-times dynamics)
		     (LIST ,@actuals)))))

