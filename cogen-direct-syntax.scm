;;; $Id$
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;;

;;; set result to the unit of the identity monad
(define result id)
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
  `(_LET ,l ',v ,e (LAMBDA (,v) ,body)))
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
  `(_SEL_MEMO ,l ,sel ,a))
(define (make-ge-test-memo l tst a)
  `(_TEST_MEMO ,l ,tst ,a))
(define (make-ge-ctor l ctor args)
  `(_CTOR ,l ,ctor ,@args))
(define (make-ge-sel l sel a)
  `(_SEL ,l ,sel ,a))
(define (make-ge-test l tst a)
  `(_TEST ,l ,tst ,a))
(define (make-ge-lift l diff a)
  `(_LIFT ,l ,diff ,a))
(define (make-ge-eval l diff a)
  `(_EVAL ,l ,diff ,a))


(define (make-residual-let var exp body)
  (if (and (pair? body) (memq (car body) '(LET LET*)))
      (let ((header (cadr body))
	    (body (caddr body)))
	`(LET* ((,var ,exp) ,@header) ,body))
      `(LET ((,var ,exp)) ,body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an implementation using macros

(define-syntax _app
  (syntax-rules ()
    ((_app 0 f arg ...)
     (f arg ...))
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

(define (_lambda_memo lv arity label vvs bts f)
  (let* ((vars (map gensym-local arity)))
    (let* ((dynamics (project-dynamic (cons label vvs) bts))
	   (new-vvs (apply append dynamics))
	   (new-bts (binding-times dynamics))
	   (freevars (map (lambda (bt vv)
			    (gensym-local 'var))
			  new-bts new-vvs)))
      (if (= lv 1)
	  `(STATIC-CONSTRUCTOR
	    ',label
	    (LAMBDA ,freevars
	      (LAMBDA ,vars
		,(reset (apply (apply f vvs) vars))))
	    (LIST ,@new-vvs)
	    ',new-bts)
	  ;; > lv 1
	  `(_LAMBDA_MEMO
	    ,(- lv 1)
	    ',arity
	    ',(gensym 'cls)
	    (LIST ,@new-vvs)
	    ',new-bts
	    (LAMBDA ,freevars
	      (LAMBDA ,vars
		,(reset (apply (apply f vvs) vars)))))))))

(define (_vlambda lv arity var f)
  (let* ((vars (map gensym-local arity))
	 (vvar (gensym-local var))
	 (fun `(LAMBDA ,(append vars vvar)
		 ,(reset (apply f vars))))) ;unclear what to do with vvar
    (if (= lv 1)
	fun
	`(_VLAMBDA ,(pred lv) ',arity ',var ,fun))))

(define (_let lv orig-var e f)
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
      (shift k
	     `(_LET ,(pred lv) ',orig-var
		    ,e (LAMBDA (,var)
			 ,(reset (k (f var))))))))))

(define-syntax _ctor_memo
  (syntax-rules ()
    ((_ 0 bts ctor arg ...)
     (static-constructor 'ctor ctor (list arg ...) 'bts))
    ((_ lv (bt ...) ctor arg ...)
     `(_CTOR_MEMO ,(pred lv) (,(pred bt) ...) ctor ,arg ...))))

(define-syntax _sel_memo
  (syntax-rules ()
    ((_ 0 sel v)
     (sel (v 'VALUE)))
    ((_sel_memo lv sel v)
     `(_SEL_MEMO ,(pred lv) sel ,(maybe-reset v)))))

(define-syntax _test_memo
  (syntax-rules ()
    ((_ 0 ctor-test v)
     (ctor-test (v 'VALUE)))
    ((_ lv ctor-test v)
     `(_TEST_MEMO ,(pred lv) ctor-test ,(maybe-reset v)))))

(define-syntax _ctor
  (syntax-rules ()
    ((_ 0 ctor arg ...)
     (ctor arg ...))
    ((_ lv ctor arg ...)
     `(_CTOR ,(pred lv) ctor ,arg ...))))

(define-syntax _sel
  (syntax-rules ()
    ((_ 0 sel v)
     (sel v))
    ((_ lv sel v)
     `(_SEL ,(pred lv) sel ,(maybe-reset v)))))

(define-syntax _test
  (syntax-rules ()
    ((_ 0 ctor-test v)
     (ctor-test v))
    ((_ lv ctor-test v)
     `(_TEST ,(pred lv) ctor-test ,(maybe-reset v)))))

(define-syntax _if
  (syntax-rules ()
    ((_if 0 e1 e2 e3)
     (if e1 e2 e3))
    ((_if lv e1 e2 e3)
     (shift k `(_IF ,(pred lv)
		    ,(maybe-reset e1)
		    ,(reset (k e2))
		    ,(reset (k e3)))))))

(define-syntax _op
  (syntax-rules ()
    ((_op 1 op arg ...)
     `(op ,(maybe-reset arg) ...))
    ((_op lv op arg ...)
     `(_OP ,(pred lv) op ,(maybe-reset arg) ...))))

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
     (start-memo-internal level 'fn fn bts args))))

(define (nextlevel memo-template args)
  (let ((level (list-ref memo-template 1))
	(goal-proc (list-ref memo-template 3))
	(bts (cadr (list-ref memo-template 4))))
    (start-memo-internal level
			 goal-proc
			 (eval goal-proc (interaction-environment))
			 bts
			 args)))

(define (start-memo-internal level fname fct bts args)
  (clear-residual-program!) 
  (clear-memolist!)
  (gensym-local-reset!)
  (let* ((result (multi-memo level fname fct bts args))
	 (goal-proc (car *residual-program*))
	 (defn-template (take 2 goal-proc))
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
       (actuals (apply append dynamics))
       (found
	(or (assoc pp *memolist*)
	    (let*
		((new-name (gensym fname))
		 (cloned-pp (clone-dynamic full-pp bts))
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

