;;; $Id$
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;;
;;; includes the conversion of the residual code to A-normal form
;;; hence performs full context propagation
;;;

(set-scheme->abssyn-let-insertion! #t)
(set-memo-optimize! #f)

;;; this is vital in order to guarantee that every lambda carries
;;; information about its free variables

;;; interface to create generating extensions
;;; syntax constructors
(define (make-ge-var l v)
  `(_VAR ,l ,v))
(define (make-ge-const c)
  `',c)
(define (make-ge-cond l lb c t e)
  `(_IF ,l ,lb ,c ,t ,e))
(define (make-ge-op l o args)
  (if (trivial-operator? o)
      `(_OP ,l ,o ,@args)
      `(_OP_SERIOUS ,l ,o ,@args)))
(define (make-ge-call f bts args)
  `(,f ,@args))
(define (make-ge-let hl unf? bl v e body)
  ;;`(LET ((,v ,e)) ,body)
  `(_LET ,hl ,unf? ,bl ((,v ,e)) ,body))
(define (make-ge-begin hl prop? e1 e2)
  `(_BEGIN ,hl ,prop? ,e1 ,e2))
(define (make-ge-lambda-memo l vars btv label fvars bts body)
  `(_LAMBDA_MEMO ,l ',vars ',label (LIST ,@fvars) ',bts
		 (LAMBDA ,fvars (LAMBDA ,vars ,body))))
(define (make-ge-vlambda-memo l fixed-vars var btv label fvars bts body)
  `(_VLAMBDA_MEMO ,l ',fixed-vars ',var ',label (LIST ,@fvars) ',bts
		 (LAMBDA ,fvars (LAMBDA (,@fixed-vars . ,var) ,body))))
(define (make-ge-app-memo l f btv args)
  `(_APP_MEMO ,l ,f ,@args))
(define (make-ge-lambda l vars btv body)
  `(_LAMBDA ,l ,vars ,body))
(define (make-ge-app l f btv args)
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

(define-syntax _var
  (syntax-rules ()
    ((_ 0 v)
     v)
    ((_ 1 v)
     (if (symbol? v)			;HACK
	 (make-residual-var v)
	 v))))

(define-syntax _complete
  (syntax-rules ()
    ((_ body)
     (let ((var (gensym-local 'mlet)))
       (shift k (make-residual-let-trivial
		 var body (list (k (make-residual-var var)))))))))

(define-syntax _complete-serious
  (syntax-rules ()
    ((_ body)
     (let ((var (gensym-local 'mlet)))
       (shift k (make-residual-let-serious
		 var body (list (k (make-residual-var var)))))))))

(define-syntax _app
  (syntax-rules ()
    ((_app 0 e ...)
     (e ...))
    ((_app 1 e ...)
     (_complete-serious (make-residual-call e ...)))
    ((_app lv e ...)
     (_complete `(_APP ,(pred lv) ,e ...)))))

(define-syntax _app_memo
  (syntax-rules ()
    ((_app_memo 0 f arg ...)
     ((f 'VALUE) arg ...))
    ((_app_memo 1 e ...)
     (_complete-serious (make-residual-call e ...)))))

(define-syntax _lambda
  (syntax-rules ()
    ((_lambda 0 vars body)
     (lambda vars body))
    ((_lambda lv vars body)
     (_lambda-internal lv 'vars (lambda vars body)))))

(define (_lambda-internal lv arity f)
  (let* ((vars (map gensym-local arity))
	 (body (reset (apply f vars))))
    (if (= lv 1)
	(make-residual-closed-lambda vars 'FREE (list body))
	(make-ge-lambda (pred lv) vars #f body))))

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
    ;; this only works in the two-level case for (= lv 1)
    (make-residual-closed-lambda formals
				 actual-fvs
				 (list (reset (apply (apply f vvs) formals))))))


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
    ((_ 0 u? bl ((?v e)) body)
     (let ((?v e)) body))
    ((_ 1 u? bl ((?v e)) body)
     (_let-internal-1 u? bl '?v (reset e) (lambda (?v) body)))))

(define (_let-internal-1 unf? bl orig-var e f)
  (let ((var (gensym-local orig-var)))
    (shift k (make-residual-let-trivial var e (list (reset (k (f var))))))))

(define-syntax _begin
  (syntax-rules ()
    ((_ 0 bl e1 e2)
     (begin e1 e2))
    ((_ 1 bl e1 e2)
     (shift k (make-residual-begin e1 (reset (k e2)))))
    ((_ lv bl e1 e2)
     (shift k `(_BEGIN ,(pred lv) 0 ,e1 ,(reset (k e2)))))))

(define-syntax _ctor_memo
  (syntax-rules ()
    ((_ 0 bts ctor arg ...)
     (static-constructor 'ctor ctor (list arg ...) 'bts))
    ((_ 1 (bt ...) ctor arg ...)
     (_complete-serious
      (make-residual-call (make-residual-var 'ctor) arg ...)))))

(define-syntax _s_t_memo
  (syntax-rules ()
    ((_ 0 sel v)
     (sel (v 'VALUE)))
    ((_sel_memo 1 sel v)
     (_complete-serious 
      (make-residual-call (make-residual-var 'sel) v)))))

(define-syntax _if
  (syntax-rules ()
    ((_if 0 lb e1 e2 e3)
     (if e1 e2 e3))
    ((_if 1 bl e1 e2 e3)
     (shift k (make-residual-if e1 (reset (k e2)) (reset (k e3)))))
    ((_if lv bl e1 e2 e3)
     (shift k `(_IF ,(pred lv) 0
		    ,e1
		    ,(reset (k e2))
		    ,(reset (k e3)))))))

(define-syntax _op
  (syntax-rules (apply _define_data)
;;;    ((_op lv _define_data arg)
;;;     (make-residual-define-data lv arg))
    ((_op 0 op arg ...)
     (op arg ...))
    ((_op 1 apply f arg ...)
     (_complete-serious (make-residual-primop 'apply f arg ...)))
    ((_op 1 op arg ...)
     (_complete (make-residual-primop 'op arg ...)))
    ((_op lv op arg ...)
     (_complete `(_OP ,(pred lv) op ,arg ...)))))

(define-syntax _op_serious
  (syntax-rules ()
    ((_ 0 op arg ...)
     (op arg ...))
    ((_ 1 op arg ...)
     (_complete-serious (make-residual-call (make-residual-var 'op) arg ...)))
    ((_ lv op arg ...)
     (_complete `(_OP_SERIOUS ,(pred lv) op ,arg ...)))))

(define-syntax _lift0
  (syntax-rules ()
    ((_ 1 val)
     (make-residual-literal val))
    ((_ lv val)
     `(_LIFT0 ,(pred lv) ',val))))

(define-syntax _lift
  (syntax-rules ()
    ((_ 0 diff value)
     (_lift0 diff value))
    ((_ 1 diff value)
     `(_LIFT0 ,diff ,value))
    ((_ lv diff value)
     `(_LIFT ,(pred lv) ,diff ,value))))

(define-syntax _eval
  (syntax-rules ()
    ((_ 0 0 body)
     (eval body (interaction-environment)))
    ((_ 0 1 body)
     (_complete body))
    ((_ 0 diff body)
     (_complete `(_EVAL 0 ,(pred diff) ',body)))
    ((_ 1 0 body)
     (_complete `(EVAL ,body (INTERACTION-ENVIRONMENT))))
    ((_ 1 1 body)
     body)				;;;?????????? _complete ??????????
    ((_ lv diff body)
     (_complete `(_EVAL ,(pred lv) ,diff ,body)))))

;;; memo function stuff
(define-syntax start-memo
  (syntax-rules ()
    ((_ package filename fn bts args)
     (start-memo-internal package filename 1 'fn fn bts args))))

(define (start-memo-internal package filename level fname fct bts args)
  (initialize-residual-program! package filename)
  (clear-memolist!)
  (clear-support-code!)
  (gensym-local-reset!)
  (gensym-reset!)
  (let* ((initial-scope (gensym-local-push!))
	 (result (multi-memo level fname fct bts args))
	 (drop-scope (gensym-local-pop!)))
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
		 (new-def  (make-residual-definition! new-name
						      new-formals
						      (reset (apply fct (cdr cloned-pp))))))
	      (cons pp new-name))))
       (res-name (cdr found))
       (exit-scope (gensym-local-pop!)))
    (if (= level 1)
	;; generate call to fn with actual arguments
	(_complete-serious
	 (apply make-residual-call
		(make-residual-var res-name)
		(map (lambda (arg) (_var 1 arg)) actuals)))
	;; reconstruct multi-memo
	(error "this should not happen"))))
