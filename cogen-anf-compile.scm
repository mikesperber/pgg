;;; $Id$
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;;
;;; includes the conversion of the residual code to A-normal form
;;; hence performs full context propagation
;;;

(set-scheme->abssyn-let-insertion! #f)
(set-memo-optimize! #f)

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
    ;; this is wrong
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
	 (body (reset (apply f vars)))
	 (l (pred lv))
	 (generate-lambda
	  (if (zero? l)
	      (lambda ()
		(make-residual-closed-lambda vars 'FREE body))
	      (lambda ()
		(make-residual-generator-vve '_LAMBDA l vars body)))))
    (if *lambda-is-pure*
	(generate-lambda)
	(_complete			;don't duplicate, experimental
	 (generate-lambda)))))

(define-syntax _lambda_memo
  (syntax-rules ()
    ((_ 0 arity label vvs bts f)
     (static-constructor label f vvs bts))
    ((_ arg ...)
     (_lambda_memo-internal arg ...))))

(define (_lambda_memo-internal lv arity label vvs bts f)
  ;; the let* bindings are identical to cogen-direct-anf ---msp
  (let* ((formals (map gensym-local arity))
	 (lambda-pp (cons label vvs))
	 (dynamics (top-project-dynamic lambda-pp bts))
	 (compressed-dynamics (map remove-duplicates dynamics))
	 (actual-fvs (apply append compressed-dynamics))
	 (clone-map (map (lambda (arg)
			   (cons arg (if (symbol? arg)
					 (gensym-local arg)
					 (gensym-local 'clone))))
			 actual-fvs))
	 (cloned-pp (top-clone-with clone-map lambda-pp bts))
	 (cloned-vvs (cdr cloned-pp))
	 (new-bts (map pred (map cdr compressed-dynamics)))
	 (formal-fvs (map cdr clone-map)))
    ;; this only works in the two-level case for (= lv 1)
    ;; pjt: this is wrong
    (make-residual-closed-lambda formals
				 actual-fvs
				 (reset (apply (apply f vvs) formals)))))


(define (_vlambda lv arity var f)
  (let* ((vars (map gensym-local arity))
	 (vvar (gensym-local var))
	 (fun `(LAMBDA ,(append vars vvar)
		 ,(reset (apply f vars))))) ;unclear what to do with vvar
    (if (= lv 1)
	fun
	`(_VLAMBDA ,(pred lv) ',arity ',var ,fun))))

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
    ((_if 0 e1 e2 e3)
     (if e1 e2 e3))
    ((_if 1 e1 e2 e3)
     (shift k (make-residual-if e1 (reset (k e2)) (reset (k e3)))))
    ((_if lv e1 e2 e3)
     (shift k `(_IF ,(pred lv) 0
		    ,e1
		    ,(reset (k e2))
		    ,(reset (k e3)))))))

(define-syntax _op
  (syntax-rules (apply _define_data)
    ((_op lv _define_data arg) 'lose)
    ((_op 0 op arg ...)
     (op arg ...))
    ((_op 1 apply f arg ...)
     (_complete-serious (make-residual-primop 'apply f arg ...)))
    ((_op 1 op arg ...)
     (_complete (make-residual-primop 'op arg ...)))
    ((_op lv op arg ...)
     (_complete `(_OP ,(pred lv) op ,arg ...)))))

(define-syntax _op_serious
  (syntax-rules (_define_data)
    ((_ lv _define_data arg) 'lose)
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
