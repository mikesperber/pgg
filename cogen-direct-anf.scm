;;; $Id$
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;;
;;; includes the conversion of the residual code to A-normal form
;;; hence performs full context propagation
;;;

(set-scheme->abssyn-let-insertion! #f)

;;; interface to create generating extensions
;;; syntax constructors
(define (make-ge-var l v)
  v)
(define (make-ge-freevar l v)
  `(_FREEVAR ,l ,v))
(define (make-ge-const c)
  (if (number? c)
      c
      `',c))
(define (make-ge-cond l lb c t e)
  `(_IF ,l ,c ,t ,e))
(define (make-ge-op l o args)
  `(_OP ,l ,o ,@args))
(define (make-ge-op-pure l o args)
  `(_OP_PURE ,l ,o ,@args))
(define (make-ge-call f bts args)
  `(,f ,@args))
(define (make-ge-let hl unf? bl v e body)
  (make-residual-let v e body))
(define (make-ge-begin hl prop? e1 e2)
  (if (zero? hl)
      (make-residual-begin e1 e2)
      `(_BEGIN ,hl ,prop? ,e1 ,e2)))
(define (make-ge-lambda-memo l vars btv label fvars bts body)
  `(_LAMBDA_MEMO ,l ',vars ',label (LIST ,@fvars) ',bts
		 (LAMBDA ,fvars (LAMBDA ,vars ,body))))
(define (make-ge-vlambda-memo l fixed-vars var btv label fvars bts body)
  `(_VLAMBDA_MEMO ,l ',fixed-vars ',var ',label (LIST ,@fvars) ',bts
		 (LAMBDA ,fvars (LAMBDA ,(append fixed-vars var) ,body))))
(define (make-ge-app-memo l f btv args)
  `(_APP_MEMO ,l ,f ,@args))
(define (make-ge-lambda l vars btv body)
  (if (zero? l)
      (make-residual-closed-lambda vars '() body)
      `(_LAMBDA ,l ,vars ,body)))
(define (make-ge-vlambda l fixed-vars var btv body)
  (if (zero? l)
      (make-residual-closed-lambda (append fixed-vars var) '() body)
      `(_VLAMBDA ,l ,fixed-vars ,var ,body)))
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
(define (make-ge-make-cell-memo l label bt a)
  `(_MAKE-CELL_MEMO ,l ,label ,bt ,a))
(define (make-ge-cell-ref-memo l a)
  `(_S_T_MEMO ,l CELL-REF ,a))
(define (make-ge-cell-set!-memo l r a)
  `(_CELL-SET!_MEMO ,l ,r ,a))
(define (make-ge-cell-eq?-memo l args)
  `(_CELL-EQ?_MEMO ,l ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an implementation using macros

(define-syntax _app
  (syntax-rules ()
    ((_app 0 e ...)
     (e ...))
    ((_app 1 e ...)
     (_complete-serious (make-residual-call e ...)))
    ((_app lv e ...)
     (_complete (make-residual-generator '_APP (pred lv) e ...)))))

(define-syntax _app_memo
  (syntax-rules ()
    ((_app_memo 0 f arg ...)
     ((f 'VALUE) arg ...))
    ((_app_memo lv e ...)
     (_complete (make-residual-generator _APP_MEMO (pred lv) e ...)))))

(define-syntax _lambda
  (syntax-rules ()
    ((_lambda 0 vars body)
     (lambda vars body))
    ((_lambda lv vars body)
     (_lambda-internal lv 'vars (lambda vars body)))))

(define (_lambda-internal lv arity f)
  (let* ((vars (map gensym-local arity))
	 (body (reset (apply f vars))))
    (_complete				;don't duplicate, experimental
     (make-ge-lambda (pred lv) vars #f body))))

(define-syntax _lambda_memo
  (syntax-rules ()
    ((_lambda_memo 0 arity label vvs bts f)
     (static-constructor label f vvs bts))
    ((_lambda_memo arg ...)
     (_lambda_memo-internal arg ...))))

(define (_lambda_memo-internal lv arity label vvs bts f)
  (address-registry-reset!)
  (address-map-reset!)
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
	 (new-bts (binding-times compressed-dynamics))
	 (formal-fvs (map cdr clone-map)))
    ;; (> lv 0)
    (_complete
     `(_LAMBDA_MEMO
       ,(- lv 1)
       ',arity
       ',(gensym 'cls)
       (LIST ,@actual-fvs)
       ',new-bts
       (LAMBDA ,formal-fvs
	 (LAMBDA ,formals
	   ,(reset (apply (apply f cloned-vvs) formals))))))))

(define-syntax _vlambda
  (syntax-rules ()
    ((_vlambda 0 () var body)
     (lambda var body))
    ((_vlambda 0 (fixed-var ...) var body)
     (lambda (fixed-var ... . var) body))
    ((_vlambda lv (fixed-var ...) var body)
     (_vlambda-internal lv '(var fixed-var ...)
			(lambda (var fixed-var ...) body)))))

(define (_vlambda-internal lv arity f)
  (let* ((vars (map gensym-local arity))
	 (body (reset (apply f vars))))
    (_complete				;don't duplicate, experimental
     (make-ge-vlambda (pred lv) (cdr vars) (car vars) #f body))))

(define-syntax _vlambda_memo
  (syntax-rules ()
    ((_vlambda_memo 0 arity var label vvs bts f)
     (static-constructor label f vvs bts))
    ((_vlambda_memo arg ...)
     (_vlambda_memo-internal arg ...))))

(define (_vlambda_memo-internal lv arity var label vvs bts f)
  (address-registry-reset!)
  (address-map-reset!)
  (let* ((fixed-formals (map gensym-local arity))
	 (formal (gensym-local var))
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
	 (new-bts (binding-times compressed-dynamics))
	 (formal-fvs (map cdr clone-map)))
    ;; (> lv 0)
    (_complete
     `(_VLAMBDA_MEMO
       ,(- lv 1)
       ',arity
       ',var
       ',(gensym 'cls)
       (LIST ,@actual-fvs)
       ',new-bts
       (LAMBDA ,formal-fvs
	 (LAMBDA ,(append fixed-formals formal)
	   ,(reset (apply (apply f cloned-vvs)
			  fixed-formals
			  (list formal)))))))))	;horrendously wrong

(define-syntax _begin
  (syntax-rules ()
    ((_begin 0 bl e1 e2)
     (begin e1 e2))
    ((_begin 1 bl e1 e2)
     (shift k (make-residual-begin e1 (reset (k e2)))))
    ((_begin lv bl e1 e2)
     (shift k `(_BEGIN ,(pred lv) 0 ,e1 ,(reset (k e2)))))))

(define-syntax _ctor_memo
  (syntax-rules ()
    ((_ctor_memo 0 bts ctor arg ...)
     (static-constructor 'ctor ctor (list arg ...) 'bts))
    ((_ctor_memo lv (bt ...) ctor arg ...)
     (_complete `(_CTOR_MEMO ,(pred lv) (,(pred bt) ...) ctor ,arg ...)))))

(define-syntax _s_t_memo
  (syntax-rules ()
    ((_s_t_memo 0 sel v)
     (sel (v 'VALUE)))
    ((_s_t_memo lv sel v)
     (_complete `(_S_T_MEMO ,(pred lv) sel ,v)))))

(define-syntax _make-cell_memo
  (syntax-rules ()
    ((_make-cell_memo 0 lab bt arg)
     (static-cell lab arg bt))
    ((_make-cell_memo lv lab bt arg)
     (_complete `(_MAKE-CELL_MEMO ,(pred lv) lab ,(pred bt) ,arg)))))

(define-syntax _cell-set!_memo
  (syntax-rules ()
    ((_cell-set!_memo 0 ref arg)
     ((ref 'CELL-SET!) arg))
    ((_cell-set!_memo lv ref arg)
     (_complete `(_CELL-SET!_MEMO ,(pred lv) ,ref ,arg)))))

(define-syntax _cell-eq?_memo
  (syntax-rules ()
    ((_cell-eq?_memo 0 ref1 ref2)
     (eq? (ref1 'VALUE) (ref2 'VALUE)))
    ((_cell-eq?_memo lv ref1 ref2)
     (_complete `(_CELL-EQ?_MEMO ,(pred lv) ,ref1 ,ref2)))))

(define-syntax _if
  (syntax-rules ()
    ((_if 0 e1 e2 e3)
     (if e1 e2 e3))
    ((_if 1 e1 e2 e3)
     (shift k (make-residual-if e1 (reset (k e2)) (reset (k e3)))))
    ((_if 1 e1 e2 e3)
     (shift k (let* ((cond-code e1)
		     (the-store (current-static-store!))
		     (then-code (reset (k e2)))
		     (xxxxxxxxx (install-static-store! the-store))
		     (else-code (reset (k e3))))
		(make-residual-if cond-code then-code else-code))))
    ((_if lv e1 e2 e3)
     (shift k (let* ((cond-code e1)
		     (the-store (current-static-store!))
		     (then-code (reset (k e2)))
		     (xxxxxxxxx (install-static-store! the-store))
		     (else-code (reset (k e3))))
		(make-residual-generator
		 '_IF (pred lv) cond-code then-code else-code))))))

(define-syntax _op
  (syntax-rules (apply cons _define_data _define)
    ((_op lv _define_data arg)
     (make-residual-define-data lv arg))
    ((_op lv _define var arg)
     (make-residual-define-mutable lv 'var arg))
    ((_op 0 op arg ...)
     (op arg ...))
    ((_op 1 cons e1 e2)
     (_complete (make-residual-cons e1 e2)))
    ((_op 1 apply f arg)
     (_complete-serious (make-residual-apply f arg)))
    ((_op 1 op arg ...)
     (_complete `(op ,arg ...)))
    ((_op lv op arg ...)
     (_complete `(_OP ,(pred lv) op ,arg ...)))))

(define-syntax _op_pure
  (syntax-rules (cons)
    ((_op_pure 0 op arg ...)
     (op arg ...))
    ((_op_pure 1 cons e1 e2)
     (make-residual-cons e1 e2))
    ((_op_pure 1 op arg ...)
     `(op ,arg ...))
    ((_op_pure lv op arg ...)
     (_complete `(_OP_PURE ,(pred lv) op ,arg ...)))))

(define-syntax _freevar
  (syntax-rules ()
    ((_freevar 0 arg)
     arg)
    ((_freevar 1 arg)
     'arg)
    ((_freevar lv arg)
     `(_FREEVAR ,(pred lv) arg))))

(define-syntax _lift0
  (syntax-rules ()
    ((_lift0 1 val)
     (make-residual-literal val))
    ((_lift0 lv val)
     `(_LIFT0 ,(pred lv) ',val))))

(define-syntax _lift
  (syntax-rules ()
    ((_lift 0 diff value)
     (_lift0 diff value))
    ((_lift 1 diff value)
     `(_LIFT0 ,diff ,value))
    ((_lift lv diff value)
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
