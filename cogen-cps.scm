;;; library functions for a multi-level generator using CPS after BTA,
;;; higher-order abstract syntax, and a special function
;;; representation to accomplish proper function memoization in the
;;; presence of dynamic free variables
;;; $Id$
;;;

;;; the following types are involved:
;;; Value = AtomicValue
;;;       + Constructor \times 2Value^*
;;;       + Function (2Value^* -> 2Value)
;;; Code
;;; K Code : an object in the CPS monad delivering Code
;;; 2Value = Value + K Code 
;;; Level = NaturalNumber

;;; set result to the unit of the continuation monad
(define result result-c)

;;; interface to create generating extensions
;;; syntax constructors
(define (make-ge-var l v)
  v)
(define (make-ge-const c)
  `(_LIFT0 1 ,c))
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
		 (LAMBDA_RESULT ,fvars (LAMBDA_RESULT ,vars ,body))))
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
  `(_ST_MEMO ,l ,sel ,a))
(define (make-ge-test-memo l tst a)
  `(_ST_MEMO ,l ,tst ,a))
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


(define (make-residual-let var exp body)
  (if (and (pair? body) (memq (car body) '(LET LET*)))
      (let ((header (cadr body))
	    (body (caddr body)))
	`(LET* ((,var ,exp) ,@header) ,body))
      `(LET ((,var ,exp)) ,body)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; application with arbitrary many arguments
;;; _app : Level \times K Code \times (K Code)^* -> K Code
(define (_app lv fc . argsc)
  (if (zero? lv)
      (lambda (kappa)
	(fc (lambda (f)
	      (lib-arg-list
	       argsc
	       (lambda (args)
		 ((apply f args) kappa))))))
      (lambda (kappa)
	(kappa
	 `(_APP ,(- lv 1)
		,(fc id)
		,@(map (lambda (ac) (ac id)) argsc))))))

;;; dito for memoized functions
;;; _app_memo : Level \times K Code \times (K Code)^* -> K Code
(define (_app_memo lv fc . argsc)
  (if (zero? lv)
      (lambda (kappa)
	(fc (lambda (f)
	      (lib-arg-list
	       argsc
	       (lambda (args)
		 ((apply (f 'value) args) kappa))))))
      (lambda (kappa)
	(kappa
	 `(_APP_MEMO ,(- lv 1)
		     ,(fc id)
		     ,@(map (lambda (ac) (ac id)) argsc))))))

;;; lambda abstraction with arbitrary arity
;;;
;;; _lambda_memo : Level \times Ident^* \times Label \times 2Value^*
;;; \times BT^* \times (2Value^* -> (K Code)^* -> K Code) -> K Code
(define-syntax _lambda_memo
  (syntax-rules ()
    ((_ 0 arity label fvcs bts f)
     (lambda (k)
       (k (static-constructor label f (map (lambda (g) (g id)) fvcs) bts))))
    ((_ arg ...)
     (_lambda_memo-internal arg ...))))

(define (_lambda_memo-internal lv arity label fvcs bts f)
  (lambda (kappa)
    (let* ((vars (map gensym-local arity))
	   (varcs (map result vars))
	   (lambda-pp (cons label (map (lambda (comp) (comp id)) fvcs)))
	   (dynamics (project-dynamic lambda-pp bts))
	   (new-freevars (apply append dynamics))
	   (new-bts (binding-times dynamics))
	   (cloned-pp (clone-dynamic lambda-pp bts))
	   (formal-fvs (apply append (project-dynamic cloned-pp bts)))
	   (actual-fvs (cdr cloned-pp)))
      ;; lv >= 1
      (kappa `(_LAMBDA_MEMO
	       ,(- lv 1)
	       ',arity
	       ',(gensym 'cls)
	       (LIST ,@new-freevars)
	       ',new-bts
	       (LAMBDA_RESULT ,formal-fvs
                 (LAMBDA_RESULT ,vars
		   ,((apply (apply f actual-fvs) vars) id))))))))

(define-syntax lambda_result
  (syntax-rules ()
    ((_ () body ...)
     (lambda () body ...))
    ((_ (fv ...) body ...)
     (lambda (fv ...)
       (let ((fv (result fv)) ...) body ...)))))

;;; lambdas that are never memoized
;;; remember we live in CPS: variables must ALWAYS be bound to
;;; continuations returning variables!
;;; `arity' is the list of the original variable names

(define (_lambda lv arity f)
  (if (zero? lv)
      (result (lambda vars
		(apply f (map result vars))))
      (let* ((vars (map gensym-local arity))
	     (varcs (map result vars)))
	(lambda (kappa)
	  (kappa `(_LAMBDA ,(- lv 1) ',arity
			   (LAMBDA ,vars ,((apply f varcs) id))))))))

;;; LET with context propagation
(define-syntax _let
  (syntax-rules ()
    ((_ lv ((?v e)) body)
     (_let-internal lv '?v e (lambda (?v) body)))))

(define (_let-internal lv orig-var ec f)
  (lambda (kappa)
    (let* ((var (gensym orig-var))
	   (varc (lambda (k) (k var))))
      (ec
       (lambda (value)
	 (cond
	  ((zero? lv)
	   ((f (result value)) kappa))
	  ((and (= lv 1) (or (not (pair? value)) (equal? 'QUOTE (car value))))
	   ((f (result value)) kappa))
	  (else
	   `(_LET ,(- lv 1) ((,var ,value))
		  ,((f varc) kappa)))))))))

(define (_begin lv e1c e2c)
  (lambda (kappa)
    (if (= lv 0)
	(e1c (lambda (y1) (e2c kappa)))
	(e1c (lambda (y1) `(_BEGIN ,(pred lv) ,y1 ,(e2c kappa)))))))

;;; constructors with memoization
(define-syntax _ctor_memo
  (syntax-rules ()
    ((_ 0 bts ctor argc ...)
     (lambda (k)
       (lib-arg-list (list argc ...)
		     (lambda (args)
		       (k (STATIC-CONSTRUCTOR 'ctor
					      ctor
					      args
					      'bts) )))))
    ((_ lv (bt ...) ctor argc ...)
     (lambda (k)
       (k `(_CTOR_MEMO ,(pred lv) (,(pred bt) ...) ctor
		       ,(argc id) ...))))))

;;; selectors and tests for constructors with memoization
(define-syntax _s_t_memo
  (syntax-rules ()
    ((_ 0 sel/test argc)
     (lambda (k) (argc (lambda (arg) (k (sel/test (arg 'VALUE)))))))
    ((_ lv sel/test argc)
     (lambda (k) (k `(_S_T_MEMO ,(pred lv) sel/test ,(argc id)))))))

;;; conditional
(define (_If level e1c e2c e3c)
  (lambda (kappa)
    (e1c (lambda (e1)
	  (if (zero? level)
	      (if e1 (e2c kappa) (e3c kappa))
	      `(_IF ,(- level 1) ,e1 ,(e2c kappa) ,(e3c kappa)))))))

;;; primitive operators
(define-syntax _op
  (syntax-rules ()
    ((_ 0 op argc ...)
     (lambda (k) (lib-arg-list (list argc ...) (lambda (args) (k (apply op args))))))
    ((_ lv op argc ...)
     (lambda (k) (k `(_OP ,(pred lv) op ,(argc id) ...))))))

;;; differing from [GJ1995] we need two different lift operators, as
;;; the 2nd argument of _Lift is a number and not some syntactic construct
(define-syntax _lift0
  (syntax-rules ()
    ((_ 0 val)
     (lambda (k) (k 'val)))
    ((_ lv val)
     (lambda (k) (k `(_LIFT0 ,(pred lv) val))))))

(define-syntax _lift
  (syntax-rules ()
    ((_ 0 diff valc)
     (lambda (k) (valc (lambda (val) (k `(_LIFT0 ,(pred diff) ,val))))))
    ((_ lv diff valc)
     (lambda (k) (valc (lambda (val) (k `(_LIFT ,(pred lv) ,diff ,val))))))))

;;; general argument-list processing
;;; accept a list of argument continuations `argsc' and a continuation
;;; `c' and pass the argument list to `c'
(define (lib-arg-list argsc c)
  (if (null? argsc)
      (c '())
      ((car argsc)
       (lambda (arg)
	 (lib-arg-list (cdr argsc)
		       (lambda (args) (c (cons arg args))))))))

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

;;; transform the arguments to CPS computations, if necessary
(define (start-memo-internal level fname fct bts args)
  (clear-residual-program!) 
  (clear-memolist!)
  (gensym-local-reset!)
  (gensym-reset!)
  ((multi-memo level fname fct bts
	       (map (lambda (arg)
		      (if (procedure? arg) arg (result arg))) args))
   (lambda (result)
     (let* ((goal-proc (car *residual-program*))
	    (defn-template (take 2 goal-proc))
	    (defn-body (list-tail goal-proc 2)))
       (set! *residual-program*
	     (list (append defn-template
			   (cdr *residual-program*)
			   defn-body)))
       result))))

;;;
;;; the memo-function
;;; - fn is the name of the function to memoize
;;; - args are the free variables of the function's body
;;; - bts are their binding times
(define (multi-memo level fname fct bts argsc)
  (lambda (kappa)
    (lib-arg-list
     argsc 
     (lambda (args)
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
		      (new-def `(DEFINE (,new-name ,@new-formals)
				  ,((apply fct (map result (cdr cloned-pp))) id))))
		   (add-to-residual-program! new-def)
		   (cons pp new-name))))
	    (res-name (cdr found))
	    (exit-scope (gensym-local-pop!)))
	 ;; (display (list "multi-memo" fname args actuals)) (newline)
	 (if (= level 1)
	     ;; generate call to fn with actual arguments
	     (kappa `(,res-name ,@actuals))
	     ;; reconstruct multi-memo
	     (kappa `(MULTI-MEMO ,(- level 1)
				 ',res-name ,res-name
				 ',(binding-times dynamics)
				 (LIST ,@actuals)))))))))

