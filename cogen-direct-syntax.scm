;;; $Id$
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;;

(define-syntax old-reset
  (syntax-rules ()
    ((_ e) (reset e))))

(define-syntax new-reset
  (syntax-rules ()
    ((_ e) e)))

;;; interface to create generating extensions
;;; syntax constructors
(define (make-ge-var l v)
  v)
(define (make-ge-const c)
  `',c)
(define (make-ge-cond l lb c t e)
  `(_IF ,l ,lb ,c ,t ,e))
(define (make-ge-op l o args)
  `(_OP ,l ,o ,@args))
(define (make-ge-call f bts args)
  `(,f ,@args))
(define (make-ge-let hl unf? bl v e body)
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

(define-syntax _app
  (syntax-rules ()
    ((_app 0 e ...)
     (e ...))
    ((_app 1 e ...)
     `(,(old-reset e) ...))
    ((_app lv e ...)
     `(_APP ,(pred lv) ,(old-reset e) ...))))

(define-syntax _app_memo
  (syntax-rules ()
    ((_app_memo 0 f arg ...)
     ((f 'VALUE) arg ...))
    ((_app_memo lv e ...)
     `(_APP_MEMO ,(pred lv) ,(old-reset e) ...))))

(define-syntax _lambda
  (syntax-rules ()
    ((_lambda 0 vars body)
     (lambda vars body))
    ((_lambda lv vars body)
     (_lambda-internal lv 'vars (lambda vars body)))))

(define (_lambda-internal lv arity f)
  (let* ((vars (map gensym-local arity))
	 (body (old-reset (apply f vars))))
    (if (= lv 1)
	`(LAMBDA ,vars ,body)
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
    `(_LAMBDA_MEMO
      ,(- lv 1)
      ',arity
      ',(gensym 'cls)
      (LIST ,@actual-fvs)
      ',new-bts
      (LAMBDA ,formal-fvs
	(LAMBDA ,formals
	  ,(old-reset (apply (apply f cloned-vvs) formals)))))))

(define (_vlambda lv arity var f)
  (let* ((vars (map gensym-local arity))
	 (vvar (gensym-local var))
	 (fun `(LAMBDA ,(append vars vvar)
		 ,(old-reset (apply f vars))))) ;unclear what to do with vvar
    (if (= lv 1)
	fun
	`(_VLAMBDA ,(pred lv) ',arity ',var ,fun))))

(define-syntax _let
  (syntax-rules ()
    ((_ 0 u? bl ((?v e)) body)
     (let ((?v e)) body))
    ((_ 1 u? bl ((?v e)) body)
     (_let-internal-1 u? bl '?v (old-reset e) (lambda (?v) body)))
    ((_ lv u? bl ((?v e)) body)
     (_let-internal lv u? bl '?v (old-reset e) (lambda (?v) body)))))

(define (_let-internal-1 unf? bl orig-var e f)
  (let ((var (gensym-local orig-var)))
    (cond
     ((or unf? (not (pair? e)) (equal? 'QUOTE (car e)))
      (f e))
     (else
      (shift k (make-residual-let var e (reset (k (f var)))))))))

(define (_let-internal lv unf? bl orig-var e f)
  (let ((var (gensym-local orig-var)))
	(shift k
	       (make-ge-let (pred lv) unf? bl var e (reset (k (f var)))))))

(define-syntax _begin
  (syntax-rules ()
    ((_ 0 bl e1 e2)
     (begin e1 e2))
    ((_ 1 bl e1 e2)
     (shift k (make-residual-begin (old-reset e1) (reset (k e2)))))
    ((_ lv bl e1 e2)
     (shift k `(_BEGIN ,(pred lv) 0 ,(old-reset e1) ,(reset (k e2)))))))

(define-syntax _ctor_memo
  (syntax-rules ()
    ((_ 0 bts ctor arg ...)
     (static-constructor 'ctor ctor (list arg ...) 'bts))
    ((_ lv (bt ...) ctor arg ...)
     `(_CTOR_MEMO ,(pred lv) (,(pred bt) ...) ctor ,(old-reset arg) ...))))

(define-syntax _s_t_memo
  (syntax-rules ()
    ((_ 0 sel v)
     (sel (v 'VALUE)))
    ((_sel_memo lv sel v)
     `(_S_T_MEMO ,(pred lv) sel ,(old-reset v)))))

(define-syntax _if
  (syntax-rules ()
    ((_if 0 lb e1 e2 e3)
     (if e1 e2 e3))
    ((_if 1 bl e1 e2 e3)
     (shift k (make-residual-if (reset e1) (reset (k e2)) (reset (k e3)))))
    ((_if lv bl e1 e2 e3)
     (shift k `(_IF ,(pred lv) 0
		    ,(reset e1)
		    ,(reset (k e2))
		    ,(reset (k e3)))))))

(define-syntax _op
  (syntax-rules (apply cons _define_data)
    ((_op lv _define_data arg)
     (make-residual-define-data lv arg))
    ((_op 0 op arg ...)
     (op arg ...))
    ((_op 1 cons e1 e2)
     (make-residual-cons (old-reset e1) (old-reset e2)))
    ((_op 1 apply f arg)
     (make-residual-apply (old-reset f) (old-reset arg)))
    ((_op 1 op arg ...)
     `(op ,(old-reset arg) ...))
    ((_op lv op arg ...)
     `(_OP ,(pred lv) op ,(old-reset arg) ...))))

(define-syntax _lift0
  (syntax-rules ()
    ((_ 1 val)
     (if (or (number? val) (string? val) (boolean? val))
	 val
	 `(QUOTE ,val)))
    ((_ lv val)
     (new-reset `(_LIFT0 ,(pred lv) ',val)))))

(define-syntax _lift
  (syntax-rules ()
    ((_ 0 diff value)
     (_lift0 diff value))
    ((_ 1 diff value)
     (new-reset `(_LIFT0 ,diff ,value)))
    ((_ lv diff value)
     (new-reset `(_LIFT ,(pred lv) ,diff ,value)))))

(define-syntax _eval
  (syntax-rules ()
    ((_ 0 0 body)
     (eval body (interaction-environment)))
    ((_ 0 1 body)
     body)
    ((_ 0 diff body)
     (new-reset `(_EVAL 0 ,(pred diff) ',body)))
    ((_ 1 0 body)
     (new-reset `(EVAL ,body (INTERACTION-ENVIRONMENT))))
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
  (clear-support-code!)
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
  (let*
      ((enter-scope (gensym-local-push!))
       (full-pp (cons fname args))
       (pp (top-project-static full-pp bts))
       (dynamics (top-project-dynamic full-pp bts))
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
		 (cloned-pp (top-clone-dynamic full-pp bts))
		 ; (new-formals (map cdr clone-map))
		 (new-formals (apply append (top-project-dynamic cloned-pp bts)))
		 (new-entry (add-to-memolist! (cons pp new-name)))
		 (new-def  (make-residual-definition! new-name
						      new-formals
						      (reset (apply fct (cdr cloned-pp))))))
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

