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
(define (make-ge-const c)
  `',c)
(define (make-ge-cond l lb c t e)
  `(_IF ,l ,lb ,c ,t ,e))
(define (make-ge-op l o args)
  `(_OP ,l ,o ,@args))
(define (make-ge-call f bts args)
  `(,f ,@args))
(define (make-ge-let hl unf? bl v e body)
  `(LET ((,v ,e)) ,body))
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
(define (make-ge-make-cell-memo l label bt a)
  `(_MAKE-CELL_MEMO ,l ,label ,bt ,a))
(define (make-ge-cell-ref-memo l a)
  `(_S_T_MEMO ,l CELL-REF ,a))
(define (make-ge-cell-set!-memo l r a)
  `(_CELL-SET!_MEMO ,l ,r ,a))
(define (make-ge-cell-eq?-memo l args)
  `(_CELL-EQ?_MEMO ,l ,@args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toplevel projection functions
(define (top-project-static value bts)
  (address-registry-reset!)
  (project-static value bts))
(define (top-project-dynamic value bts)
  (address-registry-reset!)
  (project-dynamic value bts))
(define (top-clone-dynamic value bts)
  (address-registry-reset!)
  (address-map-reset!)
  (clone-dynamic value bts))
(define (top-clone-with clone-map value bts)
  (address-registry-reset!)
  (address-map-reset!)
  (clone-with clone-map value bts))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an implementation using macros

(define-syntax _complete
  (syntax-rules ()
    ((_ body)
     (let ((var (gensym-local 'mlet)))
       (shift k (make-residual-let-trivial var body (k var)))))))

(define-syntax _complete-serious
  (syntax-rules ()
    ((_ body)
     (let ((var (gensym-local 'mlet)))
       (shift k (make-residual-let-serious var body (k var)))))))

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
    ((_app_memo lv e ...)
     (_complete `(_APP_MEMO ,(pred lv) ,e ...)))))

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
	`(LAMBDA ,vars ,body)
	(make-ge-lambda (pred lv) vars #f body))))

(define-syntax _lambda_memo
  (syntax-rules ()
    ((_ 0 arity label vvs bts f)
     (static-constructor label f vvs bts))
    ((_ arg ...)
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
    ((_ lv (bt ...) ctor arg ...)
     (_complete `(_CTOR_MEMO ,(pred lv) (,(pred bt) ...) ctor ,arg ...)))))

(define-syntax _s_t_memo
  (syntax-rules ()
    ((_ 0 sel v)
     (sel (v 'VALUE)))
    ((_sel_memo lv sel v)
     (_complete `(_S_T_MEMO ,(pred lv) sel ,v)))))

(define-syntax _make-cell_memo
  (syntax-rules ()
    ((_ 0 lab bt arg)
     (static-cell lab arg bt))
    ((_ lv lab bt arg)
     (_complete `(_MAKE-CELL_MEMO ,(pred lv) lab ,(pred bt) ,arg)))))

(define-syntax _cell-set!_memo
  (syntax-rules ()
    ((_ 0 ref arg)
     (cell-set! (ref 'VALUE) arg))
    ((_ lv ref arg)
     (_complete `(_CELL-SET!_MEMO ,(pred lv) ,ref ,arg)))))

(define-syntax _cell-eq?_memo
  (syntax-rules ()
    ((_ 0 ref1 ref2)
     (eq? (ref1 'VALUE) (ref2 'VALUE)))
    ((_ lv ref1 ref2)
     (_complete `(_CELL-EQ?_MEMO ,(pred lv) ,ref1 ,ref2)))))

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
  (syntax-rules (apply cons _define_data)
    ((_op lv _define_data arg)
     (make-residual-define-data lv arg))
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
  (let* ((initial-scope (gensym-local-push!))
	 (result (reset (multi-memo level fname fct bts args)))
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
	(_complete-serious
	 (apply make-residual-call res-name actuals))
	;; reconstruct multi-memo
	(_complete-serious
	 (make-residual-call 'MULTI-MEMO
			     (- level 1)
			     `',res-name
			     res-name
			     `',(binding-times dynamics)
			     `(LIST ,@actuals))))))
