;;; $Id $
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;; using introspective operators
;;;

;;; interface to create generating extensions
;;; syntax constructors
(define binding-rtd '(binding))
(define (make-binding value btime) (list binding-rtd value btime))
(define (binding? b) (and (pair? b) (eq? (car b) binding-rtd)))
(define (binding->value b) (cadr b))
(define (binding->btime b) (caddr b))

(define (make-var-binding value btime)
  (make-binding `(_VAR ,value) btime))

(define (make-ge-var l v)
  `(_VAR ,v))
(define (make-ge-const c)
  `',c)
(define (make-ge-cond l lb c t e)
  `(_IF ,l ,c ,t ,e))
(define (make-ge-op l o args)
  `(_OP ,l ,o ,@args))
(define (make-ge-call f bts args)
  `(_APP 0 ,f ,bts ,@args))
(define (make-ge-let l unf? prop? v e body)
  `(_LET ,l ,unf? ,prop? ',v ,e (LAMBDA (,v) ,body)))
(define (make-ge-begin l prop? e1 e2)
  `(_BEGIN ,l ,prop? ,e1 ,e2))
(define (make-ge-lambda-memo l vars btv label fvars bts body)
  `(_LAMBDA ,l ',vars ',btv (LAMBDA ,vars ,body)))
(define (make-ge-vlambda-memo l fixed-vars var btv label fvars bts body)
  `(_VLAMBDA_MEMO ,l ',fixed-vars ',var ',btv ',label (LIST ,@fvars) ',bts
		 (LAMBDA ,fvars (LAMBDA ,(append fixed-vars var) ,body))))
(define (make-ge-app-memo l f btv args)
  `(_APP ,l ,f ,btv ,@args))
(define (make-ge-lambda l vars btv body)
  `(_LAMBDA ,l ',vars ',btv (LAMBDA ,vars ,body)))
(define (make-ge-app l f btv args)
  `(_APP ,l ,f ,btv ,@args))
(define (make-ge-ctor-memo l bts ctor args)
  `(_CTOR_MEMO ,l ,bts ,ctor ,@args))
(define (make-ge-sel-memo l sel a)
  `(_SEL_MEMO ,l ,sel ,a))
(define (make-ge-test-memo l tst a)
  `(_OP ,l ,tst ,a))
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

;;; cogen functions
(define-syntax _var
  (syntax-rules ()
    ((_ v)
     (binding->value v))))

(define-syntax _app
  (syntax-rules ()
    ((_app 0 f (bt ...) arg ...)
     (f (make-binding arg bt) ...))
    ((_app lv f (bt ...) arg ...)
     `(_APP ,(pred lv) ,(reset f) (,(pred bt) ...) ,(reset arg) ...))))

(define-syntax _lambda
  (syntax-rules ()
    ((_ 0 vars bts f)
     f)
    ;;((lambda vv (apply f (map make-binding vv bts))))
    ((_ bla ...)
     (_lambda-internal bla ...))))

(define (_lambda-internal lv arity btv f)
  (let* ((vars (map gensym-local arity))
	 (fun `(LAMBDA ,vars
		 ,(reset (apply f (map make-var-binding vars btv))))))
    (if (= lv 1)
	fun
	`(_LAMBDA ,(pred lv) ',arity ,(map pred btv) ,fun))))

(define-syntax _lambda_memo
  (syntax-rules ()
    ((_ lv arity btv labal vvs bts f)
     (_lambda lv arity btv f))))

(define (_let lv unf? prop? orig-var e f)
  (let ((var (gensym-local orig-var)))
    (cond
     ((zero? lv)
      (f (make-binding e lv)))
     ((= lv 1)
      (if (or unf? (not (pair? e)) (equal? 'QUOTE (car e)))
	  (f (make-binding e lv))
	  (shift k `(LET ((,var ,e))
		      ,(reset (k (f (make-var-binding var lv))))))))
     (else
      (shift k
	     `(_LET ,(pred lv) ,unf? ,prop? ',orig-var
		    ,e (LAMBDA (,var)
			 ,(reset (k (f (make-var-binding var lv)))))))))))

(define-syntax _begin
  (syntax-rules ()
    ((_ 0 p? e1 e2)
     (begin e1 e2))
    ((_ 1 p? e1 e2)
     (shift k (make-residual-begin (reset e1) (reset (k e2)))))
    ((_ lv p? e1 e2)
     (shift k `(_BEGIN ,(pred lv) ,p? ,(reset e1) ,(reset (k e2)))))))

(define-syntax _ctor_memo
  (syntax-rules ()
    ((_ 0 (bt ...) ctor arg ...)
     (ctor (make-binding arg bt) ...))
    ((_ lv (bt ...) ctor arg ...)
     `(_CTOR_MEMO ,(pred lv) (,(pred bt) ...) ctor ,(reset arg) ...))))


(define-syntax _sel_memo
  (syntax-rules ()
    ((_ 0 sel v)
     (binding->value (sel v)))
    ((_sel_memo lv sel v)
     `(_SEL_MEMO ,(pred lv) sel ,(reset v)))))

(define-syntax _if
  (syntax-rules ()
    ((_if 0 e1 e2 e3)
     (if e1 e2 e3))
    ((_if 1 e1 e2 e3)
     (shift k (make-residual-if (reset e1) (reset (k e2)) (reset (k e3)))))
    ((_if lv e1 e2 e3)
     (shift k `(_IF ,(pred lv)
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
     (make-residual-cons e1 e2))
    ((_op 1 apply f arg)
     (make-residual-apply f arg))
    ((_op 1 op arg ...)
     `(op ,(reset arg) ...))
    ((_op lv op arg ...)
     `(_OP ,(pred lv) op ,(reset arg) ...))))

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
  (clear-support-code!)
  (gensym-local-reset!)
  (gensym-reset!)
  (let* ((result (multi-memo level fname fct bts (map make-binding
						      args bts)))
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

;;; don't store the static skeleton, but the entire value
;;; and employ a special comparison function

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
       (actuals (map binding->value (apply append dynamics)))
       (found
	(or (assoc pp *memolist*)
	    (let*
		((new-name (gensym fname))
		 (cloned-pp (clone-dynamic full-pp bts))
		 (new-formals (map binding->value (apply append (project-dynamic cloned-pp bts))))
		 (new-entry (add-to-memolist! (cons pp new-name)))
		 (new-def  `(DEFINE (,new-name ,@new-formals)
			      ,(reset (apply fct (cdr cloned-pp))))))
	      (add-to-residual-program! new-def)
	      (cons pp new-name))))
       (res-name (cdr found))
       (exit-scope (gensym-local-pop!))
       (new-bts (binding-times dynamics)))
    (if (= level 1)
	;; generate call to fn with actual arguments
	(make-ge-call res-name new-bts actuals)
	;; reconstruct multi-memo
	`(MULTI-MEMO ,(- level 1)
		     ',res-name ,res-name
		     ',new-bts
		     (LIST ,@actuals)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (project-dynamic value bt-args)
  (let* ((values-by-bt
	  (project-dynamic-level (cdr value) bt-args 0))
	 (nested-dynamics
	  (let loop ((values (car values-by-bt)))
	    (if (null? values)
		'()
		(let ((value (car values))
		      (dynamics (loop (cdr values))))
		  (cond
		   ((procedure? value)
		    (multi-append (extract-dynamics-proc value) dynamics))
		   ((vector? value)
		    (multi-append (extract-dynamics-vector value) dynamics))
		   (else
		    dynamics)))))))
    (multi-append nested-dynamics (cdr values-by-bt))))

(define (extract-dynamics-proc proc)
  (let* ((env (closure-env proc)))
    (extract-dynamics-vector env)))

(define (extract-dynamics-vector vec)
  (let ((len (vector-length vec)))
    (let loop ((i 0)
	       (c (lambda (vals bts) (project-dynamic (cons #f vals) bts))))
      (if (= i len)
	  (c '() '())
	  (let ((vec-i (vector-ref vec i)))
	    (loop (+ i 1) (lambda (vals bts)
			    (c (cons (binding->value vec-i) vals)
			       (cons (binding->btime vec-i) bts)))))))))

;;; project the static part
(define (project-static pp bts)
  (cons (car pp)
	(let loop ((values (cdr pp)) (bts bts))
	  (if (null? values)
	      '()
	      (let ((rest (loop (cdr values) (cdr bts))))
		(if (zero? (car bts))
		    (cons (project-one-static (car values)) rest)
		    rest))))))

(define (project-one-static value)
  (cond
   ((procedure? value)
    (project-static-procedure value))
   ((vector? value)
    (project-static-vector value))
   ((binding? value)
    (and (zero? (binding->btime value))
	 (project-one-static (binding->value value))))
   (else
    value)))

(define (project-static-procedure proc)
  (let* ((env (closure-env proc))
	 (static-env (and env (project-static-vector env)))
	 (template (closure-template proc)))
    (cons template static-env)))

(define (project-static-vector vec)
  (let* ((len (vector-length vec))
	 (static-vec (make-vector len)))
    (let loop ((i 0))
      (if (= i len)
	  static-vec
	  (begin
	    (vector-set! static-vec i
			 (project-one-static (vector-ref vec i)))
	    (loop (+ i 1)))))))

;;; cloning
(define (clone-one-dynamic value bt)
  (make-binding
   (if (= 0 bt)
       (cond
	((procedure? value)
	 (clone-procedure value))
	((vector? value)
	 (clone-vector value))
	(else
	 value))
       (if (symbol? value)
	   (gensym-local value)
	   (gensym-local 'clone)))
   bt))

(define (clone-dynamic value bts)
  (cons (car value)
	(let loop ((values (cdr value)) (bts bts))
	  (if (null? values)
	      '()
	      (let* ((value (car values))
		     (bt (car bts))
		     (cloned (loop (cdr values) (cdr bts)))
		     (new-value (clone-one-dynamic (binding->value value) bt)))
		(cons new-value cloned))))))

(define (clone-entity val)
  (cond
   ((binding? val)
    (clone-one-dynamic (binding->value val) (binding->btime val)))
   ((vector? val)
    (clone-vector val))
   ((procedure? val)
    (clone-procedure val))
   ((pair? val)
    (cons (clone-entity (car val))
	  (clone-entity (cdr val))))
   (else
    val)))

(define (clone-vector vec)
  (let* ((len (vector-length vec))
	 (new-vec (make-vector len)))
    (vector-set! new-vec 0 (vector-ref vec 0))
    (let loop ((i 1))
      (if (= i len)
	  new-vec
	  (let* ((vec-i (vector-ref vec i)))
	    (vector-set! new-vec i (clone-entity vec-i))
	    (loop (+ i 1)))))))

(define (clone-procedure proc)
  (let* ((env (closure-env proc))
	 (cloned-env (and env (clone-vector env)))
	 (template (closure-template proc)))
    (make-closure template cloned-env)))

;;; memo-lookup
(define memo-lookup
  (lambda (key1)
    (let loop ((memolist *memolist*))
      (if (null? memolist)
	  #f
	  (let* ((key-value (car memolist))
		 (key (car key-value)))
	    (if (memo-equal? key key1)
		key-value
		(loop (cdr memolist))))))))

;;; need to recognize and treat constructors
(define memo-equal?
  (lambda (key1 key2)
    (or (eq? key1 key2)
	(cond
	 ((and (procedure? key1) (procedure? key2))
	  (and (eq? (closure-template key1) (closure-template key2))
	       (memo-equal? (closure-env key1) (closure-env key2))))
	 ((and (vector? key1) (vector? key2))
	  (let ((l1 (vector-length key1)) (l2 (vector-length key2)))
	    (and (eq? l1 l2)
		 (let loop ((i 0))
		   (if (= i l1)
		       #t
		       (and (memo-equal? (vector-ref key1 i)
					 (vector-ref key2 i))
			    (loop (+ i 1))))))))
	 ((and (binding? key1) (binding? key2))
	  (memo-static-equal? key1 key2))
	 ((and (pair? key1) (pair? key2))
	  (and (memo-equal? (car key1) (car key2))
	       (memo-equal? (cdr key1) (cdr key2))))
	 ((string? key1)
	  (equal? key1 key2))
	 (else
	  #f)))))

(define memo-static-equal?
  (lambda (bind1 bind2)
    (or (< 0 (binding->btime bind1))
	(memo-equal? (binding->value bind1)
		     (binding->value bind2)))))
