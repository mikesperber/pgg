;;; $Id$
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;;

;;; interface to create generating extensions
;;; syntax constructors
(define (make-thunk arg) `(LAMBDA () ,arg))
(define (run-thunk t) (t))
(define (reset-thunk t) (reset (t)))

(define (make-ge-var l v)
  v)
(define (make-ge-const c)
  `',c)
(define (make-ge-cond l c t e)
  `(_IF ,l ,(make-thunk c) ,(make-thunk t) ,(make-thunk e)))
(define (make-ge-op l o args)
  `(_OP ,l ',o ,@(map make-thunk args)))
(define (make-ge-call f args)
  `(,f ,@args))
(define (make-ge-let l v e body)
  `(_LET ,l ',v ,e (LAMBDA (,v) ,body)))
(define (make-ge-begin l e1 e2)
  `(_BEGIN ,l ,e1 ,(make-thunk e2)))
(define (make-ge-lambda-memo l vars btv label fvars bts body)
  `(_LAMBDA_MEMO ',l ',vars ',label (LIST ,@fvars) ',bts
		 (LAMBDA ,fvars (LAMBDA ,vars ,body))))
(define (make-ge-vlambda-memo l fixed-vars var btv label fvars bts body)
  `(_VLAMBDA_MEMO ',l ',fixed-vars ',var ',label (LIST ,@fvars) ',bts
		 (LAMBDA ,fvars (LAMBDA (,@fixed-vars . ,var) ,body))))
(define (make-ge-app-memo l f btv args)
  `(_APP_MEMO ,l ,(make-thunk f) ,@(map make-thunk args)))
(define (make-ge-lambda l vars btv body)
  `(_LAMBDA ',l ',vars (LAMBDA ,vars ,body)))
(define (make-ge-app l f btv args)
  `(_APP ,l ,(make-thunk f) ,@(map make-thunk args)))
(define (make-ge-ctor-memo l bts ctor args)
  `(_CTOR_MEMO ,l ',bts ',ctor ,@args))
(define (make-ge-sel-memo l sel a)
  `(_S_T_MEMO ,l ',sel ,a))
(define (make-ge-test-memo l tst a)
  `(_S_T_MEMO ,l ',tst ,a))
(define (make-ge-ctor l ctor args)
  `(_OP ,l ',ctor ,@args))
(define (make-ge-sel l sel a)
  `(_OP ,l ',sel ,a))
(define (make-ge-test l tst a)
  `(_OP ,l ',tst ,a))
(define (make-ge-lift l diff a)
  `(_LIFT ,l ,diff ,a))
(define (make-ge-eval l diff a)
  `(_EVAL ,l ,diff ,a))

;;; cogen functions
(define (_app lv f . args)
  (let ((args (map reset-thunk args))
	(f (reset-thunk f)))
    (if (= lv 1)
	`(,f ,@args)
	(make-ge-app (pred lv) f '() args)))) 

(define (_app_memo lv f . args)
  (let ((args (map reset-thunk args))
	(f (reset-thunk f)))
  (if (= lv 1)
      `((,f 'VALUE) ,@args)
      (make-ge-app-memo (pred lv) f '() args))))

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

(define (_begin lv e1 e2)
  (cond
   ((zero? lv)
    (e2))
   ((= lv 1)
    (shift k (make-residual-begin e1 (reset (k (e2))))))
   (else
    (shift k `(_BEGIN ,(pred lv)
		      ,e1 (LAMBDA ()
			    ,(reset (k (e2)))))))))

(define (_ctor_memo lv bts ctor . args)
  (let ((new-bts (map pred bts)))
    (if (= lv 1)
	`(STATIC-CONSTRUCTOR ',ctor ,ctor (LIST ,@args) ',new-bts)
	(make-ge-ctor-memo (- lv 1) new-bts ctor args))))

(define (_s_t_memo lv sel/test v)
  (if (= lv 1)
      `(,sel/test (,v 'VALUE))
      (make-ge-sel-memo (- lv 1) sel/test v)))

;;; needs RESET, somewhere
;;; therefore: the arms of the conditional must be thunks, so that we
;;; can capture control. we get this for free in the CPS version where
;;; et2 and et3 are continuations, anyway
(define (_If level et1 et2 et3)
  (if (= level 1)
      (shift k `(IF ,(reset-thunk et1) ,(reset (k (et2))) ,(reset (k (et3)))))
      (shift k (make-ge-cond (- level 1)
			     (reset-thunk et1)
			     (reset (k (et2)))
			     (reset (k (et3)))))))

;;; alternative implementation
(define (_If1 level et1 et2 et3)
  (if (= level 0)
      (if (et1) (et2) (et3))
      (shift k `(_IF1 ,(- level 1) ,(reset-thunk et1)
		      (LAMBDA () ,(reset (k (et2))))
		      (LAMBDA () ,(reset (k (et3))))))))

(define (_Op level op . args)
  (let ((args (map reset-thunk args)))
    (cond
     ((eq? op '_DEFINE_DATA)
      (make-residual-define-data level (car args)))
     ((= level 1)
      (cond
       ((eq? op 'cons)
	(make-residual-cons (car args) (cadr args)))
       ((eq? op 'apply)
	(make-residual-apply (car args) (cadr args)))
       (else
	`(,op ,@args))))
     (else
      (make-ge-op (pred level) op args)))))

(define (_Lift0 level val)
  (if (= level 1)
      (if (or (number? val) (string? val) (boolean? val))
	  val
	  `(QUOTE ,val))
      `(_LIFT0 ,(- level 1) ',val)))

(define (_Lift level diff value)
  (cond
   ((zero? level)
    (_LIFT0 diff value))
   ((= level 1)
    `(_LIFT0 ,diff ,value))
   (else
    `(_LIFT ,(- level 1) ,diff ,value))))

(define (_Eval level diff body)
  (cond
   ((= level 0)
    (cond
     ((zero? diff)
      (eval body (interaction-environment)))
     ((= 1 diff)
      body)
     (else
      `(_EVAL 0 ,(- diff 1) ',body))))
   ((= level 1)
    (cond
     ((zero? diff)
      `(EVAL ,body (INTERACTION-ENVIRONMENT)))
     ((= 1 diff)
      body)
     (else
      `(_EVAL 0 ,diff ',body))))
   (else
    `(_EVAL ,(- level 1) ,diff ,body))))

;;; memo function stuff
(define (start-memo level fn bts args)
  (clear-residual-program!) 
  (clear-memolist!)
  (gensym-local-reset!)
  (multi-memo level fn bts args))

;;; the memo-function
;;; - fn is the name of the function to memoize
;;; - args are the free variables of the function's body
;;; - bts are their binding times
(define (multi-memo level fn bts args)
  (let*
      ((enter-scope (gensym-local-push!))
       (full-pp (cons fn args))
       (pp (project-static full-pp bts))
       (dynamics (project-dynamic full-pp bts))
       (actuals (apply append dynamics))
       (found
	(or (assoc pp *memolist*)
	    (let*
		((new-name (gensym fn))
		 (cloned-pp (clone-dynamic full-pp bts))
		 (new-formals (apply append (project-dynamic cloned-pp bts)))
		 (new-entry (add-to-memolist! (cons pp new-name)))
		 (new-def  `(DEFINE (,new-name ,@new-formals)
			      ,(reset (apply (eval fn (interaction-environment))
					     (cdr cloned-pp))))))
	      (add-to-residual-program! new-def)
	      (cons pp new-name))))
       (res-name (cdr found))
       (exit-scope (gensym-local-pop!)))
    (if (= level 1)
	;; generate call to fn with actual arguments
	`(,res-name ,@actuals)
	;; reconstruct multi-memo
	`(MULTI-MEMO ,(- level 1)
		     ',res-name
		     ',(binding-times dynamics)
		     (LIST ,@actuals)))))

