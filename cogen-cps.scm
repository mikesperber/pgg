;;; library functions for a multi-level generator using CPS after BTA,
;;; higher-order abstract syntax, and a special function
;;; representation to accomplish proper function memoization in the
;;; presence of dynamic free variables
;;; $Id$
;;; $Log$
;;; Revision 1.1  1995/10/13 16:13:17  thiemann
;;; *** empty log message ***
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

;;; application with arbitrary many arguments
;;; _app : Level \times K Code \times (K Code)^* -> K Code
(define (_app lv fc . argsc)
  (lambda (kappa)
    (fc (lambda (f)
	  (lib-arg-list
	   argsc
	   (lambda (args)
	     (if (= lv 1)
		 (let ((w (gensym 'tmp)))
		   `((,f ,@args)
		     (lambda (,w) ,(kappa w))))
		 (kappa `(_APP ,(- lv 1) ,f ,@args)))))))))

;;; dito for memoized functions
;;; _app_memo : Level \times K Code \times (K Code)^* -> K Code
(define (_app_memo lv fc . argsc)
  (lambda (kappa)
    (fc (lambda (f)
	  (lib-arg-list
	   argsc
	   (lambda (args)
	     (if (= lv 1)
		 (let ((w (gensym 'tmp)))
		   `(((,f 'VALUE) ,@args)
		     (lambda (,w) ,(kappa w))))
		 (kappa `(_APP_MEMO ,(- lv 1) ,f ,@args)))))))))

;;; lambda abstraction with arbitrary arity
;;;
;;; static function representation by a 4-tuple consisting of
;;; (lambda DFV (lambda vars body)), the unique label of the lambda,
;;; the list FV of all free variables, and the binding times of the
;;; free variables. the tuple is represented by a function which maps
;;; 'value, 'label, 'vvs, and 'bts to the respective components of the
;;; tuple. 
;;; [this contortion is not really necessary for ALL lambdas but only
;;; for those that are eventually memoized]
;;; the main problem here lies in the binding times of the free
;;; variables which are independent of the binding times of the
;;; function and the bound variables
;;;
;;; _lambda_memo : Level \times Ident^* \times Label \times 2Value^*
;;; \times BT^* \times (2Value^* -> (K Code)^* -> K Code) -> K Code
(define (_lambda_memo lv arity label vvcs bts f)
  (lambda (kappa)
    (let* ((vars (map gensym arity))
	   (varcs (map result vars)))
      (if (= lv 1)
	  (let* ((fvs (map (lambda (x) (gensym 'fv)) bts))
		 (new-bts (map pred bts)))
	    (kappa `(STATIC-CONSTRUCTOR
		     ',label
		     (LAMBDA ,fvs
		       (LAMBDA ,vars
			 ,((apply f varcs) id)))
		     (LIST ,@fvs)
		     ',bts)))
	  (let* ((dynamics (project-dynamic (cons label vvcs) bts))
		 (new-vvcs (apply append dynamics))
		 (new-bts (binding-times dynamics)))
	    (lib-arg-list
	     new-vvcs
	     (lambda (new-vvs)
	       (kappa `(_LAMBDA_MEMO
			,(- lv 1)
			',arity
			',(gensym label)
			(LIST ,@new-vvs)
			',new-bts
			(LAMBDA ,vars ,((apply f varcs) id)))))))))))

;;; lambdas that are never memoized
;;; i.e., lambdas of continuations that we introduced ourselves are
;;; certainly candidates for these lambdas (does this ever happen?)
;;; remember we live in CPS: variables must ALWAYS be bound to
;;; continuations returning variables!
;;; `arity' is the list of the original variable names
(define (_lambda lv arity f)
  (lambda (kappa)
    (let* ((vars (map gensym arity))
	   (varcs (map result vars)))
      (if (= lv 1)
	  (kappa `(LAMBDA ,vars
		    ,((apply f varcs) id)))
	  (kappa `(_LAMBDA ,(- lv 1) ',arity
			   (LAMBDA ,vars ,((apply f varcs) id))))))))

;;; LET with context propagation
(define (_let lv e f)
  (lambda (kappa)
    (let* ((var (gensym 'fresh))
	   (varc (lambda (k) (k var))))
      (e
       (lambda (value)
	 (if (= lv 1)
	     (if (pair? value)
		 `(,value (LAMBDA (,var)
			    ,((f varc) kappa)))
		 ;; `(LET ((,var ,value)) ,((f varc) kappa))
		 ((f (lambda (k) (k value))) kappa))
	     `(_LET ,(- lv 1) ,value
		    (LAMBDA (,var) ,((f varc) kappa)))))))))

;;; constructors with memoization
(define (_ctor_memo lv bts ctor . argsc)
  (lambda (kappa)
    (lib-arg-list
     argsc
     (lambda (args)
       (let ((bts (map pred bts)))
       (if (= lv 1)
	   (kappa
	    `(STATIC-CONSTRUCTOR ',ctor ,ctor (LIST ,@args) ',bts))
	   (kappa
	    `(_CTOR_MEMO ,(- lv 1)
		    ',bts
		    ',ctor ,@args))))))))

;;; selectors for constructors with memoization
(define (_sel_memo lv sel argc)
  (lambda (kappa)
    (argc
     (lambda (v)
       (if (= lv 1)
	   (kappa `(,sel (,v 'VALUE)))
	   (kappa `(_SEL_MEMO ,(- lv 1) ',sel ,v)))))))

;;; test for constructors with memoization
(define (_test_memo level ctor arg)
  (lambda (kappa)
    (arg
     (lambda (v)
       (if (= level 1)
	   (let ((ctor-test (ctors-make-test ctor)))
	     (kappa ` (,ctor-test (,v 'VALUE))))
	   (kappa `(_TEST_MEMO ,(- level 1) ',ctor ,v)))))))

;;; conditional
(define (_If level e1c e2c e3c)
  (lambda (kappa)
    (e1c (lambda (e1)
	  (if (= level 1)
	      `(IF ,e1 ,(e2c kappa) ,(e3c kappa))
	      `(_IF ,(- level 1) ,e1 ,(e2c kappa) ,(e3c kappa)))))))

;;; primitive operators
(define (_Op level op . argsc)
  (lambda (kappa)
    (lib-arg-list
     argsc
     (lambda (args)
       (if (= level 1)
	   (kappa `(,op ,@args))
	   (kappa `(_OP ,(- level 1) ',op ,@args)))))))

;;; differing from [GJ1995] we need two different lift operators, as
;;; the 2nd argument of _Lift is a number and not some syntactic construct
(define (_Lift0 level val)
  (lambda (kappa)
    (if (= level 1)
	(kappa `(QUOTE ,val))
	(kappa `(_LIFT0 ,(- level 1) ',val)))))
(define (_Liftc level val)
  (lambda (kappa)
    (val (lambda (val)
    (if (= level 1)
	(kappa `(QUOTE ,val))
	(kappa `(_LIFT0 ,(- level 1) ',val)))))))
(define (_Lift level diff valc)
  (lambda (kappa)
    (valc
     (lambda (value)
       (if (= level 1)
	   (kappa `(_LIFTc ,diff ,value))
	   (kappa `(_LIFT ,(- level 1) ,diff ,value)))))))

;;; code that must be executed at run time of a static lambda or
;;; constructor with dynamic free variables
;;; improvements:
;;; - specialize this guy wrt `label' and `bts', as well
;;;   as project-static, project-dynamic, and clone-dynamic!
;;; - use delay/force to memoize project-static and project-dynamic
;;;
;;; static-constructor : Ident \times (2Value^* -> (K Code)^* -> K
;;; Code) \times 2Value^* \times BT^* -> Value
(define (static-constructor ctor closed-value vvs bts)
  ;; (let ((closed-value (lambda fvs (lambda (arg) body)))) ...)
  (let ((ctor-vvs (cons ctor vvs)))
    (lambda (what)
      (cond
       ((equal? what 'value)
	(apply closed-value vvs))
       ((equal? what 'static)
	(project-static ctor-vvs bts))
       ((equal? what 'dynamic)
	(project-dynamic ctor-vvs bts))
       ((equal? what 'clone)
	(static-constructor ctor
			    closed-value
			    (cdr (clone-dynamic ctor-vvs bts))
			    bts)))))) 


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

(define (lib-arg-list-bts argsc bts c)
  (if (null? argsc)
      (c '())
      (if (zero? (car bts))
	  (lib-arg-list-bts (cdr argsc) (cdr bts)
			    (lambda (args)
			      (c (cons (car argsc) args))))
	  ((car argsc)
	   (lambda (arg)
	     (lib-arg-list-bts (cdr argsc) (cdr bts)
			       (lambda (args) (c (cons arg args)))))))))

(define (start-memo level fn bts argsc)
  (clear-residual-program!) 
  (clear-memolist!)
  ((multi-memo level fn bts argsc) id))

;;;
;;; the memo-function
;;; - fn is the name of the function to memoize
;;; - args are the free variables of the function's body
;;; - bts are their binding times
(define (multi-memo level fn bts argsc)
  (lambda (kappa)
    (lib-arg-list-bts
     argsc bts
     (lambda (args)
       (let*
	   ((full-pp (cons fn args))
	    (pp (project-static full-pp bts))
	    (dynamics (project-dynamic full-pp bts))
	    (actuals (apply append dynamics))
	    (found
	     (or (assoc pp *memolist*)
		 (let*
		     ((new-name (gensym fn))
		      (cloned-pp (clone-dynamic full-pp bts))
		      (new-formals-c (apply append
					  (project-dynamic cloned-pp
							   bts)))
		      (new-formals (map (lambda (c) (c id)) new-formals-c))
		      (new-entry (add-to-memolist! (cons pp new-name)))
		      (new-def
		       (let ((func (eval fn (interaction-environment)))
			     (kappa (gensym 'kappa)))
			 (if (= level 1)
			     `(DEFINE (,new-name ,@new-formals)
				(LAMBDA (,kappa)
				  ,((apply func (cdr cloned-pp))
				    (lambda (w) `(,kappa ,w)))))
			     `(DEFINE (,new-name ,@new-formals)
				,((apply func (cdr cloned-pp))
				  id))))))
		   (add-to-residual-program! new-def)
		   (cons pp new-name))))
	    (res-name (cdr found)))
	 (let ((w (gensym 'w)))
	   (if (= level 1)
	     ;; generate call to fn with actual arguments
	       `((,res-name ,@actuals)
		 (LAMBDA (,w) ,(kappa w)))
	     ;; reconstruct multi-memo
	      (kappa `(MULTI-MEMO ,(- level 1)
			   ',res-name
			   ',(binding-times dynamics)
			   (LIST ,@actuals))))))))))

