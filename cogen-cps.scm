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
;;; set creation of conditional arms
(define (make-conditional-arm e) e)

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
      (let* ((dynamics (project-dynamic (cons label vvcs) bts))
	     (new-vvcs (apply append dynamics))
	     (new-bts (binding-times dynamics))
	     (fvs (map (lambda (x) (gensym 'fv)) bts))
	     (fvcs (map result fvs)))
	(lib-arg-list
	 new-vvcs
	 (lambda (new-vvs)
	   (if (= lv 1)
	       (kappa `(STATIC-CONSTRUCTOR
			',label
			(LAMBDA ,fvs
			  (LAMBDA ,vars
			    ,((apply (apply f fvcs) varcs) id)))
			(LIST ,@new-vvs)
			',new-bts))
	       ;; > lv 1
	       (kappa `(_LAMBDA_MEMO
			,(- lv 1)
			',arity
			',(gensym label)
			(LIST ,@new-vvs)
			',new-bts
			(LAMBDA ,fvs
			  (LAMBDA ,vars
			    ,((apply (apply f fvcs) varcs) id))))))))))))

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
	 (cond
	  ((or (zero? lv)
	       (and (= lv 1) (not (pair? value))))
	   ((f (result value)) kappa))
	  (else
	   `(_LET ,(- lv 1) ,value
		  (LAMBDA (,var) ,((f varc) kappa))))))))))

;;; constructors with memoization
(define (_ctor_memo lv bts ctor . argsc)
  (let ((bts (map pred bts)))
    (if (zero? lv)
	(lambda (kappa)
	  (lib-arg-list
	   argsc
	   (lambda (args)
	     (kappa
	      (STATIC-CONSTRUCTOR ctor
				  (eval ctor (interaction-environment))
				  args
				  bts)))))
	(lambda (kappa)
	  (kappa
	   `(_CTOR_MEMO ,(- lv 1) ',bts ',ctor
			,@(map (lambda (ac) (ac id)) argsc)))))))

;;; selectors for constructors with memoization
(define (_sel_memo lv sel argc)
  (if (zero? lv)
      (lambda (kappa)
	(argc
	 (lambda (v)
	   (kappa (sel (v 'VALUE))))))
      (lambda (kappa)
	(kappa
	 (cond
	  ((= lv 1)
	   `(_SEL_MEMO ,(- lv 1)  ,sel ,(argc id)))
	  (else
	   `(_SEL_MEMO ,(- lv 1) ',sel ,(argc id))))))))

;;; test for constructors with memoization
(define (_test_memo level ctor-test argc)
  (if (zero? level)
      (lambda (kappa)
	(argc
	 (lambda (v)
	   (kappa (ctor-test (v 'value))))))
      (lambda (kappa)
	(kappa
	 (cond
	  ((= level 1)
	   `(_TEST_MEMO ,(- level 1) ,ctor-test ,(argc id)))
	  (else
	   `(_TEST_MEMO ,(- level 1) ',ctor-test ,(argc id))))))))

;;; conditional
(define (_If level e1c e2c e3c)
  (lambda (kappa)
    (e1c (lambda (e1)
	  (if (zero? level)
	      (if e1 (e2c kappa) (e3c kappa))
	      `(_IF ,(- level 1) ,e1 ,(e2c kappa) ,(e3c kappa)))))))

;;; primitive operators
(define (_Op level op . argsc)
  (if (zero? level)
      (lambda (kappa)
	(lib-arg-list
	 argsc
	 (lambda (args)
	   (kappa (apply op args)))))
      (lambda (kappa)
	(kappa
	 (cond
	  ((= 1 level)
	   `(_OP 0 ,op ,@(map (lambda (ac) (ac id)) argsc)))
	  (else
	   `(_OP ,(- level 1) ',op ,@(map (lambda (ac) (ac id)) argsc))))))))

;;; differing from [GJ1995] we need two different lift operators, as
;;; the 2nd argument of _Lift is a number and not some syntactic construct
(define (_Lift0 level val)
  (lambda (kappa)
    (if (= level 0)
	(kappa val)
	(kappa `(_LIFT0 ,(- level 1) ',val)))))

(define (_Liftc level valc)
  (lambda (kappa)
    (if (= level 0)
	(kappa valc)
	(valc (lambda (val)
	       (kappa `(_LIFT0 ,(- level 1) ',val)))))))

(define (_Lift level diff valc)
  (lambda (kappa)
    (valc
     (lambda (value)
       (if (= level 1)
	   (kappa `(_LIFTc ,diff ,value))
	   (kappa `(_LIFT ,(- level 1) ,diff ,value)))))))

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

;;; no longer used
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

;;; transform the arguments to CPS, if necessary
(define (start-memo level fn bts args)
  (clear-residual-program!) 
  (clear-memolist!)
  ((multi-memo level fn bts
	       (map (lambda (arg)
		      (if (procedure? arg) arg (result arg))) args))
   id))

;;;
;;; the memo-function
;;; - fn is the name of the function to memoize
;;; - args are the free variables of the function's body
;;; - bts are their binding times
(define (multi-memo level fn bts argsc)
  (lambda (kappa)
    (lib-arg-list
     argsc 
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
		      (new-formals (apply append
					  (project-dynamic cloned-pp
							   bts)))
		      (new-entry (add-to-memolist! (cons pp new-name)))
		      (new-def
		       (let ((func (eval fn (interaction-environment)))
			     (kappa (gensym 'kappa)))
			 `(DEFINE (,new-name ,@new-formals)
			    ,((apply func (map result (cdr cloned-pp)))
			      id)))))
		   (add-to-residual-program! new-def)
		   (cons pp new-name))))
	    (res-name (cdr found)))
	 (let ((w (gensym 'w)))
	   (if (= level 1)
	     ;; generate call to fn with actual arguments
	       (kappa `(,res-name ,@actuals))
	       ;; reconstruct multi-memo
	      (kappa `(MULTI-MEMO ,(- level 1)
			   ',res-name
			   ',(binding-times dynamics)
			   (LIST ,@actuals))))))))))

