;;; library functions for a multi-level generator using CPS after BTA,
;;; higher-order abstract syntax, and a special function
;;; representation to accomplish proper function memoization in the
;;; presence of dynamic free variables
;;; $Id$
;;; $Log$
;;; Revision 1.2  1995/10/23 16:52:50  thiemann
;;; continuation based reduction works
;;;
;;; Revision 1.1  1995/10/13  16:13:17  thiemann
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
  (lambda (kappa)
    (lib-arg-list
     argsc
     (lambda (args)
       (let ((bts (map pred bts)))
	 (if (zero? lv)
	     (kappa
	      (STATIC-CONSTRUCTOR ctor
				  (eval ctor (interaction-environment))
				  args
				  bts))
	     (kappa
	      `(_CTOR_MEMO ,(- lv 1) ',bts ',ctor ,@args))))))))

;;; selectors for constructors with memoization
(define (_sel_memo lv sel argc)
  (lambda (kappa)
    (argc
     (lambda (v)
       (kappa
	(cond
	 ((zero? lv)
	  (sel (v 'VALUE)))
	 ((= lv 1)
	  `(_SEL_MEMO ,(- lv 1)  ,sel ,v))
	 (else
	  `(_SEL_MEMO ,(- lv 1) ',sel ,v))))))))

;;; test for constructors with memoization
(define (_test_memo level ctor-test arg)
  (lambda (kappa)
    (arg
     (lambda (v)
       (kappa
	(cond
	 ((zero? level)
	  (ctor-test (v 'value)))
	 ((= level 1)
	  `(_TEST_MEMO ,(- level 1) ,ctor-test ,v))
	 (else
	  `(_TEST_MEMO ,(- level 1) ',ctor-test ,v))))))))

;;; conditional
(define (_If level e1c e2c e3c)
  (lambda (kappa)
    (e1c (lambda (e1)
	  (if (zero? level)
	      (if e1 (e2c kappa) (e3c kappa))
	      `(_IF ,(- level 1) ,e1 ,(e2c kappa) ,(e3c kappa)))))))

;;; primitive operators
(define (_Op level op . argsc)
  (lambda (kappa)
    (lib-arg-list
     argsc
     (lambda (args)
       (kappa
	(cond
	 ((zero? level)
	  (apply op args))
	 ((= 1 level)
	  `(_OP 0 ,op ,@args))
	 (else
	  `(_OP ,(- level 1) ',op ,@args))))))))

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

