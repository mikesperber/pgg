;;; library functions for a multi-level generator using CPS after BTA,
;;; higher-order abstract syntax, and a special function
;;; representation to accomplish proper function memoization in the
;;; presence of dynamic free variables

;;; application with arbitrary many arguments
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
(define (_lambda_memo lv arity label fvs bts f)
  (lambda (kappa)
    (let ((vars (nlist arity (lambda ()
			       (let ((var (gensym 'fresh)))
				 (lambda (kappa) (kappa var))))))
	  (bts (map pred bts)))
      (if (= lv 1)
	  (kappa
	   `(STATIC-CONSTRUCTOR ',label
			       (LAMBDA ,fvs
				 (LAMBDA ,(map (lambda (cv) (cv id)) vars)
				   ,((f vars) id)))
			       (LIST ,@fvs)
			       ',bts))
	  (kappa `(_LAMBDA_MEMO
		   ,(- lv 1)
		   ,arity
		   ',label
		   ',fvs
		   ',bts
		   (LAMBDA ,vars ,((f vars) id))))))))

;;; lambdas that are never memoized
;;; i.e., lambdas of continuations that we introduced ourselves are
;;; certainly candidates for these lambdas (does this ever happen?)
;;; remember we live in CPS: variables must ALWAYS be bound to
;;; continuations returning variables!
(define (_lambda lv arity f)
  (lambda (kappa)
    (let* ((vars (nlist arity (lambda () (gensym 'fresh))))
	   (varsc (map (lambda (var) (lambda (k) (k var))) vars)))
      (if (= lv 1)
	  (kappa `(LAMBDA ,vars
		    ,((f varsc) id)))
	  (kappa `(_LAMBDA ,(- lv 1) ,arity
			   (LAMBDA ,vars ,((f varsc) id))))))))

;;; LET with context propagation
(define (_let lv e f)
  (lambda (kappa)
    (let* ((var (gensym 'fresh))
	   (varc (lambda (k) (k var)))
	   (value (e id)))
      (if (= lv 1)
	  (if (pair? value)
	      `(LET ((,var ,value))
		 ,((f varc) kappa))
	      ((f (lambda (k) (k value))) kappa))
	  `(_LET ,(- lv 1) ,value
		 (LAMBDA (,var) ,((f varc) kappa)))))))

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
	    `(_CTOR ,(- lv 1)
		    ',bts
		    ',ctor ,@args))))))))

;;; selectors for constructors with memoization
(define (_sel_memo lv ctor i arg)
  (lambda (kappa)
    (arg
     (lambda (v)
       (if (= lv 1)
	   (let ((var (gensym 'fresh)))
	     (kappa `(LET ((,var (,v 'VALUE)))
		       (IF (AND (PAIR? ,var) (EQUAL? (CAR ,var) ',ctor))
			   (LIST-REF ,var ,i)
			   (ERROR)))))
	   (kappa `(_SEL ,(- lv 1) ',ctor ',i ,v)))))))

;;; test for constructors with memoization
(define (_test_memo level ctor arg)
  (lambda (kappa)
    (arg
     (lambda (v)
       (if (= level 1)
	   (let ((var (gensym 'fresh)))
	     (kappa `(LET ((,var (,v 'VALUE)))
		       (AND (PAIR? ,var)
			    (EQUAL? (CAR ,var) ',ctor)))))
	   (kappa `(_TEST ,(- level 1) ',ctor ,v)))))))

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
(define (_Lift level diff valc)
  (lambda (kappa)
    (valc
     (lambda (value)
       (if (= level 1)
	   (kappa `(_LIFT0 ,diff ,value))
	   (kappa `(_LIFT ,(- level 1) ,diff ,value)))))))

;;; extract the static parts out of a partially static value which
;;; starts with some static tag and the rest of which is described by
;;; the binding times in bt-args
(define (project-static value bt-args)
  (cons (car value)
	(let loop ((values (cdr value))
		   (bt-args bt-args))
	  (if (null? values)
	      '()
	      (let ((skeleton (loop (cdr values)
				    (cdr bt-args))))
		(if (= 0 (car bt-args))
		    (let ((s-value (car values)))
		      (if (procedure? s-value)
			  (append (s-value 'STATIC) skeleton)
			  (cons s-value skeleton)))
		    skeleton))))))

;;; extract the dynamic parts of value
(define (project-dynamic value bt-args)
  (let* ((values-by-bt
	  (project-dynamic-level (cdr value) bt-args 0))
	 (nested-dynamics
	  (let loop ((values (car values-by-bt)))
	    (if (null? values)
		'()
		(if (procedure? (car values))
		    (multi-append ((car values) 'DYNAMIC)
				  (loop (cdr values)))
		    (loop (cdr values)))))))
    (multi-append nested-dynamics (cdr values-by-bt)))) 

;;; return `values' grouped according to `bt-args' starting with `level'
(define (project-dynamic-level values bt-args level)
  (let loop ((values values)
	     (bt-args bt-args)
	     (rvalues '())
	     (rbt-args '()))
    (if (null? values)
	(if (null? rvalues)
	    (list '())
	    (cons '()
		  (project-dynamic-level (reverse rvalues)
					 (reverse rbt-args)
					 (+ level 1))))
	(if (= level (car bt-args))
	    (let ((values-by-bt
		   (loop (cdr values) (cdr bt-args) rvalues rbt-args)))
	      (cons (cons (car values) (car values-by-bt))
		    (cdr values-by-bt)))
	    (loop (cdr values) (cdr bt-args)
		  (cons (car values) rvalues) (cons (car bt-args)
						    rbt-args))))))

;;; clone the dynamic parts of a list of values 
;;; return a value with identical static skeleton, but all dynamic
;;; parts replaced by a fresh variable
(define (clone-dynamic value bts)
  (cons (car value)
	(let loop ((values (cdr value)) (bts bts))
	  (if (null? values)
	      '()
	      (let ((skeleton (loop (cdr values) (cdr bts))))
		(if (= 0 (car bts))
		    (let ((s-value (car values)))
		      (if (procedure? s-value)
			  (cons (s-value 'CLONE) skeleton)
			  (cons s-value skeleton)))
		    (let ((cloned-var (gensym 'clone)))
		      (cons (lambda (k) (k cloned-var)) skeleton))
		  ))))))

;;; (multi-append x y) is almost like (map append x y) except when the
;;; lists x and y have different lengths in which case the result has
;;; the length of the longest argument list
(define (multi-append xss yss)
  (if (pair? xss)
      (let ((xs (car xss)))
	(if (pair? yss)
	    (let ((ys (car yss)))
	      (cons (append xs ys)
		    (multi-append (cdr xss) (cdr yss))))
	    xss))
      yss)) 

;;; code that must be executed at run time of a static lambda or
;;; constructor with dynamic free variables
;;; improvements:
;;; - specialize this guy wrt `label' and `bts', as well
;;;   as project-static, project-dynamic, and clone-dynamic!
;;; - use delay/force to memoize project-static and project-dynamic
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

(define (start-memo level fn bts argsc)
  (set! *residual-program* '()) 
  (set! *memolist* '())
  (multi-memo level fn bts argsc))

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
		      (new-formals-c (apply append
					  (project-dynamic cloned-pp
							   bts)))
		      (new-formals (map (lambda (c) (c id)) new-formals-c))
		      (new-entry (set! *memolist*
				       (cons (cons pp new-name)
					     *memolist*)))
		      (new-def
		       (set! *residual-program*
			     (cons
			      `(DEFINE (,new-name ,@new-formals)
				 ,((apply (eval fn (interaction-environment))
					  (cdr cloned-pp))
				   id))
			      *residual-program*))))
		   (cons pp new-name))))
	    (res-name (cdr found)))
	 (if (= level 1)
	     ;; generate call to fn with actual arguments
	     (kappa
	      `(,res-name ,@actuals))
	     ;; reconstruct multi-memo
	     (kappa
	      `(MULTI-MEMO ,(- level 1)
			   ',res-name
			   ',(binding-times dynamics)
			   (LIST ,@actuals)))))))))

;;; reconstruct binding times from a list of value blocks
(define (binding-times blocks)
  (let loop ((blocks blocks) (i 0))
    (if (null? blocks)
	'()
	(let inner-loop ((values (car blocks)))
	  (if (null? values)
	      (loop (cdr blocks) (+ i 1))
	      (cons i (inner-loop (cdr values))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary
(define *memolist* '())
(define *residual-program* '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
