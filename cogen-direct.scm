;;; $Id$
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;; $Log$
;;; Revision 1.5  1995/11/06 15:40:48  thiemann
;;; handle eval, fix bug in lambda lifter
;;;
;;; Revision 1.4  1995/11/03  17:12:21  thiemann
;;; more sophisticated type signatures
;;; correct handling of direct-style if and let
;;; extended syntax (nested defines allowed)
;;;
;;; Revision 1.3  1995/10/27  08:52:24  thiemann
;;; fixed problem in binding-time analysis
;;;
;;; Revision 1.2  1995/10/23  16:59:07  thiemann
;;; type annotations (may) work
;;; standard memoization may be circumvented
;;;

;;; set result to the unit of the identity monad
(define result id)
;;; set creation of conditional arms
(define (make-conditional-arm e) `(LAMBDA () ,e))

;;; cogen functions
(define (_app lv f . args)
  (if (= lv 1)
      `(,f ,@args)
      `(_APP ,(pred lv) ,f ,@args))) 

(define (_app_memo lv f . args)
  (if (= lv 1)
      `((,f 'VALUE) ,@args)
      `(_APP_MEMO ,(pred lv) ,f ,@args)))

(define (_lambda lv arity f)
  (let* ((vars (map gensym arity))
	 (fun `(LAMBDA ,vars ,(reset (apply f vars)))))
    (if (= lv 1)
	fun
	`(_LAMBDA ,(pred lv) ',arity ,fun))))

(define (_lambda_memo lv arity label vvs bts f)
  (let* ((vars (map gensym arity)))
    (let* ((dynamics (project-dynamic (cons label vvs) bts))
	   (new-vvs (apply append dynamics))
	   (new-bts (binding-times dynamics))
	   (freevars (map (lambda (bt vv)
			    (if (zero? bt) vv (gensym 'var)))
			  new-bts new-vvs)))
      (if (= lv 1)
	  `(STATIC-CONSTRUCTOR
	    ',label
	    (LAMBDA ,freevars
	      (LAMBDA ,vars
		,(reset (apply (apply f freevars) vars))))
	    (LIST ,@new-vvs)
	    ',new-bts)
	  ;; > lv 1
	  `(_LAMBDA_MEMO
	    ,(- lv 1)
	    ',arity
	    ',(gensym label)
	    (LIST ,@new-vvs)
	    ',new-bts
	    (LAMBDA ,freevars
	      (LAMBDA ,vars
		,(reset (apply (apply f vvs) vars)))))))))

(define (_let lv e f)
  (let ((var (gensym 'var)))
    (if (= lv 1)
	(if (and (pair? e)
		 (not (equal? 'QUOTE (car e))))
	    (shift k `(LET ((,var ,e))
			,(reset (k (f var)))))
	    (f e))
	(shift k
	       `(_LET ,(pred lv) ,e (LAMBDA (,var)
				      ,(reset (k (f var)))))))))

(define (_ctor_memo lv bts ctor . args)
  (let ((new-bts (map pred bts)))
    (if (= lv 1)
	`(STATIC-CONSTRUCTOR ',ctor ,ctor (LIST ,@args) ',new-bts)
	`(_CTOR_MEMO ,(- lv 1) ',bts ',ctor ,@args))))

(define (_sel_memo lv sel v)
  (if (= lv 1)
      `(,sel (,v 'VALUE))
      `(_SEL_MEMO ,(- lv 1) ',sel ,v)))

(define (_test_memo level ctor-test v)
  (if (= level 1)
      `(,ctor-test (,v 'VALUE))
      `(_TEST ,(- level 1) ',ctor-test ,v)))

;;; needs RESET, somewhere
;;; therefore: the arms of the conditional must be thunks, so that we
;;; can capture control. we get this for free in the CPS version where
;;; et2 and et3 are continuations, anyway
(define (_If level e1 et2 et3)
  (if (= level 1)
      `(IF ,e1 ,(reset (et2)) ,(reset (et3)))
      `(_IF ,(- level 1) ,e1 (LAMBDA () ,(reset (et2))) (LAMBDA () ,(reset (et3))))))

;;; alternative implementation
(define (_If1 level e1 et2 et3)
  (if (= level 0)
      (if e1 (et2) (et3))
      `(_IF1 ,(- level 1) ,e1 (LAMBDA () ,(reset (et2))) (LAMBDA () ,(reset (et3))))))

(define (_Op level op . args)
  (if (= level 1)
      `(,op ,@args)
      `(_OP ,(- level 1) ',op ,@args)))

(define (_Lift0 level val)
  (if (= level 1)
      (if (or (number? val) (string? val) (boolean? val))
	  val
	  `(QUOTE ,val))
      `(_LIFT0 ,(- level 1) ',val)))

(define (_Lift level diff value)
  (if (= level 1)
      `(_LIFT0 ,diff ,value)
      `(_LIFT ,(- level 1) ,diff ,value)))

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
  (multi-memo level fn bts args))

;;; the memo-function
;;; - fn is the name of the function to memoize
;;; - args are the free variables of the function's body
;;; - bts are their binding times
(define (multi-memo level fn bts args)
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
		 (new-formals (apply append (project-dynamic cloned-pp bts)))
		 (new-entry (add-to-memolist! (cons pp new-name)))
		 (new-def  `(DEFINE (,new-name ,@new-formals)
			      ,(reset (apply (eval fn (interaction-environment))
					     (cdr cloned-pp))))))
	      (add-to-residual-program! new-def)
	      (cons pp new-name))))
       (res-name (cdr found)))
    (if (= level 1)
	;; generate call to fn with actual arguments
	`(,res-name ,@actuals)
	;; reconstruct multi-memo
	`(MULTI-MEMO ,(- level 1)
		     ',res-name
		     ',(binding-times dynamics)
		     (LIST ,@actuals)))))

