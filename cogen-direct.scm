;;; $Id$
;;; direct style version of the continuation-based multi-level
;;; compiler generator (with control operators)
;;; $Log$
;;; Revision 1.2  1995/10/23 16:59:07  thiemann
;;; type annotations (may) work
;;; standard memoization may be circumvented
;;;

;;; set result to the unit of the identity monad
(define result id)

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
	   (freevars (map (lambda (x) (gensym 'var)) new-bts)))
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
	    ',(gensym label)
	    (LIST ,@new-vvs)
	    ',new-bts
	    (LAMBDA ,freevars
	      (LAMBDA ,vars
		,(reset (apply (apply f vvs) vars)))))))))

(define (_let lv e f)
  (let ((var (gensym 'var)))
    (if (= lv 1)
	(if (pair? e)
	    `(LET ((,var ,e))
	       ,(f var))
	    (f e))
	`(SHIFT k
		(_LET ,(pred lv) ,e (LAMBDA (,var)
				      (RESET (k ,(f var)))))))))

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
      `(_TEST ,(- level 1) ',ctor ,v)))

;;; needs RESET, somewhere
(define (_If level e1 e2 e3)
  (if (= level 1)
      `(IF ,e1 ,e2 ,e3)
      `(_IF ,(- level 1) ,e1 (RESET ,e2) (RESET ,e3))))

(define (_Op level op . args)
  (if (= level 1)
      `(,op ,@args)
      `(_OP ,(- level 1) ',op ,@args)))

(define (_Lift0 level val)
  (if (= level 1)
      `(QUOTE ,val)
      `(_LIFT0 ,(- level 1) ',val)))

(define (_Lift level diff value)
  (if (= level 1)
      `(_LIFT0 ,diff ,value)
      `(_LIFT ,(- level 1) ,diff ,value)))

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

