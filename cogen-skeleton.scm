;;; skeleton for multi-level cogen
;;;

;;; idea: generate new procedure names for the "copies" of the old
;;; procedures; use the original procedure names for the memoization
;;; points 
;;; current practice: generate new names for memoization points

;;; allocate a variable for the produced generating extension; it is
;;; updated as new definitions are available to cater for
;;; memo-functions 
(define *generating-extension* '())

;;; transform a multi-level programm into a program generator
(define (generate-d d*)
  ;; need to add some initialization stuff, e.g., reset name
  ;; generators, clear memo caches, and to return the constructed
  ;; program afterwards
  (set! *generating-extension* '())
  (let loop ((d* d*))
    (if (null? d*)
	*generating-extension*
	(let* ((d (car d*))
	       (fname (annDefFetchProcName d))
	       (e (annDefFetchProcBody d))
	       (formals (annDefFetchProcFormals d)))
	  (set! *generating-extension*
		(cons `(define (,fname ,@formals)
			 ,(generate fname e))
		      *generating-extension*))
	  (loop (cdr d*))))))
;;; transform binding-time annotated expression e 
;;; into the generating extension
(define (generate fname e)
  (let loop ((e e))
    (cond
     ((annIsVar? e)
      (annFetchVar e))
     ((annIsConst? e)
      `(QUOTE ,(annFetchConst e)))
     ((annIsCond? e)
      `(_IF ,(+ 1 (annExprFetchLevel (annFetchCondTest e)))
	    ,(loop (annFetchCondTest e))
	    ,(loop (annFetchCondThen e))
	    ,(loop (annFetchCondElse e))))
     ((annIsOp? e)
      `(_OP ,(+ 1 (annExprFetchLevel e))
	    ',(annFetchOpName e)
	    ,@(map loop (annFetchOpArgs e))))
     ((annIsCall? e)
      `(,(annFetchCallName e)
	,@(map loop (annFetchCallArgs e))))
     ((annIsLet? e)
      `(_LET ,(+ 1 (annExprFetchLevel (annFetchLetHeader e)))
	     ,(loop (annFetchLetHeader e))
	     (LAMBDA (,(annFetchLetVar e))
	       ,(loop (annFetchLetBody e)))))
     ((annIsLambda? e)
      `(_LAMBDA ,(+ 1 (annExprFetchLevel e))
		(LAMBDA ,(annFetchLambdaVars e)
		  ,(loop (annFetchLambdaBody e)))))
     ((annIsApp? e)
      `(_APP ,(+ 1 (annExprFetchLevel e))
	     ,(loop (annFetchAppRator e))
	     ,@(map loop (annFetchAppRands e))))
     ((annIsCtor? e)
      `(_CTOR_MEMO ,(+ 1 (annExprFetchLevel e))
		   ,(map succ (map annExprFetchLevel (annFetchCtorArgs e))) 
		   ',(annFetchCtorName e)
		   ,@(map loop (annFetchCtorArgs e))))
     ((annIsSel? e)
      `(_SEL_MEMO ,(succ (annExprFetchLevel e))
		  ',(annFetchSelName e)
		  ,(annFetchSelComp e)
		  ,(loop (annFetchSelArg e))))
     ((annIsTest? e)
      `(_TEST_MEMO ,(succ (annExprFetchLevel e))
		   ',(annFetchTestName e)
		   ,(loop (annFetchTestArg e))))
     ((annIsLift? e)
      `(_LIFT ,(+ 1 (annExprFetchLevel e))
	      ,(annFetchLiftDiff e)
	      ,(loop (annFetchLiftBody e))))
     ((annIsMemo? e)
      (let* ((memo-fname (gensym fname))
	     (var-exprs (annFetchMemoVars e))
	     (vars (map annFetchVar var-exprs))
	     (bts (map (lambda (z) (+ z 1))
		       (map annExprFetchLevel var-exprs))))
	(set! *generating-extension*
	      (cons `(DEFINE (,memo-fname ,@vars)
		       ,(loop (annFetchMemoBody e)))
	       *generating-extension*))
	`(MULTI-MEMO ,(+ 1 (annExprFetchLevel e))
		     ',memo-fname
		     ',bts
		     (LIST ,@vars))))
     (else
      (error)))))

;;; sort args by binding times
;;; output: list of groups of the form (a1 ... an) for each level 0
;;; through max (bts)
(define (group-by-binding-time args bts)
   (let loop ((level 0)
	      (args args)
	      (bts bts)
	      (remaining-args '())
	      (remaining-bts '()))
     (if (null? args)
	 (if (null? remaining-args)
	     (list '())
	     (cons '() (loop (+ level 1)
			     remaining-args
			     remaining-bts
			     '() '())))
	 (if (= level (car bts))
	     (let ((result (loop level (cdr args) (cdr bts)
				 remaining-args remaining-bts)))
	       (cons (cons (car args) (car result))
		     (cdr result)))
	     (loop level (cdr args) (cdr bts)
		   (cons (car args) remaining-args)
		   (cons (car bts) remaining-bts))))))
