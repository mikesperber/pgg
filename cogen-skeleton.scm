;;; skeleton for multi-level cogen
;;; $Id$
;;; $Log$
;;; Revision 1.3  1995/10/23 16:53:03  thiemann
;;; continuation based reduction works
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
      `(_LIFT0 1 ',(annFetchConst e)))
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
;;;      `(_LAMBDA ,(+ 1 (annExprFetchLevel e))
;;;		(LAMBDA ,(annFetchLambdaVars e)
;;;		  ,(loop (annFetchLambdaBody e))))
      (let* ((fvar-exprs (annFreeVars e))
	     (fvars (map annFetchVar fvar-exprs))
	     (bts (map succ (map annExprFetchLevel fvar-exprs))))
	`(_LAMBDA_MEMO
	  ',(+ 1 (annExprFetchLevel e))
	  ',(annFetchLambdaVars e)
	  ',(annFetchLambdaLabel e)
	  ',fvars
	  ',bts
	  (LAMBDA ,fvars
	    (LAMBDA ,(annFetchLambdaVars e)
	    ,(loop (annFetchLambdaBody e)))))))
     ((annIsApp? e)
      `(_APP_MEMO ,(+ 1 (annExprFetchLevel (annFetchAppRator e)))
	     ,(loop (annFetchAppRator e))
	     ,@(map loop (annFetchAppRands e))))
     ((annIsCtor? e)
      `(_CTOR_MEMO ,(+ 1 (annExprFetchLevel e))
		   ',(map succ (map annExprFetchLevel (annFetchCtorArgs e))) 
		   ',(annFetchCtorName e)
		   ,@(map loop (annFetchCtorArgs e))))
     ((annIsSel? e)
      `(_SEL_MEMO ,(succ (annExprFetchLevel (annFetchSelArg e)))
		  ',(annFetchSelName e)
		  ,(loop (annFetchSelArg e))))
     ((annIsTest? e)
      `(_TEST_MEMO ,(succ (annExprFetchLevel (annFetchTestArg e)))
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
	     (bts (map succ (map annExprFetchLevel var-exprs))))
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
