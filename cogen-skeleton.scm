;;; skeleton for multi-level cogen
;;; $Id$
;;; $Log$
;;; Revision 1.7  1995/11/06 15:40:50  thiemann
;;; handle eval, fix bug in lambda lifter
;;;
;;; Revision 1.6  1995/11/03  17:12:26  thiemann
;;; more sophisticated type signatures
;;; correct handling of direct-style if and let
;;; extended syntax (nested defines allowed)
;;;
;;; Revision 1.5  1995/10/27  08:52:27  thiemann
;;; fixed problem in binding-time analysis
;;;
;;; Revision 1.4  1995/10/23  16:59:13  thiemann
;;; type annotations (may) work
;;; standard memoization may be circumvented
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
	    ,(make-conditional-arm (loop (annFetchCondThen e)))
	    ,(make-conditional-arm (loop (annFetchCondElse e)))))
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
	  (LIST ,@fvars)
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
     ((annIsEval? e)
      (let ((bodylevel (+ 1 (annExprFetchLevel (annFetchEvalBody e))))
	    (ctxlevel (+ 1 (annExprFetchLevel e))))
      `(_EVAL ,bodylevel ,(- ctxlevel bodylevel)
	      ,(loop (annFetchEvalBody e)))))
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
