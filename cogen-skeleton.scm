;;; skeleton for multi-level cogen
;;; $Id$
;;; $Log$
;;; Revision 1.9  1996/04/01 08:18:53  thiemann
;;; everything's up and running again
;;;
;;; Revision 1.8  1995/11/09  16:48:06  thiemann
;;; implemented simple occurrence count analysis
;;;
;;; Revision 1.7  1995/11/06  15:40:50  thiemann
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
  ;; perform occurrence count analysis
  (oca-d d*)
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
      (make-ge-var (annFetchVar e)))
     ((annIsConst? e)
      (make-ge-const (annFetchConst e)))
     ((annIsCond? e)
      (make-ge-cond (+ 1 (annExprFetchLevel (annFetchCondTest e)))
		    (loop (annFetchCondTest e))
		    (loop (annFetchCondThen e))
		    (loop (annFetchCondElse e))))
     ((annIsOp? e)
      (make-ge-op (+ 1 (annExprFetchLevel e))
		  (annFetchOpName e)
		  (map loop (annFetchOpArgs e))))
     ((annIsCall? e)
      (make-ge-call (annFetchCallName e)
		    (map loop (annFetchCallArgs e))))
     ((annIsLet? e)
      (let* ((level (annExprFetchLevel (annFetchLetHeader e)))
	     (var (annFetchLetVar e))
	     (unfoldability (annFetchLetUnfoldability e))
	     (n-level (if unfoldability level (+ level 1))))
	(make-ge-let n-level
		     var
		     (loop (annFetchLetHeader e))
		     (loop (annFetchLetBody e)))))
     ((annIsLambda? e)
;;;      `(_LAMBDA ,(+ 1 (annExprFetchLevel e))
;;;		(LAMBDA ,(annFetchLambdaVars e)
;;;		  ,(loop (annFetchLambdaBody e))))
      (let* ((fvar-exprs (annFreeVars e))
	     (fvars (map annFetchVar fvar-exprs))
	     (bts (map succ (map annExprFetchLevel fvar-exprs)))
	     (vars (annFetchLambdaVars e)))
	(make-ge-lambda-memo (+ 1 (annExprFetchLevel e))
			     vars
			     (annFetchLambdaLabel e)
			     fvars
			     bts
			     (loop (annFetchLambdaBody e)))))
     ((annIsApp? e)
      (make-ge-app-memo (+ 1 (annExprFetchLevel (annFetchAppRator e)))
			(loop (annFetchAppRator e))
			(map loop (annFetchAppRands e))))
     ((annIsCtor? e)
      (make-ge-ctor-memo (+ 1 (annExprFetchLevel e))
			 (map succ (map annExprFetchLevel (annFetchCtorArgs e))) 
			 (annFetchCtorName e)
			 (map loop (annFetchCtorArgs e))))
     ((annIsSel? e)
      (make-ge-sel-memo (succ (annExprFetchLevel (annFetchSelArg e)))
			(annFetchSelName e)
			(loop (annFetchSelArg e))))
     ((annIsTest? e)
      (make-ge-test-memo (succ (annExprFetchLevel (annFetchTestArg e)))
			 (annFetchTestName e)
			 (loop (annFetchTestArg e))))
     ((annIsLift? e)
      (make-ge-lift (+ 1 (annExprFetchLevel e))
		    (annFetchLiftDiff e)
		    (loop (annFetchLiftBody e))))
     ((annIsEval? e)
      (let ((bodylevel (+ 1 (annExprFetchLevel (annFetchEvalBody e))))
	    (ctxlevel (+ 1 (annExprFetchLevel e))))
	(make-ge-eval bodylevel
		      (- ctxlevel bodylevel)
		      (loop (annFetchEvalBody e)))))
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
