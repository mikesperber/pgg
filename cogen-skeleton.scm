;;; skeleton for multi-level cogen
;;; $Id$
;;; $Log$
;;; Revision 1.16  1996/08/14 09:00:16  thiemann
;;; revived CPS version and some polishing
;;;
;;; Revision 1.15  1996/08/01 11:53:09  thiemann
;;; modified bta of operators
;;;
;;; Revision 1.14  1996/08/01 07:07:06  thiemann
;;; fixed _lambda_memo
;;;
;;; Revision 1.12  1996/07/30 08:56:34  thiemann
;;; bugfixes in BTA and preprocessor
;;;
;;; Revision 1.11  1996/07/15 14:01:20  thiemann
;;; stable version after MBTA
;;;
;;; Revision 1.10  1996/06/03 07:34:57  thiemann
;;; checkpoint after including new equational BTA
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
(define memo-optimize #t)

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
      (make-ge-var (+ 1 (annExprFetchLevel e))
		   (annFetchVar e)))
     ((annIsConst? e)
      (make-ge-const (annFetchConst e)))
     ((annIsCond? e)
      (make-ge-cond (+ 1 (annExprFetchLevel (annFetchCondTest e)))
		    (loop (annFetchCondTest e))
		    (loop (annFetchCondThen e))
		    (loop (annFetchCondElse e))))
     ((annIsOp? e)
      (let ((op-name (annFetchOpName e))
	    (op-args (annFetchOpArgs e)))
	(if (eq? op-name INTERNAL-IDENTITY)
	    (loop (car op-args))
	    (make-ge-op (+ 1 (annExprFetchLevel e))
			op-name (map loop op-args)))))
     ((annIsCall? e)
      (make-ge-call (annFetchCallName e)
		    (map loop (annFetchCallArgs e))))
     ((annIsLet? e)
      (let* ((level (annExprFetchLevel (annFetchLetHeader e)))
	     (var (annFetchLetVar e))
	     (n-level (if (annFetchLetUnfoldability e)
			  level
			  (+ level 1)))
	     (header (loop (annFetchLetHeader e)))
	     (body (loop (annFetchLetBody e))))
	(if (= 0 (annFetchLetUseCount e))
	    (make-ge-begin n-level header body)
	    (make-ge-let n-level var header body))))
     ((annIsLambda? e)
;;;      `(_LAMBDA ,(+ 1 (annExprFetchLevel e))
;;;		(LAMBDA ,(annFetchLambdaVars e)
;;;		  ,(loop (annFetchLambdaBody e))))
      (let* ((fvar-exprs (annFreeVars e))
	     (fvars (map annFetchVar fvar-exprs))
	     (bts (map succ (map annExprFetchLevel fvar-exprs)))
	     (vars (annFetchLambdaVars e))
	     (btv (annFetchLambdaBTVars e))
	     ;; temporary
	     (level (annExprFetchLevel e))
	     (body (annFetchLambdaBody e))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (full-ecr (annExprFetchType e)))))))
	(if (and memo-optimize (<= memo-level level))
	    (make-ge-lambda (+ 1 level)
			    vars btv
			    (loop body))
	    (make-ge-lambda-memo (+ 1 level)
				 vars btv
				 (annFetchLambdaLabel e)
				 fvars
				 bts
				 (loop body)))))
     ((annIsVLambda? e)
      (let* ((fvar-exprs (annFreeVars e))
	     (fvars (map annFetchVar fvar-exprs))
	     (bts (map succ (map annExprFetchLevel fvar-exprs)))
	     (fixed-vars (annFetchVLambdaFixedVars e))
	     (var (annFetchVLambdaVar e))
	     (btv (annFetchVLambdaBTVars e)))
	(make-ge-vlambda-memo (+ 1 (annExprFetchLevel e))
			      fixed-vars
			      var btv
			      (annFetchVLambdaLabel e)
			      fvars
			      bts
			      (loop (annFetchVLambdaBody e)))))
     ((annIsApp? e)
      (let* ((rator (annFetchAppRator e))
	     (rands (annFetchAppRands e))
	     (level (annExprFetchLevel rator))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (full-ecr (annExprFetchType rator)))))))
	(if (and memo-optimize (<= memo-level level))
	    (make-ge-app (+ 1 level)
			 (loop rator)
			 (map loop rands))
	    (make-ge-app-memo (+ 1 level)
			      (loop rator)
			      (map succ (map annExprFetchLevel rands))
			      (map loop rands)))))
     ((annIsCtor? e)
      (let* ((level (annExprFetchLevel e))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (full-ecr
					     (annExprFetchType
					      e)))))))
	(if (and memo-optimize (<= memo-level level))
	    (make-ge-ctor (+ 1 level)
			  (annFetchCtorName e)
			 (map loop (annFetchCtorArgs e)))
	    (make-ge-ctor-memo (+ 1 level)
			       (map succ (map annExprFetchLevel (annFetchCtorArgs e))) 
			       (annFetchCtorName e)
			       (map loop (annFetchCtorArgs e))))))
     ((annIsSel? e)
      (let* ((arg (annFetchSelArg e))
	     (level (annExprFetchLevel arg))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (full-ecr (annExprFetchType arg)))))))
	((if (and memo-optimize (<= memo-level level))
	     make-ge-sel make-ge-sel-memo)
	 (succ level)
	 (annFetchSelName e)
	 (loop (annFetchSelArg e)))))
     ((annIsTest? e)
      (let* ((arg (annFetchTestArg e))
	     (level (annExprFetchLevel arg))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (full-ecr (annExprFetchType arg)))))))
	((if (and memo-optimize (<= memo-level level))
	     make-ge-test make-ge-test-memo)
	 (succ level)
	 (annFetchTestName e)
	 (loop (annFetchTestArg e)))))
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
	`(MULTI-MEMO ,(+ 1 (annFetchMemoLevel e))
		     ',memo-fname ,memo-fname
		     ',bts
		     (LIST ,@vars))))
     (else
      (error)))))
