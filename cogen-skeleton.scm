;;; skeleton for multi-level cogen
;;; $Id$

;;; idea: generate new procedure names for the "copies" of the old
;;; procedures; use the original procedure names for the memoization
;;; points 
;;; current practice: generate new names for memoization points

;;; allocate a variable for the produced generating extension; it is
;;; updated as new definitions are available to cater for
;;; memo-functions 

;;; (define *generating-extension* '())
;;; (define *memo-optimize* #t)

;;; transform a multi-level programm into a program generator
(define (generate-d d*)
  ;; need to add some initialization stuff, e.g., reset name
  ;; generators, clear memo caches, and to return the constructed
  ;; program afterwards
  (set-generating-extension! '())
  ;; perform occurrence count analysis
  (oca-d d*)
  (let ((make-define (lambda (name formals body)
		       (if formals
			   `(define (,name ,@formals) ,body)
			   `(define ,name ,body)))))
    (let loop ((d* d*))
      (if (null? d*)
	  *generating-extension*
	  (begin
	    (let* ((d (car d*)))
	      (if (annIsDef? d)
		  (let* ((fname (annDefFetchProcName d))
			 (e (annDefFetchProcBody d))
			 (new-body (generate fname e))
			 (formals (annDefFetchProcFormals d)))
		    (set-generating-extension!
		     (cons (make-define fname formals new-body)
			   *generating-extension*)))))
	    (loop (cdr d*)))))))
;;; transform binding-time annotated expression e 
;;; into the generating extension
(define (generate fname e)
  (let loop ((e e))
    (cond
     ((annIsVar? e)
      ((if (annFetchVarGlobal e) make-ge-freevar make-ge-var)
       (annExprFetchLevel e)
       (annFetchVar e)))
     ((annIsConst? e)
      (make-ge-const (annFetchConst e)))
     ((annIsCond? e)
      (let ((e-test (annFetchCondTest e))
	    (e-then (annFetchCondThen e))
	    (e-else (annFetchCondElse e)))
	(make-ge-cond (annExprFetchLevel e-test)
		      (annExprFetchLevel e)
		      (loop e-test)
		      (loop e-then)
		      (loop e-else))))
     ((annIsOp? e)
      (let ((op-name (annFetchOpName e))
	    (op-args (annFetchOpArgs e)))
	(cond
	 ((eq? op-name INTERNAL-IDENTITY)
	  (loop (car op-args)))
	 ((annFetchOpDiscardability e)
	  (make-ge-op-pure (annExprFetchLevel e) op-name (map loop op-args)))
	 (else
	  (make-ge-op (annExprFetchLevel e) op-name (map loop op-args))))))
     ((annIsCall? e)
      (let ((args (annFetchCallArgs e)))
      (make-ge-call (annFetchCallName e)
		    (map annExprFetchLevel args)
		    (map loop args))))
     ((annIsLet? e)
      (let* ((header (annFetchLetHeader e))
	     (body (annFetchLetBody e))
	     (var (annFetchLetVar e))
	     (unfold? (annFetchLetUnfoldability e))
	     (header-level (annExprFetchLevel header))
	     (body-level (annExprFetchLevel body))
	     (resid-header (loop header))
	     (resid-body (loop body)))
	(if (= 0 (annFetchLetUseCount e))
	    (make-ge-begin header-level
			   body-level
			   resid-header
			   resid-body)
	    (make-ge-let header-level
			 unfold?
			 body-level
			 var
			 resid-header
			 resid-body))))
     ((annIsLambda? e)
;;;      `(_LAMBDA ,(annExprFetchLevel e)
;;;		(LAMBDA ,(annFetchLambdaVars e)
;;;		  ,(loop (annFetchLambdaBody e))))
      (let* ((fvar-exprs (annFreeVars e))
	     (fvars (map annFetchVar fvar-exprs))
	     (bts (map annExprFetchLevel fvar-exprs))
	     (vars (annFetchLambdaVars e))
	     (btv (map ann->bt (annFetchLambdaBTVars e)))
	     ;; temporary
	     (level (annExprFetchLevel e))
	     (body (annFetchLambdaBody e))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType e))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-lambda level
			    vars btv
			    (loop body))
	    (make-ge-lambda-memo level
				 vars btv
				 (annFetchLambdaLabel e)
				 fvars
				 bts
				 (loop body)))))
     ((annIsVLambda? e)
      (let* ((fvar-exprs (annFreeVars e))
	     (fvars (map annFetchVar fvar-exprs))
	     (bts (map annExprFetchLevel fvar-exprs))
	     (fixed-vars (annFetchVLambdaFixedVars e))
	     (var (annFetchVLambdaVar e))
	     (btv (annFetchVLambdaBTVars e))
	     (level (annExprFetchLevel e))
	     (body (annFetchVLambdaBody e))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType e))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-vlambda level fixed-vars
			    var btv
			    (loop body))
	    (make-ge-vlambda-memo level
				  fixed-vars
				  var btv
				  (annFetchVLambdaLabel e)
				  fvars
				  bts
				  (loop body)))))
     ((annIsApp? e)
      (let* ((rator (annFetchAppRator e))
	     (rands (annFetchAppRands e))
	     (level (annExprFetchLevel rator))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType rator)))))
	     (the-rator (loop rator))
	     (the-rands (map loop rands))
	     (btv (map annExprFetchLevel rands)))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-app level the-rator btv the-rands)
	    (make-ge-app-memo level the-rator btv the-rands))))
     ((annIsCtor? e)
      (let* ((level (annExprFetchLevel e))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType e))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-ctor level
			  (annFetchCtorName e)
			 (map loop (annFetchCtorArgs e)))
	    (make-ge-ctor-memo level
			       (map annExprFetchLevel (annFetchCtorArgs e)) 
			       (annFetchCtorName e)
			       (map loop (annFetchCtorArgs e))))))
     ((annIsSel? e)
      (let* ((arg (annFetchSelArg e))
	     (level (annExprFetchLevel arg))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType arg))))))
	((if (and *memo-optimize* (<= memo-level level))
	     make-ge-sel make-ge-sel-memo)
	 level
	 (annFetchSelName e)
	 (loop (annFetchSelArg e)))))
     ((annIsTest? e)
      (let* ((arg (annFetchTestArg e))
	     (level (annExprFetchLevel arg))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType arg))))))
	((if (and *memo-optimize* (<= memo-level level))
	     make-ge-test make-ge-test-memo)
	 level
	 (annFetchTestName e)
	 (loop (annFetchTestArg e)))))
     ((annIsLift? e)
      (make-ge-lift (annExprFetchLevel e)
		    (annFetchLiftDiff e)
		    (loop (annFetchLiftBody e))))
     ((annIsEval? e)
      (let ((bodylevel (annExprFetchLevel (annFetchEvalBody e)))
	    (ctxlevel (annExprFetchLevel e)))
	(make-ge-eval bodylevel
		      (- ctxlevel bodylevel)
		      (loop (annFetchEvalBody e)))))
     ((annIsMemo? e)
      (let* ((memo-fname (gensym fname))
	     (var-exprs (annFetchMemoVars e))
	     (vars (map annFetchVar var-exprs))
	     (bts (map annExprFetchLevel var-exprs))
	     (generated-body (loop (annFetchMemoBody e))))
	(set-generating-extension!
	      (cons `(DEFINE (,memo-fname ,@vars)
		       ,generated-body)
	       *generating-extension*))
	`(MULTI-MEMO ,(annFetchMemoLevel e)
		     ',memo-fname ,memo-fname
		     ',bts
		     (LIST ,@vars))))
     ((annIsRef? e)
      (let* ((level (annExprFetchLevel e))
	     (arg (annFetchRefArg e))
	     (arg-level (annExprFetchLevel arg))
	     (label (annFetchRefLabel e))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType e))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-op level 'MAKE-CELL (list (loop arg)))
	    (make-ge-make-cell-memo level label arg-level (loop arg))))
      )
     ((annIsDeref? e)
      (let* ((arg  (annFetchDerefArg e))
	     (level (annExprFetchLevel arg))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType arg))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-op level 'CELL-REF (list (loop arg)))
	    (make-ge-cell-ref-memo level (loop arg)))))
     ((annIsAssign? e)
      (let* ((ref (annFetchAssignRef e))
	     (arg (annFetchAssignArg e))
	     (level (annExprFetchLevel ref))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType ref))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-op level 'CELL-SET! (list (loop ref) (loop arg)))
	    (make-ge-cell-set!-memo level (loop ref) (loop arg)))))
     ((annIsCellEq? e)
      (let* ((args (annFetchCellEqArgs e))
	     (level (annExprFetchLevel (car args)))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType (car args)))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-op level 'EQ? (map loop args))
	    (make-ge-cell-eq?-memo level (map loop args)))))
     (else
      (error "cogen-skeleton:generate unrecognized syntax")))))

