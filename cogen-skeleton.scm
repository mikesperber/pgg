;;; cogen-skeleton

;;; copyright � 1996, 1997, 1998, 1999 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; skeleton for multi-level cogen

;;; idea: generate new procedure names for the "copies" of the old
;;; procedures; use the original procedure names for the memoization
;;; points 
;;; current practice: generate new names for memoization points

;;; allocate a variable for the produced generating extension; it is
;;; updated as new definitions are available to cater for
;;; memo-functions 

;;; (define *generating-extension* '())
;;; (define *memo-optimize* #t)

;;; figure out the maximum binding time at which each constructed type is used 
(define *type-maxbt-alist* '())
(define (register-type-desc! desc bt)
  (let ((type-name (desc-type desc)))
    (cond
     ((assoc type-name *type-maxbt-alist*)
      => (lambda (entry)
	   (let ((value (cdr entry)))
	     (if (< (car value) bt)
		 (set-car! value bt)))))
     (else
      (set! *type-maxbt-alist*
	    (cons (list type-name bt)
		  *type-maxbt-alist*))))))

;;; generate a list of define-data generators for those types which are
;;; used at binding time greater than zero
(define (generate-define-data def-type*)
  (let loop ((def-type* def-type*) (res '()))
    (cond
     ((null? def-type*)
      (reverse res))
     ((and (eq? (caar def-type*) 'define-data)
	   (cond
	    ((assoc (cadar def-type*) *type-maxbt-alist*)
	     => (lambda (entry)
		  (let ((bt (cadr entry)))
		    (and (< 0 bt)
			 bt))))
	    (else
	     #f)))
      => (lambda (level)
	   (loop (cdr def-type*)
		 (cons
		  `(_OP ,level _DEFINE-DATA ',(cdar def-type*))
		  res))))
     (else
      (loop (cdr def-type*) res)))))

;;; transform a multi-level programm into a program generator
(define (generate-d d* skeleton def-type*)
  ;; need to add some initialization stuff, e.g., reset name
  ;; generators, clear memo caches, and to return the constructed
  ;; program afterwards
  (set-generating-extension! '())
  (set! *type-maxbt-alist* '())
  ;; perform occurrence count analysis
  (oca-d d*)
  (let ((make-define (lambda (name formals body)
		       (cons 'DEFINE
			     (cons (if formals
				       (cons name formals)
				       name)
				   (if (and (pair? body) (eq? (car body) 'begin))
				       (cdr body)
				       (list body)))))))
    (let loop ((d* d*))
      (if (pair? d*)
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
	    (loop (cdr d*)))))
    (let loop ((bts (cdr skeleton))
	       (formals '())
	       (actuals '())
	       (i 1))
      (if (null? bts)
	  (let ((gen-def-data* (generate-define-data def-type*))
		(goal-name '$goal))
	    (set! formals (reverse formals))
	    (set! actuals (reverse actuals))
	    (set-generating-extension!
	     (cons (make-define
		    'SPECIALIZE-$GOAL
		    formals
		    `(specialize
		      ,(if (null? gen-def-data*)
			   goal-name
			   `(lambda args
			      ,@gen-def-data*
			      (apply ,goal-name args)))
		      ',skeleton (list ,@actuals)))
		   *generating-extension*)))
	  (let ((sym (string->symbol (string-append "x" (number->string i)))))
	    (if (zero? (car bts))
		(loop (cdr bts) (cons sym formals) (cons sym actuals) (+ i 1))
		(loop (cdr bts) formals (cons `',sym actuals) (+ i 1))))))
    *generating-extension*))

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
     ((annIsBegin? e)
      (let* ((header (annFetchBeginHeader e))
	     (body (annFetchBeginBody e))
	     (header-level (annExprFetchLevel header))
	     (body-level (annExprFetchLevel body))
	     (resid-header (loop header))
	     (resid-body (loop body)))
	(make-ge-begin header-level
		       body-level
		       resid-header
		       resid-body)))
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
	       (type->memo (node-fetch-type (annExprFetchType e)))))
	     (desc (annFetchCtorDesc e)))
	(register-type-desc! desc level)
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-ctor level
			  (annFetchCtorName e)
			  (map loop (annFetchCtorArgs e)))
	    (make-ge-ctor-memo level
			       (map annExprFetchLevel (annFetchCtorArgs e))
			       (desc-hidden desc)
			       (annFetchCtorName e)
			       (map loop (annFetchCtorArgs e))))))
     ((annIsSel? e)
      (let* ((arg (annFetchSelArg e))
	     (level (annExprFetchLevel arg))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType arg))))))
	(register-type-desc! (annFetchSelDesc e) level)
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
	(register-type-desc! (annFetchTestDesc e) level)
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
	     (maybe-var-expr (annFetchMemoSpecial e))
	     (maybe-var-level (and maybe-var-expr (annExprFetchLevel maybe-var-expr)))
	     (maybe-var (and maybe-var-expr (annFetchVar maybe-var-expr)))
	     (bts (map annExprFetchLevel var-exprs))
	     (generated-body (loop (annFetchMemoBody e))))
	;; (display-line maybe-var-level " " maybe-var)
	(if maybe-var
	    (let ((exit-with
		   (lambda (rvars rbts)
		     (set! vars (cons maybe-var (reverse rvars)))
		     (set! bts (cons maybe-var-level (reverse rbts)))
		     (set! maybe-var (trim-symbol maybe-var)))))
	      (let loop ((vars vars) (bts bts) (rvars '()) (rbts '()))
		(if (pair? vars)
		    (if (eq? (car vars) maybe-var)
			(loop (cdr vars) (cdr bts) rvars rbts)
			(loop (cdr vars) (cdr bts)
			      (cons (car vars) rvars) (cons (car bts) rbts)))
		    (exit-with rvars rbts)))))
	(set-generating-extension!
	      (cons `(DEFINE (,memo-fname ,@vars)
		       ,generated-body)
	       *generating-extension*))
	`(MULTI-MEMO ,(annFetchMemoLevel e) ,(annExprFetchLevel e)
		     ',memo-fname ,memo-fname
		     ,maybe-var
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
	    (make-ge-make-cell-memo level label arg-level (loop arg)))))
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
     ((annIsVector? e)
      (let* ((level (annExprFetchLevel e))
	     (size (annFetchVectorSize e))
	     (arg (annFetchVectorArg e))
	     (arg-level (annExprFetchLevel arg))
	     (label (annFetchVectorLabel e))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType e))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-op level 'MAKE-VECTOR (list (loop size) (loop arg)))
	    (make-ge-make-vector-memo level label arg-level (loop size) (loop arg)))))
     ((annIsVref? e)
      (let* ((arg   (annFetchVrefArg e))
	     (index (annFetchVrefIndex e))
	     (level (annExprFetchLevel arg))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType arg))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-op level 'VECTOR-REF (list (loop arg) (loop index)))
	    (make-ge-vector-ref-memo level (loop arg) (loop index)))))
     ((annIsVlen? e)
      (let* ((vec   (annFetchVlenVec e))
	     (level (annExprFetchLevel vec))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType vec))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-op level 'VECTOR-LENGTH (list (loop vec)))
	    (make-ge-vector-length-memo level (loop vec)))))
     ((annIsVset? e)
      (let* ((vec (annFetchVsetVec e))
	     (index (annFetchVsetIndex e))
	     (arg (annFetchVsetArg e))
	     (level (annExprFetchLevel vec))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType vec))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-op level 'VECTOR-SET! (list (loop vec) (loop index) (loop arg)))
	    (make-ge-vector-set!-memo level (loop vec) (loop index) (loop arg)))))
     ((annIsVfill? e)
      (let* ((vec (annFetchVfillVec e))
	     (arg (annFetchVfillArg e))
	     (level (annExprFetchLevel vec))
	     (memo-level
	      (ann->bt
	       (type->memo (node-fetch-type (annExprFetchType vec))))))
	(if (and *memo-optimize* (<= memo-level level))
	    (make-ge-op level 'VECTOR-FILL! (list (loop vec) (loop arg)))
	    (make-ge-vector-fill!-memo level (loop vec) (loop arg)))))
     (else
      (error "cogen-skeleton:generate unrecognized syntax")))))

