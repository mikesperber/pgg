;;; cogen-oca.scm

;;; copyright © 1996, 1997, 1998, 1999, 2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; occurrence count analysis on abstract syntax
;;; meaning of an oc for a variable:
;;; 0 - it does not occur
;;; 1 - it occurs exactly once
;;; >1 - everything else

(define (oca-d d*)
  (for-each (lambda (d)
	      (oca-e (annDefFetchProcBody d) '()))
	    d*))

(define (oca-e e bv)
  (let loop ((e e))
    (cond
     ((annIsVar? e)
      (oca-use-var (annFetchVar e) bv))
     ((annIsConst? e)
      bv)
     ((annIsCond? e)
      (oca-add (loop (annFetchCondTest e))
	       (oca-max (loop (annFetchCondThen e))
			(loop (annFetchCondElse e)))))
     ((annIsOp? e)
      (let ((args (annFetchOpArgs e)))
	(if (null? args)
	    bv
	    (apply oca-add (map loop args)))))
     ((annIsCall? e)
      (let ((args (annFetchCallArgs e)))
	(if (null? args)
	    bv
	    (apply oca-add (map loop args)))))
     ((annIsLet? e)
      (let* ((header (annFetchLetHeader e))
	     (body (annFetchLetBody e))
	     (oca-body (oca-e body (cons (cons (annFetchLetVar e) 0) bv)))
	     (oc (cdar oca-body)))
	(annSetLetUnfoldability!
	 e (or (= 1 oc)			; beware from inlining side effecting ops!
	       (and (zero? oc) (annExprTerminates? header))))
	(annSetLetUseCount! e oc)
	(oca-add (loop header)
		 (cdr oca-body))))
     ((annIsBegin? e)
      (oca-add (loop (annFetchBeginHeader e))
	       (loop (annFetchBeginBody e))))
     ((annIsVLambda? e)
      (let* ((fixed-formals (annFetchVLambdaFixedVars e))
	     (var-formal (annFetchVLambdaVar e))
	     (formals (cons var-formal fixed-formals)))
	(list-tail (oca-e (annFetchVLambdaBody e)
			  (append (map (lambda (v) (cons v 0)) formals) bv))
		   (length formals))))
     ((annIsLambda? e)
      (let ((formals (annFetchLambdaVars e)))
	(list-tail (oca-e (annFetchLambdaBody e)
			  (append (map (lambda (v) (cons v 0)) formals) bv))
		   (length formals))))
     ((annIsApp? e)
      (apply oca-add (cons (loop (annFetchAppRator e))
			   (map loop (annFetchAppRands e)))))
     ((annIsCtor? e)
      (let ((args (annFetchCtorArgs e)))
	(if (null? args)
	    bv
	    (apply oca-add (map loop args)))))
     ((annIsSel? e)
      (loop (annFetchSelArg e)))
     ((annIsTest? e)
      (loop (annFetchTestArg e)))
     ((annIsLift? e)
      (loop (annFetchLiftBody e)))
     ((annIsRef? e)
      (loop (annFetchRefArg e)))
     ((annIsDeref? e)
      (loop (annFetchDerefArg e)))
     ((annIsAssign? e)
      (oca-add (loop (annFetchAssignRef e))
	       (loop (annFetchAssignArg e))))
     ((annIsCellEq? e)
      (apply oca-add (map loop (annFetchCellEqArgs e))))
     ((annIsVector? e)
      (oca-add (loop (annFetchVectorSize e))
	       (loop (annFetchVectorArg e))))
     ((annIsVref? e)
      (oca-add (loop (annFetchVrefArg e))
	       (loop (annFetchVrefIndex e))))
     ((annIsVlen? e)
      (loop (annFetchVlenVec e)))
     ((annIsVset? e)
      (oca-add (loop (annFetchVsetVec e))
	       (loop (annFetchVsetIndex e))
	       (loop (annFetchVsetArg e))))
     ((annIsVfill? e)
      (oca-add (loop (annFetchVfillVec e))
	       (loop (annFetchVfillArg e))))
     ((annIsEval? e)
      (loop (annFetchEvalBody e)))
     ((annIsMemo? e)
      (oca-add
       (cond
	((annFetchMemoSpecial e) => loop)
	(else (oca-use-var '****unused**** bv)))
       (loop (annFetchMemoBody e))))
     (else
      (error 'oca-e "Unknown syntax construction")))))

(define oca-max
  (lambda bvs
    (apply map oca-max-pairs bvs)))

(define oca-max-pairs
  (lambda bvrow
    (cons (caar bvrow) (apply max (map cdr bvrow)))))

(define oca-add
  (lambda bvs
    (apply map oca-add-pairs bvs)))

(define oca-add-pairs
  (lambda bvrow
    (cons (caar bvrow) (apply + (map cdr bvrow)))))

(define oca-shift
  (lambda (bv)
    (for-each (lambda (p) (set-cdr! p (* 2 (cdr p)))) bv)))

(define oca-use-var
  (lambda (var bv)
    (let loop ((bv bv))
      (if (null? bv)
	  '()
	  (if (equal? (caar bv) var)
	      (cons (cons var 1) (cdr bv))
	      (cons (car bv) (loop (cdr bv))))))))