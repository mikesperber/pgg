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
      (let ((oca-body
	     (oca-e (annFetchLetBody e)
		    (cons (cons (annFetchLetVar e) 0)
			  bv))))
	(annSetLetUnfoldability! e (= 1 (cdar oca-body)))
	(oca-add (loop (annFetchLetHeader e))
		 (cdr oca-body))))
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
     ((annIsEval? e)
      (loop (annFetchEvalBody e)))
     ((annIsMemo? e)
      (loop (annFetchMemoBody e)))
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