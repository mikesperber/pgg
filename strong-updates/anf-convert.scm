;;; conversion to a normal form
(define *anf-pp-count* 0)
(define *anf-app-count* 0)
(define *anf-let/unit-count* 0)
(define *anf-lambdas* '())
(define *anf-ctor* '())
(define *anf-refs* '())
(define *anf-apps* '())
(define *anf-let/units* '())
(define *anf-pp-map* '())
(define *anf-app-map* '())
(define anf-next-let!
  (lambda (anf)
    (let ((x *anf-let/unit-count*))
      (set! *anf-let/unit-count* (+ 1 x))
      (anf-let->nr! anf x)
      (set! *anf-let/units* (cons anf *anf-let/units*))
      anf)))
(define anf-next-unit!
  (lambda (anf)
    (let ((x *anf-let/unit-count*))
      (set! *anf-let/unit-count* (+ 1 x))
      (anf-unit->nr! anf x)
      (set! *anf-let/units* (cons anf *anf-let/units*))
      anf)))
(define anf-next-lambda!
  (lambda (anf)
    (let ((x *anf-pp-count*))
      (set! *anf-pp-count* (+ 1 x))
      (anf-lambda->nr! anf x)
      (set! *anf-lambdas* (cons anf *anf-lambdas*))
      anf)))
(define anf-next-ref!
  (lambda (anf)
    (let ((x *anf-pp-count*))
      (set! *anf-pp-count* (+ 1 x))
      (anf-ref->nr! anf x)
      (set! *anf-refs* (cons anf *anf-refs*))
      anf)))
(define anf-next-ctor!
  (lambda (anf)
    (let ((x *anf-pp-count*))
      (set! *anf-pp-count* (+ 1 x))
      (anf-ctor->nr! anf x)
      (set! *anf-ctor* (cons anf *anf-ctor*))
      anf)))
(define anf-next-app!
  (lambda (anf)
    (let ((x *anf-app-count*))
      (set! *anf-app-count* (+ 1 x))
      (anf-app->nr! anf x)
      (set! *anf-apps* (cons anf *anf-apps*))
      anf)))

(define (anf-update-map! map get-index contents)
    (let loop ((contents contents))
      (if (pair? contents)
	    (let* ((entry (car contents))
		   (i (get-index entry)))
	    (vector-set! map i entry)
	    (loop (cdr contents))))))
(define (anf-extract-maps!)
  (set! *anf-pp-map* (make-vector *anf-pp-count*))
  (anf-update-map! *anf-pp-map* anf-lambda->nr *anf-lambdas*)
  (anf-update-map! *anf-pp-map* anf-ref->nr *anf-refs*)
  ;(anf-update-map! *anf-pp-map* anf-ctor->nr *anf-ctor*)
  (set! *anf-app-map* (make-vector *anf-app-count*))
  (anf-update-map! *anf-app-map* anf-app->nr *anf-apps*))

(define (anf-convert d*)
  (set! *anf-pp-count* 0)
  (set! *anf-app-count* 0)
  (set! *anf-let/unit-count* 0)
  (set! *anf-lambdas* '())
  (set! *anf-ctor* '())
  (set! *anf-refs* '())
  (set! *anf-apps* '())
  (set! *anf-let/units* '())
  (set! *anf-pp-map* '())
  (set! *anf-app-map* '())
  (gensym-reset!)
  (let ((result (map anf-convert-d d*)))  
    (anf-extract-maps!)
    result))

(define (anf-convert-d d)
  (let ((name (annDefFetchProcName d))
	(formals (annDefFetchProcFormals d))
	(body	(annDefFetchProcBody d)))
    (if formals
	(make-anf-def name
		      (anf-convert-top (annMakeLambda 0 formals body)))
	(make-anf-def name (anf-convert-top body)))))

(define (anf-convert-e* e* v make-anf c)
  (let rec ((args e*) (conv-args '()))
    (if (null? args)
	(let ((newvar (or v (gensym 'anf))))
	  (anf-next-let!
	   (make-anf-let newvar
			 (make-anf (reverse conv-args))
			 (c (make-anf-var newvar)))))
	      (let ((arg (car args))
		    (args (cdr args)))
		(anf-convert-e arg
			       (lambda (conv-arg)
				 (rec args (cons conv-arg conv-args))))))))

(define (anf-convert-top e)
  (anf-convert-e e (lambda (z) (anf-next-unit! (make-anf-unit z)))))

(define (anf-convert-e e c)
  (let loop ((e e) (v #f) (c c))
    (cond
     ((annIsVar? e)
      (c (make-anf-var (annFetchVar e))))
     ((annIsConst? e)
      (let ((newvar (or v (gensym 'anf))))
	(anf-next-let!
	 (make-anf-let newvar
		       (make-anf-const (annFetchConst e))
		       (c (make-anf-var newvar))))))
     ((annIsCond? e)
      (loop (annFetchCondTest e)
	    #f
	    (lambda (t)
	      (let ((newvar (or v (gensym 'anf))))
		(anf-next-let!
		 (make-anf-let newvar
			       (make-anf-cond t (anf-convert-top (annFetchCondThen e))
					      (anf-convert-top (annFetchCondElse e)))
			       (c (make-anf-var newvar))))))))
     ;; an alternative
;;;     ((annIsCond? e)
;;;      (loop (annFetchCondTest e)
;;;	    #f
;;;	    (lambda (t)
;;;	      (make-anf-cond t
;;;			     (loop (annFetchCondThen e) v c)
;;;			     (loop (annFetchCondElse e) v c)))))
     ((annIsOp? e)
      (let ((args (annFetchOpArgs e)))
	(anf-convert-e* args v (lambda (conv-args)
				 (make-anf-op (annFetchOpName e) conv-args))
			c)))
     ((annIsCall? e)
      (let ((args (annFetchCallArgs e)))
	(anf-convert-e* args v (lambda (conv-args)
				 (anf-next-app!
				  (make-anf-app (make-anf-var (annFetchCallName e)) conv-args)))
			c)))
     ((annIsLet? e)
      (let ((letvar (annFetchLetVar e))
	    (body (annFetchLetBody e)))
	(loop (annFetchLetHeader e) letvar
	      (lambda (conv-header)
		(if (and (anf-var? conv-header) ;this will always be the case
			 (eq? letvar (anf-var->name conv-header)))
		    (loop body v c)
		    (anf-next-let!
		     (make-anf-let letvar conv-header
				   (loop body v c))))))))
     ((annIsVLambda? e)
      (error "VLambda ignored"))
     ((annIsLambda? e)
      (let ((newvar (or v (gensym 'anf))))
	(anf-next-let!
	 (make-anf-let newvar
		       (anf-next-lambda!
			(make-anf-lambda (annFetchLambdaVars e)
					 (anf-convert-top (annFetchLambdaBody e))))
		       (c (make-anf-var newvar))))))
     ((annIsApp? e)
      (loop (annFetchAppRator e) #f
	    (lambda (conv-rator)
	      (anf-convert-e* (annFetchAppRands e) v
			      (lambda (conv-rands)
				(anf-next-app!
				 (make-anf-app conv-rator conv-rands)))
			      c))))
     ((annIsCtor? e)
      (anf-convert-e* (annFetchCtorArgs e) v
		      (lambda (conv-rands)
			(anf-next-ctor!
			 (make-anf-ctor (annFetchCtorName e)
					conv-rands)))
		      c))
     ((annIsSel? e)
      (anf-convert-e* (list (annFetchSelArg e)) v (lambda (conv-rands)
						    (make-anf-sel (annFetchSelName e)
								  (car conv-rands)))
		      c))
     ((annIsTest? e)
      (anf-convert-e* (list (annFetchTestArg e)) v (lambda (conv-rands)
						     (make-anf-test (annFetchTestName e)
								    (car conv-rands)))
		      c))
     ((annIsRef? e)
      (anf-convert-e* (list (annFetchRefArg e)) v
		      (lambda (conv-rands)
			(anf-next-ref!
			 (make-anf-ref (car conv-rands))))
		      c))
     ((annIsDeref? e)
      (anf-convert-e* (list (annFetchDerefArg e)) v (lambda (conv-rands)
						      (make-anf-deref (car conv-rands)))
		      c))
     ((annIsAssign? e)
      (anf-convert-e* (list (annFetchAssignRef e)
			      (annFetchAssignArg e)) v
		      (lambda (conv-rands)
			(make-anf-assign (car conv-rands) (cadr conv-rands)))
		      c))
     ((annIsCellEq? e)
      (anf-convert-e* (annFetchCellEqArgs e) v
		      (lambda (conv-rands)
			(make-anf-celleq  conv-rands))
		      c))
     ((annIsEval? e)
      (let ((args (list (annFetchEvalBody e))))
	(anf-convert-e* args v (lambda (conv-args)
				 (make-anf-op 'eval conv-args))
			c)))
     ((annIsLift? e)
      (error "Lift ignored"))
     ((annIsMemo? e)
      (error "Memo ignored"))
     (else
      (error 'annFreeVars "Unknown syntax construction")))))
