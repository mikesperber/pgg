;;; cogen-bta
;;; $Id$
;;;

(define *bta-user-memoization* #f)

;;; binding-time analysis
;;; `d*' list of function definitions
;;; `symtab' is an initial symbol table where only constructors,
;;; selectors, and constructor test are defined
;;; `skeleton' function call with arguments replaced by binding times
(define (bta-run d* symtab skeleton def-typesig* def-opsig* def-memo)
  ;;(display "bta-run") (newline)
  (set! *bta-user-memoization*
	(and def-memo
	     (= 3 (length def-memo))
	     (equal? (caddr def-memo) 'ONLY)))
  ;;(display (list "*bta-user-memoization*" *bta-user-memoization*))
  (let* ((goal-proc (car skeleton))
	 (bts (cdr skeleton))
	 (d (annDefLookup goal-proc d*))
	 (formals (annDefFetchProcFormals d))
	 (d0 (annMakeDef '$goal formals
			 (annMakeCall goal-proc (map annMakeVar formals))))
	 (d* (cons d0 d*))
	 (dummy (bta-collect-d symtab d*))
	 (proctv (bta-get-ecr (annDefFetchProcBTVar d0)))
	 (procsig (bta-c-fetch-ctor-args (tv-get-leq-field proctv))))
    (bta-make-dynamic (car procsig))	;goal-proc returns dynamic
    (for-each (bta-assert symtab) bts (cdr procsig))
    (for-each (bta-typesig d* symtab) def-typesig*)
;;;    (pp (bta-display-d d*))
    (bta-solve-d d*)
    d*))
;;; enforce a binding-time type signature
(define (bta-typesig d* symtab)
  (lambda (typesig)
    (let* ((proc (caadr typesig))
	   (bts (cdadr typesig))
	   (bt-result (caddr typesig))
	   (d (annDefLookup proc d*)))
      (if d
	  (let* ((proctv (bta-get-ecr (annDefFetchProcBTVar d)))
		 (procsig (bta-c-fetch-ctor-args (tv-get-leq-field proctv))))
	    ((bta-assert symtab) bt-result (car procsig))
	    (for-each (bta-assert symtab) bts (cdr procsig)))))))
;;; assert binding time `bt' for `tv'
(define (bta-assert symtab)
  (lambda (bt tv0)
    (let loop ((bt bt) (tv tv0))
      (cond
       ((equal? bt 'D)
	(bta-make-dynamic tv))
       ((equal? bt 'S)
	(bta-add-leq tv '***static*** '()))
       ((equal? bt '*)
	(bta-equate tv tv0))
       ((pair? bt)
	;; might want to cross check #args of constructors
	(let* ((ctor (caar bt))
	       (makector (cadr (assoc ctor symtab)))
	       (dummy (makector ctor '()))
	       (desc (annFetchCtorDesc dummy))
	       (nt (desc-nt desc))
	       (type (desc-type desc))
	       (tvs (nlist nt tv-make)))
	  (bta-add-leq tv type tvs)
	  (for-each (lambda (one-ctor)
		 (let* ((ctor (car one-ctor))
			(bts (cdr one-ctor))
			(makector (cadr (assoc ctor symtab)))
			(dummy (makector ctor '()))
			(desc (annFetchCtorDesc dummy))
			(np (desc-np desc))
			(nc (desc-nc desc)))
		   (for-each loop bts (take nc (list-tail tvs np))))) bt)))
       (else
	(error "bta-assert: bad binding time" bt))))))
;;; syntax of constraints
(define (bta-c-make-ctor ctor ps)
  (list ctor ps)) 
(define (bta-c-fetch-ctor c)
  (list-ref c 0))
(define (bta-c-fetch-ctor-args c)
  (list-ref c 1))

;;; union-find

;;; a type variable is represented as a vector 
;;; #(substituted-value pointer-to-father
;;;   list-of-dependent-tvs leq-field lift-field unique-number)
;;; if pointer-to-father is #f then we have an ecr
(define tv-make-counter 0)
(define (tv-make)
  (set! tv-make-counter (+ tv-make-counter 1))
  (vector 0 #f '() #f '() tv-make-counter))
(define (tv-get-counter tv)
  (vector-ref tv 5))
(define (tv-value tv)
  (vector-ref tv 0))
(define (tv-set-value! tv val)
  (vector-set! tv 0 val))
(define (tv-father tv)
  (vector-ref tv 1))
(define (tv-set-father! tv father)
  (vector-set! tv 1 father))
(define (tv-get-dependents tv)
  (vector-ref tv 2))
(define (tv-set-dependents! tv deps)
  (vector-set! tv 2 deps))
(define (tv-add-dependents! tv deps)
  (let ((old-deps (tv-get-dependents tv)))
    (tv-set-dependents! tv (append deps old-deps))))
(define (tv-get-leq-field tv)
  (vector-ref tv 3))
(define (tv-set-leq-field! tv constraint)
  (vector-set! tv 3 constraint))
(define (tv-get-lift-field tv)
  (vector-ref tv 4))
(define (tv-set-lift-field! tv lift)
  (vector-set! tv 4 lift))
(define (tv-add-lift-field! tv lift)
  (tv-set-lift-field! tv (cons lift (tv-get-lift-field tv))))

(define (bta-get-ecr tv)
  (let loop ((tv tv) (descendants '()))
    (let ((father (tv-father tv)))
      (if father
	  (loop father (cons tv descendants))
	  (begin
	    ;; path compression
	    (for-each (lambda (tvd) (tv-set-father! tvd tv)) descendants)
	    tv)))))

;;; always return the old ecr for tv1) (useful to map things to \top)
(define (bta-equate tv1 tv2)
  ;;(display (list "bta-equate" (tv-get-counter tv1) (tv-get-counter tv2))) (newline)
  (let ((ecr1 (bta-get-ecr tv1))
	(ecr2 (bta-get-ecr tv2)))
    ;;(display (list "[bta-equate" (tv-get-counter ecr1) (tv-get-counter ecr2))) 
    (if (eq? ecr1 ecr2)
	ecr1
	(let ((dyn1 (bta-dynamic? ecr1))
	      (dyn2 (bta-dynamic? ecr2)))
	  (if (or dyn1 dyn2)
	      (begin
		(tv-set-father! ecr2 ecr1)
		(if (not dyn1) (bta-make-dynamic ecr1))
		(if (not dyn2) (bta-make-dynamic-internal ecr2)))
	      ;; only if both are static:
	      (begin
		(tv-set-father! ecr2 ecr1)
		(tv-add-dependents! ecr1 (tv-get-dependents ecr2))
		(let ((leq1 (tv-get-leq-field ecr1))
		      (leq2 (tv-get-leq-field ecr2)))
;;;		  (display (list "leq1:" (and leq1 (bta-c-fetch-ctor leq1))
;;;				 "leq2:" (and leq2 (bta-c-fetch-ctor leq2)))) 
		  (if (and leq1 leq2)
		      (if (equal? (bta-c-fetch-ctor leq1)
				  (bta-c-fetch-ctor leq2))
			  (begin
			    (for-each bta-equate
				 (bta-c-fetch-ctor-args leq1)
				 (bta-c-fetch-ctor-args leq2))
;;;			    (display (list "final-leq:"(bta-c-fetch-ctor leq1)))
			    )
			  (begin	; different constraints
			    (bta-make-dynamic ecr1)
			    (bta-make-dynamic ecr2)
;;;			    (display (list "final-leq:" #f))
			    ))
		      ;; at least one of leq1, leq2 is #f
		      (let* ((constraint (or leq1 leq2))
			     (ctor (and constraint
					(bta-c-fetch-ctor
					 constraint)))
			     (lifts1 (tv-get-lift-field ecr1))
			     (lifts2 (tv-get-lift-field ecr2))
			     (lifts0 (if leq1 lifts2 lifts1)))
			(tv-set-leq-field! ecr1 constraint)
			(tv-set-leq-field! ecr2 constraint)
;;;			(display (list "final-leq:" ctor))
			(cond
			 ((not ctor)
			  (tv-set-lift-field!
			   ecr1
			   (append lifts2 lifts1)))
			 ((equal? ctor '***static***)
			  (tv-add-dependents! ecr1 (append lifts1 lifts2))
			  (for-each (lambda (tv)
				      (bta-add-leq tv '***static*** '()))
				      lifts0))
			 (else
			  (for-each (lambda (tv)
				      (bta-equate tv ecr1))
				    lifts0))))))))
	  ecr1))))

(define (bta-dynamic? tv)
  (> (tv-value tv) 0))
(define (bta-dynamize! tv)
  (tv-set-value! tv 1))

(define (bta-make-dynamic tv)
  ;;(display "bta-make-dynamic ") (display (tv-get-counter tv)) (newline)
  (bta-make-dynamic-internal (bta-get-ecr tv)))
(define (bta-make-dynamic-internal ecr)
  (if (bta-dynamic? ecr)
      ecr
      (begin
	(bta-dynamize! ecr)
	(let ((dependencies (tv-get-dependents ecr)))
	  (tv-set-dependents! ecr '())
	  (for-each bta-make-dynamic dependencies))
	(let ((lifts (tv-get-lift-field ecr)))
	  (tv-set-lift-field! ecr '())
	  (for-each (lambda (lift) (bta-equate lift ecr)) lifts))
	(let ((leq-field (tv-get-leq-field ecr)))
	  (tv-set-leq-field! ecr #f)
	  (if leq-field
	      (for-each bta-make-dynamic
		   (bta-c-fetch-ctor-args leq-field)))))))

;;; generate type variables and constraints
(define (bta-collect-d symtab d*)
  (let ((symtab 
	 (append
	  (map (lambda (d)
		 (annDefSetProcBTVar! d (tv-make))
		 (cons (annDefFetchProcName d) (annDefFetchProcBTVar d)))
	       d*)
	  symtab)))
    (map (lambda (d)
	   (let* ((formals (annDefFetchProcFormals d))
		  (argtvs
		   (or (and formals (map (lambda (v) (cons v (tv-make))) formals)) '()))
		  (bodytv
		   (bta-collect (annDefFetchProcBody d)
				(append argtvs symtab)))
		  (proctv
		   (annDefFetchProcBTVar d)))
	     (if formals
		 (bta-add-leq 
		  proctv
		  '-> (cons bodytv (map cdr argtvs)))
		 (bta-equate proctv bodytv))))
	 d*)))

;;; bta-collect returns the outer binding-time type variable of e
(define (bta-collect e symtab)
  (let loop ((e e))
    ;; (display "bta-collect ") (display e) (newline)
    (let ((pi (tv-make)) (po (tv-make)))
      (annExprSetBTi! e pi)
      (annExprSetBTo! e po)
      (tv-add-lift-field! pi po)
      ;; pi and po must be associated with the expression
      (cond
       ((annIsVar? e)
	(bta-equate pi (cdr (assoc (annFetchVar e) symtab))))
       ((annIsConst? e)
	(bta-add-leq pi '***static*** '()))
       ((annIsCond? e)
	(let ((po1 (loop (annFetchCondTest e)))
	      (po2 (loop (annFetchCondThen e)))
	      (po3 (loop (annFetchCondElse e))))
	  (if (not *bta-user-memoization*)
	      (bta-add-dependent po1 pi))
	  (bta-equate pi po2)
	  (bta-equate pi po3)))
       ((annIsOp? e)
	(let* ((pos (map loop (annFetchOpArgs e)))
	       (property (annFetchOpProperty e))
	       (tv-internal (tv-make))
	       (process
		(lambda (phi bt)
		  (cond
		   ((equal? bt '*)
		    (bta-equate phi tv-internal))
		   ((equal? bt 'd)
		    (bta-make-dynamic phi))
		   ((equal? bt '-)
		    '())
		   (else
		    (error "bad type of operator"))))))
	  (bta-add-leq tv-internal '***static*** '())
	  (if property
	      (cond
	       ((member property '(D MEMO))
		(for-each (lambda (phi) (bta-equate tv-internal phi)) pos)
		(bta-equate tv-internal pi)
		(bta-make-dynamic tv-internal))
	       ((pair? property)
		(for-each process pos (car property))
		(process po (cadr property)))
	       (else
		;; an error-primitive
		(bta-add-dependent tv-internal pi)
		(bta-add-dependent pi tv-internal)))
	      ;; no property: standard treatment
	      (begin
		(bta-equate tv-internal pi)
		(for-each (lambda (po) (bta-equate tv-internal po)) pos)))))
       ((annIsCall? e)
	(let ((pos (map loop (annFetchCallArgs e))))
	  (bta-add-leq (cdr (assoc (annFetchCallName e) symtab))
		       '-> (cons pi pos))))
       ((annIsLet? e)
	(let* ((pl (tv-make))
	       (po1 (loop (annFetchLetHeader e)))
	       (po2 (bta-collect (annFetchLetBody e)
				 (cons (cons (annFetchLetVar e) pl) symtab))))
	  (bta-equate pl po1)
	  (bta-equate pi po2)))
       ((annIsLambda? e)
	(let* ((vars (annFetchLambdaVars e))
	       (pvs (map (lambda (v) (tv-make)) vars))
	       (po0 (bta-collect (annFetchLambdaBody e)
				 (append (map cons vars pvs) symtab))))
	  (bta-equate pi po)
	  (bta-add-leq pi '-> (cons po0 pvs))
	  (for-each (lambda (pp) (bta-add-dependent pi pp)) (cons po0 pvs))))
       ((annIsApp? e)
	(let* ((po0 (loop (annFetchAppRator e)))
	       (pos (map loop (annFetchAppRands e))))
	  (bta-add-leq po0 '-> (cons pi pos))
	  (for-each (lambda (pp) (bta-add-dependent po0 pp)) (cons pi pos))))
       ((annIsCtor? e)
	(let* ((pos (map loop (annFetchCtorArgs e)))
	       (ctor (annFetchCtorName e))
	       (ctorDesc (annFetchCtorDesc e))
	       (nr-pre (desc-np ctorDesc))
	       (nr-arg (desc-nc ctorDesc))
	       (nr-post (- (desc-nt ctorDesc) (+ nr-pre nr-arg)))
	       (pp (append (nlist nr-pre tv-make)
			   pos
			   (nlist nr-post tv-make))))
	  (bta-equate pi po)
	  (bta-add-leq pi (desc-type ctorDesc) pp)
	  (for-each (lambda (px) (bta-add-dependent pi px)) pp)))
       ((annIsSel? e)
	(let* ((po (loop (annFetchSelArg e)))
	       (comp (annFetchSelComp e))
	       (desc (annFetchSelDesc e))
	       (nr-pre (desc-np desc))
	       (nr-arg (desc-nc desc))
	       (nr-post (- (desc-nt desc) (+ nr-pre nr-arg))))
	  (bta-add-leq po (desc-type desc)
		       (append (nlist nr-pre tv-make)
			       (nlist (- comp 1) tv-make)
			       (list pi)
			       (nlist (- nr-arg comp) tv-make)
			       (nlist nr-post tv-make)))
	  (bta-add-dependent po pi)))
       ((annIsTest? e)
	(let* ((po (loop (annFetchTestArg e)))
	       (desc (annFetchTestDesc e)))
	  (bta-add-leq po (desc-type desc) (nlist (desc-nt desc) tv-make))
	  (bta-add-leq pi '***static*** '())
	  (bta-add-dependent po pi)))
       ((annIsEval? e)
	(let ((po (loop (annFetchEvalBody e))))
	  (bta-add-dependent po pi))
	(bta-equate po pi))
       (else
	(error "bta-collect: unrecognized syntax")))
      po)))

(define (bta-add-leq tv ctor args)
  (let* ((ecr (bta-get-ecr tv))
	 (leq1 (tv-get-leq-field ecr))
	 (leq2 (bta-c-make-ctor ctor args)))
    (if leq1
	(if (equal? ctor (bta-c-fetch-ctor leq1))
	    (for-each bta-equate args (bta-c-fetch-ctor-args leq1))
	    (begin (bta-make-dynamic-internal ecr)
		   (for-each bta-make-dynamic args)))
	(begin
	  (tv-set-leq-field! ecr leq2)
	  (let ((lifts (tv-get-lift-field ecr)))
	    (tv-set-lift-field! ecr '())
	    (if (equal? ctor '***static***)
		(begin
		  (tv-add-dependents! ecr lifts)
		  (for-each (lambda (pp)
			      (bta-add-leq pp ctor args))
			    lifts))
		(for-each (lambda (pp) (bta-equate pp ecr)) lifts)))
	  ;; maybe mark all further lifts to become equalities?
	  )))) 

(define (bta-add-dependent tv dep)
    (let* ((ecr (bta-get-ecr tv)))
      (if (bta-dynamic? ecr)
	  (bta-make-dynamic dep)
	  (tv-add-dependents! ecr (list dep)))))

;;; display binding-time constraints
(define (bta-display-d d*)
  (map (lambda (d)
	 `(DEFINE (,(annDefFetchProcName d) ,@(annDefFetchProcFormals d))
	    ,(bta-tv->type (annDefFetchProcBTVar d))
	    ,(bta-display (annDefFetchProcBody d)))) d*))

(define (bta-display-tv tv)
  (let ((ecr (bta-get-ecr tv)))
    (list (tv-get-counter ecr)
	  (tv-value ecr)
	  (and (tv-get-leq-field ecr)
	       (bta-c-fetch-ctor (tv-get-leq-field ecr)))
	  (map tv-get-counter (map bta-get-ecr (tv-get-dependents ecr)))
	  (map tv-get-counter (map bta-get-ecr (tv-get-lift-field ecr))))))

(define (bta-display e)
  (let loop ((e e))
    (let ((pi (annExprFetchBTi e)) (po (annExprFetchBTo e)))
      ;; pi and po must be associated with the expression
      (append
       (list (bta-display-tv pi) (bta-display-tv po))
       (cond
	((annIsVar? e)
	 (annFetchVar e))
       ((annIsConst? e)
	(annFetchConst e))
       ((annIsCond? e)
	`(IF ,(loop (annFetchCondTest e))
	     ,(loop (annFetchCondThen e))
	     ,(loop (annFetchCondElse e))))
       ((annIsOp? e)
	`(,(annFetchOpName e)
	  ,@(map loop (annFetchOpArgs e))))
       ((annIsCall? e)
	`(,(annFetchCallName e)
	  ,@(map loop (annFetchCallArgs e))))
       ((annIsLet? e)
	`(LET ((,(annFetchLetVar e) ,(loop (annFetchLetHeader e))))
	   ,(loop (annFetchLetBody e))))
       ((annIsLambda? e)
	`(LAMBDA ,(annFetchLambdaVars e)
	   ,(loop (annFetchLambdaBody e))))
       ((annIsApp? e)
	`(,(loop (annFetchAppRator e))
	  ,@(map loop (annFetchAppRands e))))
       ;; constructor support not yet cast in stone
       ((annIsCtor? e)
	`(,(annFetchCtorName e) ,@(map loop (annFetchCtorArgs e))))
       ((annIsSel? e)
	`(,(annFetchSelName e) ,(loop (annFetchSelArg e))))
       ((annIsTest? e)
	`(,(annFetchTestName e) ,(loop (annFetchTestArg e))))
       ((annIsEval? e)
	`(EVAL ,(loop (annFetchEvalBody e)))))))))

;;; bta-solve-d evaluates the normalized constraint set and inserts
;;; memoization points unless manually overridden.
(define (bta-solve-d d*)
  ;;(display "bta-solve") (newline)
  (for-each (lambda (d)
	      (bta-solve (annDefFetchProcBody d))
	      (let ((bt (bta-tv->type (annDefFetchProcBTVar d))))
		(annDefSetProcBTVar! d bt))) d*)) 

;;; you may win somewhat by using a global DYNAMIC type variable
;;; returns #t for serious expressions
;;; only dynamic ifs and lambdas which guard serious expressions are
;;; considered for memoization points
(define (bta-solve e)
  (let ((pi (bta-get-ecr (annExprFetchBTi e)))
	(po (bta-get-ecr (annExprFetchBTo e))))
    (if (and (not (eq? pi po))
	     (bta-dynamic? po)
	     (not (bta-dynamic? pi)))
	(begin
	  (annExprSetBTo! e pi)
	  (annIntroduceLift e 0 1)
	  (bta-solve (annFetchLiftBody e)))
	(begin
	  (annExprSetLevel! e (tv-value pi))
	  ;;(annExprSetBTi! e (tv-get-counter pi))
	  ;;(annExprSetBTo! e (tv-get-counter po))
	  (cond
	   ((annIsVar? e)
	    #f)
	   ((annIsConst? e)
	    #f)
	   ((annIsCond? e)
	    (let* ((e-test (annFetchCondTest e))
		   (dyn-test (bta-dynamic? (bta-get-ecr
					    (annExprFetchBto
					     e-test))))
		   (r-test (bta-solve e-test))
		   (r-then (bta-solve (annFetchCondThen e)))
		   (r-else (bta-solve (annFetchCondElse e))))
	      ;;(and dyn-test)
	      (if (or r-test r-then r-else)
		(begin
		  (if (not *bta-user-memoization*)
		      (annIntroduceMemo e
					(annExprFetchLevel e-test)
					(annFreeVars e)))
		  (not dyn-test))
		#f)))
	   ((annIsOp? e)
	    (let ((args (map bta-solve (annFetchOpArgs e)))
		  (property (annFetchOpProperty e)))
	      (if property
		  (cond
		   ((equal? property 'MEMO)
		    (let* ((args (annFetchOpArgs e))
			   (lv (annFetchConst (car args)))
			   (body (cadr args)))
		      (annIntroduceMemo1 e lv (annFreeVars body) body)
		      #f))
		   (else
		    (any? args)))
		  (any? args))))
	   ((annIsCall? e)
	    (for-each bta-solve (annFetchCallArgs e))
	    #t)
	   ((annIsLet? e)
	    (let ((header (bta-solve (annFetchLetHeader e)))
		  (body (bta-solve (annFetchLetBody e))))
	      (or header body)))
	   ((annIsLambda? e)
	    (let* ((body (bta-solve (annFetchLambdaBody e)))
		   (dyn-lambda
		    (bta-dynamic? (bta-get-ecr (annExprFetchBto e)))))
	      (if (and body (not *bta-user-memoization*))
		  (annIntroduceMemo e (annExprFetchLevel e) (annFreeVars e))))
	    #f)
	   ((annIsApp? e)
	    (bta-solve (annFetchAppRator e))
	    (for-each bta-solve (annFetchAppRands e))
	    #t)
	   ((annIsCtor? e)
	    (strict-or-map bta-solve (annFetchCtorArgs e)))
	   ((annIsSel? e)
	    (bta-solve (annFetchSelArg e)))
	   ((annIsTest? e)
	    (bta-solve (annFetchTestArg e)))
	   ((annIsEval? e)
	    (bta-solve (annFetchEvalBody e))))))))

;;; mainly for debugging
;;; (re)construct the binding-time type of a program point
(define (bta-tv->type tv)
  (let loop ((tv tv) (seen-tvs '()))
    (let ((tv (bta-get-ecr tv)))
      (cons
       (tv-get-counter tv)
       (if (bta-dynamic? tv)
	   'D
	   (let ((leq (tv-get-leq-field tv)))
	     (if leq
		 (let ((ctor (bta-c-fetch-ctor leq))
		       (args (bta-c-fetch-ctor-args leq)))
		   (cond
		    ((equal? ctor '***static***)
		     'S)
		    ((member tv seen-tvs)
		     '*)
		    (else
		     (let* ((seen-tvs (cons tv seen-tvs))
			    (local-loop (lambda (tv) (loop tv seen-tvs))))
		       (if (equal? ctor '->)
			   (append (map local-loop (cdr args))
				   (list ctor (local-loop (car args))))
			   (cons ctor (map local-loop (cdr args)))
			   )))))
		 'S))))))) 
