;;; cogen-bta
;;; $Id$
;;; $Log$
;;; Revision 1.3  1995/10/23 16:52:48  thiemann
;;; continuation based reduction works
;;;

;;; binding-time analysis
;;; `d*' list of function definitions
;;; `symtab' is an initial symbol table where only constructors,
;;; selectors, and constructor test are defined
;;; `skeleton' function call with arguments replaced by binding times
(define (bta-run d* symtab skeleton)
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
    (map (bta-assert symtab) bts (cdr procsig))
;;;    (pp (bta-display-d d*))
    (bta-solve-d d*)
    d*
    ))
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
	  (map (lambda (one-ctor)
		 (let* ((ctor (car one-ctor))
			(bts (cdr one-ctor))
			(makector (cadr (assoc ctor symtab)))
			(dummy (makector ctor '()))
			(desc (annFetchCtorDesc dummy))
			(np (desc-np desc)))
		   (map loop bts (list-tail tvs np)))) bt)))))))
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
	    (map (lambda (tvd) (tv-set-father! tvd tv)) descendants)
	    tv)))))

;;; SOMETHING GOES WRONG HERE WHILE PROCESSING LIFT CONSTRAINTS
;;; always return the old ecr for tv1) (useful to map things to \top)
(define (bta-equate tv1 tv2)
  (let ((ecr1 (bta-get-ecr tv1))
	(ecr2 (bta-get-ecr tv2)))
    (if (eq? ecr1 ecr2)
	ecr1
	(let ((dyn1 (bta-dynamic? ecr1))
	      (dyn2 (bta-dynamic? ecr2)))
	  (if (or dyn1 dyn2)
	      (begin
		(if (not dyn1) (bta-make-dynamic ecr1))
		(if (not dyn2) (bta-make-dynamic ecr2))
		(tv-set-father! ecr2 ecr1))
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
			    (map bta-equate
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
					(bta-c-fetch-ctor constraint))))
			(tv-set-leq-field! ecr1 constraint)
			(tv-set-leq-field! ecr2 constraint)
;;;			(display (list "final-leq:" ctor))
			(if (or (not ctor)
				(equal? ctor '***static***))
			    (tv-set-lift-field!
			     ecr1
			     (append (tv-get-lift-field ecr2)
				     (tv-get-lift-field ecr1)))
			    (begin
			      (if leq1
				  (map (lambda (tv)
					 (bta-equate tv ecr1))
				       (tv-get-lift-field ecr2)))
			      (if leq2
				  (map (lambda (tv)
					 (bta-equate tv ecr1))
				       (tv-get-lift-field ecr1))))))))))
	  ecr1))))

(define (bta-dynamic? tv)
  (> (tv-value tv) 0))
(define (bta-dynamize! tv)
  (tv-set-value! tv 1))

(define (bta-make-dynamic tv)
  ;;(display "[bta-make-dynamic ") (display (tv-get-counter tv)) (newline)
  (let ((ecr (bta-get-ecr tv)))
    (if (bta-dynamic? ecr)
	ecr
	(begin
	  (bta-dynamize! ecr)
	  (map bta-make-dynamic (tv-get-dependents ecr))
	  (let ((lifts (tv-get-lift-field ecr)))
	    (tv-set-lift-field! ecr '())
	    (map (lambda (lift) (bta-equate ecr lift)) lifts))
	  (if (tv-get-leq-field ecr)
	      (map bta-make-dynamic
		   (bta-c-fetch-ctor-args (tv-get-leq-field ecr)))))))
  ;;(display "]") (newline)
  )

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
	   (let* ((argtvs
		   (map (lambda (v)
			  (cons v (tv-make)))
			(annDefFetchProcFormals d)))
		  (bodytv
		   (bta-collect (annDefFetchProcBody d)
				(append argtvs symtab)))
		  (proctv
		   (annDefFetchProcBTVar d)))
	     (bta-add-leq 
	      proctv
	      '-> (cons bodytv (map cdr argtvs)))))
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
	  (bta-add-dependent po1 pi)
	  (bta-equate pi po2)
	  (bta-equate pi po3)))
       ((annIsOp? e)
	(let ((pos (map loop (annFetchOpArgs e))))
	  (map (lambda (po) (bta-equate pi po)) pos)
	  (bta-add-leq pi '***static*** '())))
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
	  (map (lambda (pp) (bta-add-dependent pi pp)) (cons po0 pvs))))
       ((annIsApp? e)
	(let* ((po0 (loop (annFetchAppRator e)))
	       (pos (map loop (annFetchAppRands e))))
	  (bta-add-leq po0 '-> (cons pi pos))
	  (map (lambda (pp) (bta-add-dependent po0 pp)) (cons pi pos))))
       ;; constructor support not yet cast in stone
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
	  (map (lambda (px) (bta-add-dependent pi px)) pp)))
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
	  (bta-add-dependent po pi))))
      po)))

(define (bta-add-leq tv ctor args)
  (let* ((ecr (bta-get-ecr tv))
	 (leq1 (tv-get-leq-field ecr))
	 (leq2 (bta-c-make-ctor ctor args)))
    (if leq1
	(if (equal? (bta-c-fetch-ctor leq1)
		    (bta-c-fetch-ctor leq2))
	    (map bta-equate
		 (bta-c-fetch-ctor-args leq1)
		 (bta-c-fetch-ctor-args leq2))
	    (bta-make-dynamic ecr))
	(begin
	  (tv-set-leq-field! ecr leq2)
	  (if (not (equal? ctor '***static***))
	      (map (lambda (pp) (bta-equate pp ecr))
		   (tv-get-lift-field ecr)))
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
	`(,(annFetchTestName e) ,(loop (annFetchTestArg e)))))))))

;;; bta-solve-d evaluates the normalized constraint set
(define (bta-solve-d d*)
  (map (lambda (d)
	 (bta-solve (annDefFetchProcBody d))
	 (let ((bt (bta-tv->type (annDefFetchProcBTVar d))))
	   (annDefSetProcBTVar! d bt))) d*)) 

;;; you may win somewhat by using a global DYNAMIC type variable
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
	  (annExprSetBTi! e (tv-get-counter pi))
	  (annExprSetBTo! e (tv-get-counter po))
	  (cond
	   ((annIsVar? e)
	    #t)
	   ((annIsConst? e)
	    #t)
	   ((annIsCond? e)
	    (let* ((e-test (annFetchCondTest e))
		   (dyn-test (bta-dynamic? (bta-get-ecr (annExprFetchBto e-test)))))
	      (bta-solve e-test)
	      (bta-solve (annFetchCondThen e))
	      (bta-solve (annFetchCondElse e))
	      ;;(if dyn-test
	      (annIntroduceMemo e
				(annExprFetchLevel e-test)
				(annFreeVars e))
	      ;;)
	      ))
	   ((annIsOp? e)
	    (map bta-solve (annFetchOpArgs e)))
	   ((annIsCall? e)
	    (map bta-solve (annFetchCallArgs e)))
	   ((annIsLet? e)
	    (bta-solve (annFetchLetHeader e))
	    (bta-solve (annFetchLetBody e)))
	   ((annIsLambda? e)
	    (bta-solve (annFetchLambdaBody e)))
	   ((annIsApp? e)
	    (bta-solve (annFetchAppRator e))
	    (map bta-solve (annFetchAppRands e)))
	   ((annIsCtor? e)
	    (map bta-solve (annFetchCtorArgs e)))
	   ((annIsSel? e)
	    (bta-solve (annFetchSelArg e)))
	   ((annIsTest? e)
	    (bta-solve (annFetchTestArg e))))))))

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