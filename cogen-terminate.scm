;;; cogen-terminate

;;; copyright © 1996-2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; termination analysis

;;; a graph abstraction
(define-record simple-graph
  (edges))
(define-record graph-edge
  (source target))
(define-record composite-graph
  (operator g1 g2))

(define (new-graph)
  (make-simple-graph '()))
(define (simple-graph-add-edge! g s t)
  (let ((edges (simple-graph->edges g))
	(new-edge (cons s t)))
    (if (member new-edge edges)
	#f
	(begin (simple-graph->edges! g (cons new-edge edges))
	       #t))))
(define (simple-graph-is-edge? g s t)
  (member (cons s t) (simple-graph->edges g)))
(define (simple-graph-sources/targets g get-source get-target x)
  (let loop ((edges (simple-graph->edges g))
	     (sources/targets '()))
    (if (null? edges)
	sources/targets
	(let ((edge (car edges))
	      (edges (cdr edges)))
	  (if (and (eq? (get-source edge) x)
		   (not (memq (get-target edge) sources/targets)))
	      (loop edges (cons (get-target edge) sources/targets))
	      (loop edges sources/targets))))))
(define (simple-graph-targets g s)
  (simple-graph-sources/targets g car cdr s))
(define (simple-graph-sources g t)
  (simple-graph-sources/targets g cdr car t))

(define (union-graph g1 g2)
  (make-composite-graph 'union g1 g2))
(define (intersection-graph g1 g2)
  (make-composite-graph 'intersection g1 g2))
(define (compose-graph g1 g2)
  (make-composite-graph 'compose g1 g2))
(define (closure-graph g1)		;reflexive-transitive closure
  (make-composite-graph 'closure g1 #f))
(define (converse-graph g1)
  (make-composite-graph 'converse g1 #f))

(define (is-edge? g s t)
  (cond
   ((simple-graph? g)
    (simple-graph-is-edge? g s t))
   ((composite-graph? g)
    (case (composite-graph->operator g)
      ((union) (or (is-edge? (composite-graph->g1 g) s t)
		   (is-edge? (composite-graph->g2 g) s t)))
      ((intersection) (and (is-edge? (composite-graph->g1 g) s t)
			   (is-edge? (composite-graph->g2 g) s t)))
      ((compose) (let ((xs (targets (composite-graph->g1 g) s))
		       (g2 (composite-graph->g2 g)))
		   (or-map (lambda (x) (is-edge? g2 x t)) xs)))
      ((converse) (is-edge? (composite-graph->g1 g) t s))
      ((closure) (let loop ((s s) (visited '()) (c (lambda (visited) #f)))
		   (or (eq? s t)
		       (if (memq s visited)
			   (c visited)
			   (let inner-loop
			       ((xs (targets (composite-graph->g1 g) s))
				(visited (cons s visited)))
				(if (null? xs)
				    (c visited)
				    (let ((x (car xs))
					  (xs (cdr xs)))
				      (loop x visited
					    (lambda (visited)
					      (inner-loop xs visited))))))))))
      (else
       (error 'is-edge? "unknown graph composition"))))
   (else
    (error 'is-edge? "unknown graph type"))))

(define (sources/targets g get-source get-target x)
  (let s/t ((g g) (x x))
    (cond
     ((simple-graph? g)
      (simple-graph-sources/targets g get-source get-target x))
     ((composite-graph? g)
      (case (composite-graph->operator g)
	((union) (set-union (s/t (composite-graph->g1 g) x)
			    (s/t (composite-graph->g2 g) x)))
	((intersection) (set-intersection (s/t (composite-graph->g1 g) x)
					  (s/t (composite-graph->g2 g) x)))
	((compose) (let ((ys (s/t (composite-graph->g1 g) x))
			 (g2 (composite-graph->g2 g)))
		     (apply set-union (map (lambda (y) (s/t g2 y)) ys))))
	((closure) (let loop ((x x) (visited '()) (c (lambda (visited) visited)))
		     (if (memq x visited)
			 (c visited)
			 (let inner-loop ((ys (s/t (composite-graph->g1 g) x))
					  (visited (cons x visited)))
			   (if (null? ys)
			       (c visited)
			       (let ((y (car ys))
				     (ys (cdr ys)))
				 (loop y visited
				       (lambda (visited)
					 (inner-loop ys visited)))))))))
	((converse) (sources/targets g get-target get-source x))
	(else
	 (error 'sources/targets "unknown graph composition"))))
      (else
       (error 'sources/targets "unknown graph type")))))

(define (sources g t)
  (sources/targets g cdr car t))
(define (targets g s)
  (sources/targets g car cdr s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; specific part
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; flow0 is the basic flow graph
;;; flow1 are the "put into structure" flow arcs
;;; [std flow graph = flow0 U flow1]
;;; fincr are the increasing arcs (flow1 also counts as increasing)
;;; fdecr are the decreasing arcs
;;; fctrl are the control arcs

(define *flow0* 'undefined)
(define *flow1* 'undefined)
(define *fincr* 'undefined)
(define *fdecr* 'undefined)
(define *fctrl* 'undefined)

(define *number* 'undefined)
(define (next-number)
  (set! *number* (+ 1 *number*))
  *number*)
(define get-number car)
(define get-expression cadr)

(define *calls* 'undefined)
(define (add-to-calls! item)
  (set! *calls* (cons item *calls*))
  item)
(define *lambdas* 'undefined)
(define (add-to-lambdas! item)
  (set! *lambdas* (cons item *lambdas*))
  item)
(define *apps* 'undefined)
(define (add-to-apps! item)
  (set! *apps* (cons item *apps*))
  item)
(define *ctors* 'undefined)
(define (add-to-ctors! item)
  (set! *ctors* (cons item *ctors*))
  item)
(define *sels* 'undefined)
(define (add-to-sels! item)
  (set! *sels* (cons item *sels*))
  item)
(define *done* 'undefined)
(define (add-to-done! item)
  (set! *done* (cons item *done*))
  item)
(define *vars* 'undefined)
(define (add-to-vars! item)
  (set! *vars* (cons item *vars*))
  item)
(define (remove-vars! vars)
  (let loop ((old-vars *vars*) (new-vars '()))
    (if (null? old-vars)
	(set! *vars* new-vars)
	(if (memq (caar old-vars) vars)
	    (loop (cdr old-vars) new-vars)
	    (loop (cdr old-vars) (cons (car old-vars) new-vars))))))

(define (main-initialize!)
  (set! *number* 0)
  (set! *flow0* (new-graph))
  (set! *flow1* (new-graph))
  (set! *fincr* (new-graph))
  (set! *fdecr* (new-graph))
  (set! *fctrl* (new-graph))
  (set! *calls* '())
  (set! *lambdas* '())
  (set! *apps* '())
  (set! *ctors* '())
  (set! *sels* '())
  (set! *done* '())
  (set! *vars* '()))

(define (numerate d*)
  (map numerate-d d*))
;;; need to establish the flow from binding occurrences of variables
;;; to bound occurrences (using flow0 or flow1, inside lambdas)
(define (numerate-d d)
  (set! *vars* '())
  (let* ((formals (annDefFetchProcFormals d))
	 (formal-numbers
	  (map (lambda (var)
		 (edge-function *flow0* (next-number)))
	       formals))
	 (body (annDefFetchProcBody d))
	 (body-numbered (numerate-e body (extend-env* formals
						      formal-numbers
						      the-empty-env))))
    `(,(annDefFetchProcName d) ,formal-numbers ,body-numbered)))
(define (change-edge fun)
  (edge-function *flow1* (fun #f)))
(define (edge-function flow number)
  (lambda (e-number)
    (if e-number
	(simple-graph-add-edge! flow number e-number)
	number)))
(define (numerate-e e env)
  (let loop ((e e))
    (cond
     ((annIsVar? e)
      (let ((e-number (next-number))
	    (fun! (apply-env env (annFetchVar e)
			     (lambda () (error "numerate-e")))))
	(fun! e-number)
	(add-to-vars! (cons (annFetchVar e) e-number))
	`(,e-number ,e)))
     ((annIsConst? e)
      `(,(next-number) ,e))
     ((annIsCond? e)
      (let* ((e-number (next-number))
	     (e-test (loop (annFetchCondTest e)))
	     (e-then (loop (annFetchCondThen e)))
	     (e-else (loop (annFetchCondElse e))))
	(simple-graph-add-edge! *flow0* (get-number e-then) e-number)
	(simple-graph-add-edge! *flow0* (get-number e-else) e-number)
	(simple-graph-add-edge! *fctrl* e-test e-number)
	`(,e-number ,e ,e-test ,e-then ,e-else)))
     ((annIsOp? e)
      (let* ((e-number (next-number))
	     (e-args (map loop (annFetchOpArgs e))))
	(for-each (lambda (e-arg)
		    (simple-graph-add-edge!
		     *flow0* (get-number e-arg) e-number)) e-args)
	`(,e-number ,e ,e-args)))
     ((annIsCall? e)
      (let* ((e-number (next-number))
	     (e-args (map loop (annFetchCallArgs e))))
	(add-to-calls!
	 `(,e-number ,e ,e-args))))
     ((annIsLet? e)
      (let* ((e-number (next-number))
	     (v-number (next-number))
	     (e-header (loop (annFetchLetHeader e)))
	     (var (annFetchLetVar e))
	     (e-body (numerate-e (annFetchLetBody e)
				 (extend-env var
					     (edge-function *flow0* v-number)
					     env))))
	(simple-graph-add-edge! *flow0* (get-number e-header) v-number)
	(simple-graph-add-edge! *flow0* (get-number e-body) e-number)
	(remove-vars! (list var))
	`(,e-number ,e ,v-number ,e-header ,e-body))) 
     ((annIsBegin? e)
      (let* ((e-number (next-number))
	     (e-header (loop (annFetchBeginHeader e)))
	     (e-body (numerate-e (annFetchLetBody e)
				 env)))
	(simple-graph-add-edge! *flow0* (get-number e-body) e-number)
	`(,e-number ,e ,e-header ,e-body))) 
     ((annIsVLambda? e)
      (let* ((e-number (next-number))
	     (fixed-vars (annFetchVLambdaFixedVars e))
	     (f-numbers (map (lambda (y) (next-number)) fixed-vars))
	     (var (annFetchVLambdaVar e))
	     (v-number (next-number))
	     (e-body (numerate-e (annFetchVLambdaBody e)
				 (extend-env*
				  fixed-vars
				  (map (lambda (f-number)
					 (edge-function *flow0* f-number))
				       f-numbers) 
				  (extend-env var
					      (edge-function v-number)
					      (map-env change-edge env))))))
	(remove-vars! (cons var fixed-vars))
	;;; IGNORED BY THE ANALYSIS
	`(,e-number ,e ,f-numbers ,v-number ,e-body)))
     ((annIsLambda? e)
      (let* ((e-number (next-number))
	     (vars (annFetchLambdaVars e))
	     (f-numbers (map (lambda (y) (next-number)) vars))
	     (level (annExprFetchLevel e))
	     (e-body (numerate-e
		      (annFetchLambdaBody e)
		      (extend-env* vars
				   (map (lambda (f-number)
					  (edge-function *flow0* f-number))
					f-numbers)
				   (if (zero? level)
				       (map-env change-edge env)
				       env)))))
	      (remove-vars! vars)
	      (if (zero? level)
		  (add-to-lambdas!
		   `(,e-number ,e ,f-numbers ,e-body ,(map cdr *vars*)))
		  `(,e-number ,e f-numbers ,e-body))))
     ((annIsApp? e)
      (let* ((e-number (next-number))
	     (rator (annFetchAppRator e))
	     (e-rator (loop rator))
	     (e-rands (map loop (annFetchAppRands e))))
	(if (zero? (annExprFetchLevel rator))
	    (add-to-apps!
	     `(,e-number ,e ,e-rator ,e-rands))
	    `(,e-number ,e ,e-rator ,e-rands))))
     ((annIsCtor? e)
      (let* ((e-number (next-number))
	     (e-args (map loop (annFetchCtorArgs e))))
	(for-each
	 (lambda (arg)
	   (simple-graph-add-edge! *fincr* (get-number arg) e-number))
	 e-args)
	(add-to-ctors!
	 `(,e-number ,e ,e-args))))
     ((annIsSel? e)
      (let* ((e-number (next-number))
	     (e-arg (loop (annFetchSelArg e))))
	(simple-graph-add-edge! *fdecr* e-number (get-number e-arg))
	(add-to-sels!
	 `(,e-number ,e ,e-arg))))
     ((annIsTest? e)
      `(,(next-number) ,e ,(loop (annFetchTestArg e))))
     ;; NO ATTEMPT IS MADE TO TRACE FLOW THROUGH REFERENCES
     ((annIsRef? e)
      `(,(next-number) ,e ,(loop (annFetchRefArg e))))
     ((annIsDeref? e)
      `(,(next-number) ,e ,(loop (annFetchDerefArg e))))
     ((annIsAssign? e)
      `(,(next-number) ,e ,(loop (annFetchAssignArg e))))
     ((annIsCellEq? e)
      `(,(next-number) ,e ,(map loop (annFetchCellEqArgs e))))
     ((annIsLift? e)
      `(,(next-number) ,e ,(loop (annFetchLiftBody e))))
     ((annIsEval? e)
      `(,(next-number) ,e ,(loop (annFetchEvalBody e))))
     ((annIsMemo? e)
      (let* ((e-number (next-number))
	     (e-body (loop (annFetchMemoBody e))))
	(simple-graph-add-edge! *flow0* (get-number e-body) e-number)
	`(,e-number ,e ,e-body)))
     (else
      (error 'numerate-e "Unknown syntax construction")))))

(define (connect-procedure-calls n-d*)
  (for-each
   (lambda (n-expr)
     (let ((e (get-expression n-expr)))
       (if (annIsCall? e)
	   (let* ((name (annFetchCallName e))
		  (n-d (assoc name n-d*))
		  (formal-numbers (cadr n-d))
		  (body-number (get-number (caddr n-d)))
		  (call-number (get-number n-expr))
		  (actual-numbers (caddr n-expr)))
	     (simple-graph-add-edge! *flow0* body-number call-number)
	     (for-each (lambda (actual-number formal-number)
			 (simple-graph-add-edge! *flow0* actual-number formal-number))
		       actual-numbers formal-numbers)))))
   *calls*))

(define (propagate-lambdas-one-step)
  (let ((flow-graph (closure-graph (union-graph *flow0* *flow1*))))
    (for-each
     (lambda (n-expr)
       (let ((e (get-expression n-expr))
	     (e-number (get-number n-expr)))
	 (if (annIsApp? e)
	     (let* ((rator-number (get-number (caddr n-expr)))
		    (ss (sources flow-graph rator-number)))
	       (for-each
		(lambda (s)
		  (let ((n-lambda (assoc s *lambdas*)))
		    (if n-lambda
			(if (not (member (cons rator-number s) *done*))
			    (let ((lambda-body (get-number (cadddr n-lambda)))
				  (formal-numbers (caddr n-lambda))
				  (actual-numbers (map get-number (cadddr n-expr))))
			      (add-to-done! (cons rator-number s))
			      (simple-graph-add-edge! *flow0* lambda-body e-number)
			      (for-each
			       (lambda (actual-number formal-number)
				 (simple-graph-add-edge! *flow0* actual-number formal-number))
			       actual-numbers formal-numbers)
			      (for-each
			       (lambda (free-var-number)
				 (simple-graph-add-edge! *fdecr* rator-number free-var-number))
			       (car (cddddr n-lambda))))))))
		ss)))))
     *apps*)))

(define (propagate-ctors-one-step)
  (let ((flow-graph (closure-graph (union-graph *flow0* *flow1*))))
    (for-each
     (lambda (n-expr)
       (let ((e (get-expression n-expr))
	     (e-number (get-number n-expr)))
	 (if (annIsSel? e)
	     (let* ((arg-number (get-number (caddr n-expr)))
		    (ss (sources flow-graph arg-number)))
	       (for-each
		(lambda (s)
		  (let ((n-ctor (assoc s *ctors*)))
		    (if n-ctor
			(if (not (member (cons arg-number s) *done*))
			    (let* ((ctor-name (annFetchCtorName (cadr n-ctor)))
				   (ctor-selc (annFetchSelCtor e)))
			      (add-to-done! (cons arg-number s))
			      (if (eq? ctor-name ctor-selc)
				  (let*
				      ((ctor-args (map get-number (caddr n-ctor)))
				       (n-ctor-arg (list-ref ctor-args
							     (annFetchSelComp e))))
				    (simple-graph-add-edge! *flow1*
							    n-ctor-arg
							    e-number))))))))
		ss)))))
     *sels*)))

(define (propagate-flows-fix)
  (let loop ((done *done*))
    (propagate-lambdas-one-step)
    (propagate-ctors-one-step)
    (if (not (eq? done *done*))
	(loop *done*))))

(define (perform-termination-analysis d*)
  (if *termination-analysis*
      (begin
	(main-initialize!)
	(let ((n-d* (numerate d*)))
	  (connect-procedure-calls n-d*)
	  (propagate-flows-fix)
	  (scan-procedure-calls n-d*)))))

(define (scan-procedure-calls n-d*)
  (let* ((decreasing-pos (closure-graph (union-graph *flow0* *fdecr*)))
	 (increasing-transition (union-graph *flow1* *fincr*))
	 (weakly-decreasing-neg (compose-graph (closure-graph *flow0*)
					       (compose-graph increasing-transition 
							      (closure-graph (union-graph *flow0* increasing-transition)))))
	 (decreasing-neg (closure-graph (union-graph *flow0* (union-graph *flow1* *fincr*))))
	 (may-share-transition (union-graph (union-graph *flow0* *flow1*)
					    (union-graph *fincr* *fdecr*)))
	 (may-share (closure-graph may-share-transition))
	 (may-depend (closure-graph (union-graph may-share-transition *fctrl*))))
    (for-each
     (lambda (n-expr)
       (let ((e (get-expression n-expr)))
	 (if (annIsCall? e)
	     (let* ((name (annFetchCallName e))
		    (actuals (annFetchCallArgs e))
		    (n-d (assoc name n-d*))
		    (formal-numbers (cadr n-d))
		    (actual-numbers (caddr n-expr))
		    (argc (length formal-numbers))
		    (characteristic
		     (map
		      ;; check for non weakly decreasing transition
		      (lambda (formal-number actual-number)
			(list
			 (is-edge? decreasing-pos formal-number actual-number)
			 (is-edge? weakly-decreasing-neg formal-number actual-number)
			 (is-edge? decreasing-neg formal-number actual-number)
			 (is-edge? may-depend formal-number actual-number)))
		      formal-numbers actual-numbers))
		    (may-increase
		     (map (lambda (ccc) (not (and (car ccc) (not (cadr ccc))))) characteristic))
		    (must-decrease
		     (map (lambda (ccc) (and (car ccc) (not (caddr ccc)))) characteristic))
		    (may-share-info
		     (list->vector
		      (map (lambda (formal-number)
			     (list->vector
			      (map (lambda (actual-number)
				   (is-edge? may-share formal-number actual-number))
				   actual-numbers)))
			   formal-numbers)))
		    (may-depend-info
		     (list->vector
		      (map (lambda (formal-number)
			     (list->vector
			      (map (lambda (actual-number)
				     (is-edge? may-depend formal-number actual-number))
				   actual-numbers)))
			   formal-numbers))))
	       (display (list ">>> checking call" (ann-dsp-e e)
			      characteristic may-increase must-decrease)) (newline)
	       (if (list-or may-increase)
		   (begin
		     (display (list "may increase")) (newline)
		     (if (list-or must-decrease)
			 (begin
			   (display (list "there exists a must-decrease argument") (newline))
			   (let loop ((must-decrease must-decrease) (i 0))
			     (if (null? must-decrease)
				 (begin
				   (display (list "all must-decrease arguments checked") (newline)))
				 (if (car must-decrease)
				     (let ((share #f) (depend #f))
				       ;; does it share with another argument?
				       (let s-loop ((j 0))
					 (if (< j argc)
					     (begin
					       (if (and (not (= i j))
							(vector-ref i
							(vector-ref j
							may-share-info)))
						   (begin
						     (set! share j)
						     (display (list
						     "formal#" j
						     "actual#" i "may share"))
						     (newline)))
					       (s-loop (+ j 1)))))

				       ;; does is depend on an increasing argument?
				       (let d-loop ((j 0))
					 (if (< j argc)
					     (begin
					       (if (and (not (= i j))
							(vector-ref i
							(vector-ref j
							may-depend-info)))
						   (begin
						     (display (list
						     "actual#" i "may depend on"
						     "formal#" j
						     ))
						     (newline)
						     (let ((formal-j (list-ref formal-numbers j)))
						       (if
							(not
							 (and
							  (is-edge? decreasing-pos formal-j formal-j)
							  (not (is-edge? weakly-decreasing-neg formal-j formal-j))))
							(begin
							  (set! depend j)
							  (display (list "which may be increasing"))
							  (newline))
							))))
					       (d-loop (+ j 1)))))
				       (display (list "argument#" i "share" share "depend" depend))
				       (newline))
				     (loop (cdr must-decrease) (+ i 1)))))))))))))
     *calls*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; unused, as far as I can see...
(define (annsubstitute old->new e)
  (let loop ((e e))
    (cond
     ((annIsVar? e)
      (cond
       ((assoc (annFetchVar e) old->new)
	=> (lambda (o/n) (annMakeVar (cdr o/n))))
       (else
	e)))
     ((annIsConst? e)
      e)
     ((annIsCond? e)
      (annMakeCond (loop (annFetchCondTest e))
		   (loop (annFetchCondThen e))
		   (loop (annFetchCondElse e))))
     ((annIsOp? e)
      (annMakeOp (annFetchOpName e) (map loop (annFetchOpArgs e))))
     ((annIsCall? e)
      (annMakeCall (annFetchCallName e) (map loop (annFetchCallArgs e))))
     ((annIsLet? e)
      (annMakeLet (annFetchLetVar e)
		  (loop (annFetchLetHeader e))
		  (loop (annFetchLetBody e))))
     ((annIsBegin? e)
      (annMakeLet (loop (annFetchBeginHeader e))
		  (loop (annFetchBeginBody e))))
     ((annIsVLambda? e)
      (annMakeVLambda (annFetchVLambdaLabel e)
		      (annFetchVLambdaFixedVars e)
		      (annFetchVLambdaVar e)
		      (loop (annFetchVLambdaBody e))))
     ((annIsLambda? e)
      (annMakeLambda (annFetchLambdaLabel e)
		     (annFetchLambdaVars e)
		     (loop (annFetchLambdaBody e))
		     (annFetchLambdaPoly e)))
     ((annIsApp? e)
      (annMakeApp (loop (annFetchAppRator e))
		  (map loop (annFetchAppRands e))))
     ((annIsCtor? e)
      (annMakeCtor (annFetchCtorName e)
		   (annFetchCtorLabel e)
		   (annFetchCtorDesc e)
		   (map loop (annFetchCtorArgs e))))
     ((annIsSel? e)
      (annMakeSel (annFetchSelName e)
		  (annFetchSelCtor e)
		  (annFetchSelDesc e)
		  (annFetchSelComp e)
		  (loop (annFetchSelArg e))))
     ((annIsTest? e)
      (annMakeTest (annFetchTestName e)
		   (annFetchTestDesc e)
		   (loop (annFetchTestArg e))))
     ((annIsRef? e)
      (annMakeRef (annFetchRefLabel e)
		  (loop (annFetchRefArg e))))
     ((annIsDeref? e)
      (annMakeDeref (loop (annFetchDerefArg e))))
     ((annIsAssign? e)
      (annMakeAssign (annFetchAssignLabel e)
		     (loop (annFetchAssignRef e))
		     (loop (annFetchAssignArg e))))
     ((annIsCellEq? e)
      (annMakeCellEq (map loop (annFetchCellEqArgs e))))
     ((annIsLift? e)
      (annMakeLift (annFetchLiftDiff e)
		   (loop (annFetchLiftBody e))))
     ((annIsEval? e)
      (annMakeEval #f (list (loop (annFetchEvalBody e)))))
     ((annIsMemo? e)
      (annMakeMemo (loop (annFetchMemoBody e))))
     (else
      (error 'annsubstitute "Unknown syntax construction")))))
