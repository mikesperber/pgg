;;; binding-time analysis based on annotated type systems


;;; step 0
;;; the driver for the bta

(define *bta-user-memoization* #f)
(define *bta-max-bt* #f)
(define *bta-dynamic-points* '())

;;; debugging and trace aids
(define *bta-display-level* 1)
(define-syntax debug-level
  (syntax-rules ()
    ((_ level arg ...)
     (if (>= *bta-display-level* level)
	 (begin arg ...)))))

(define (bta-note-dynamic! ann)		;;unused
  (set! *bta-dynamic-points* (cons ann *bta-dynamic-points*)))

;;; binding-time analysis
;;; `d*' list of function definitions
;;; `symtab' is an initial symbol table where only constructors,
;;; selectors, and constructor tests are defined
;;; `skeleton' function call with arguments replaced by binding times
;;; `def-typesig*' is a list of type signatures of defined operations
;;; `def-opsig*' is a list of type signature of primitive operators
;;; `def-memo' is an optional memoization definition
(define (bta-run d* symtab skeleton def-typesig* def-opsig* def-memo)
  (debug-level 1 (display "bta-run") (newline))
  (set! *bta-user-memoization*
	(and def-memo
	     (= 3 (length def-memo))
	     (equal? (caddr def-memo) 'ONLY)))
  (set! *bta-max-bt*
	(apply max (cdr skeleton)))
  (type->btann! the-top-type (make-ann))
  (set! *bta-dynamic-points* (list (type->btann the-top-type)))
  ;;(display (list "*bta-user-memoization*" *bta-user-memoization*))
  (let* ((goal-proc (car skeleton))
	 (bts (cdr skeleton))
	 (d (annDefLookup goal-proc d*))
	 (formals (annDefFetchProcFormals d))
	 (d0 (annMakeDef '$goal formals
			 (annMakeCall goal-proc (map annMakeVar formals))))
	 (d* (cons d0 d*))
	 (do-type-inference (full-collect-d* symtab d*))
	 (proc-node (full-ecr (annDefFetchProcBTVar d0)))
	 (proc-type (node-fetch-type proc-node))
	 (proc-type-tcargs (type-fetch-args proc-type))
	 (proc-type-args
	  (let loop ((node (car proc-type-tcargs)))
	    (let ((type (node-fetch-type (full-ecr node))))
	      (let ((args (type-fetch-args type))
		    (ctor (type-fetch-ctor type)))
		(if (equal? ctor ctor-product)
		    (cons (node-fetch-type (full-ecr (car args)))
			  (loop (cadr args)))
		    '())))))
	 (proc-type-result (full-ecr (cadr proc-type-tcargs)))
	 (do-type-inference (full-make-base proc-type-result))
	 (construct-bt-constraints (wft-d* d*))
	 (bt-ann* (map (lambda (bt type)
			 (cons bt (type-fetch-btann type)))
		       bts proc-type-args))
	 (bt-ann* (bt-ann-sort bt-ann*)))
    (btc-propagate *bta-max-bt* (type-fetch-btann
				 (node-fetch-type proc-type-result)))
    (for-each (lambda (ann)
		(btc-propagate *bta-max-bt* ann)) *bta-dynamic-points*)
    (for-each (lambda (bt-ann)
		(btc-propagate (car bt-ann) (cdr bt-ann)))
	      bt-ann*)
;;;    (for-each (bta-typesig d* symtab) def-typesig*)
;;;    (pp (bta-display-d d*))
    (bta-solve-d* d*)
    d*))

(define bt-ann-sort
  (lambda (bt-ann*)
    (generic-sort (lambda (bt-ann1 bt-ann2)
		    (>= (car bt-ann1) (car bt-ann2))) bt-ann*)))

;;; step 1
;;; type inference
;;; types:
;;; T ::= \bot | \top | \chi (T, ..., T)
;;; \chi \in TypeConstructors
;;; { basic(0), ->(2), *(2) } \subseteq TypeConstructors

;;; representation of nodes in the contraint graph
;;; a graph node contains two fields
;;; father - #f if node is a root otherwise a pointer to another node
;;; info - #f or pointer to info node
;;; 
;;; an info node contains the following fields
;;; id - unique number
;;; weight - # elements in the equivalence class
;;; leq - #f or left side of a \preceq constraint

;;; representation:
(define-record node (father info))
(define-record info (id)
  (weight 1)
  (type (make-type ctor-bot '()))
  (dlist '())
  (fvs '()))
(define-record type (ctor args)
  (btann (make-ann))
  (stann (make-ann))
  (memo  (make-ann)))
(define-record ann ()
  (bt 0)
  (dlist '())
  (visited #f))

;;; interface:
(define node-id 0)
(define new-node
  (lambda ()
    (set! node-id (+ 1 node-id))
    (make-node #f (make-info node-id))))

(define node-fetch-father node->father)
(define node-fetch-info   node->info)
(define node-set-father!  node->father!)
(define node-set-info!    node->info!)
(define info-fetch-id     info->id)
(define info-fetch-weight info->weight)
(define info-fetch-type   info->type)
(define info-fetch-dlist  info->dlist)
(define info-fetch-fvs    info->fvs)
(define info-set-weight!  info->weight!)
(define info-set-type!    info->type!)
(define info-set-dlist!   info->dlist!)
(define info-set-fvs!     info->fvs!)
;;;
;;; represent \bot by ***bot***, \top by type constructor ***top***
(define new-type make-type)
(define type-fetch-ctor   type->ctor)
(define type-fetch-args   type->args)
(define type-fetch-btann  type->btann)
(define type-fetch-stann  type->stann)

(define ctor-bot '***bot***)
(define ctor-top '***top***)
(define ctor-basic '***basic***)
(define ctor-function '->)
(define ctor-product '*)
(define ctor-uninteresting (list ctor-top ctor-bot ctor-basic))

(define type-bottom?
  (lambda (type)
    (equal? (type-fetch-ctor type) ctor-bot)))

(define type-function?
  (lambda (type)
    (equal? (type-fetch-ctor type) ctor-function)))

(define node-fetch-type
  (lambda (node)
    (info-fetch-type (node-fetch-info node))))

(define info-dynamic?
  (lambda (info)
    (let ((type (info-fetch-type info)))
      (and type (eq? type the-top-type)))))

;;; find representative of equivalence class (ecr)
(define (full-ecr node)
  (let loop ((node node) (ancestors '()))
    (let ((father (node-fetch-father node)))
      (if father
	  (loop father (cons node ancestors))
	  (begin
	    (for-each (lambda (ancestor)
			(node-set-father! ancestor node))
		      ancestors)
	    node)))))

;;; enforce a basic value
(define (full-make-base node)
  (full-add-leq node ctor-basic '()))

(define the-top-type
  (let ((type (make-type ctor-top '())))
    (ann->visited! (type-fetch-btann type) -1)
    type))

;;; dynamize a node
(define (full-dynamize node)
  (full-dynamize-ecr (full-ecr node)))
(define (full-dynamize-ecr ecr)
  (full-dynamize-info (node-fetch-info ecr)))
(define (full-dynamize-info info)
  (if (not (info-dynamic? info))
      (begin
	;; (display "dynamize") (newline)
	(let ((dlist (info-fetch-dlist info))
	      (type (info-fetch-type info)))
	  (info-set-type! info the-top-type)
	  (for-each full-dynamize dlist)
	  (for-each full-dynamize (type-fetch-args type))))))

;;; add free variables
(define (full-add-fvs node fvs)
  (let ((info (node-fetch-info (full-ecr node))))
    (info-set-fvs! info (append fvs (info-fetch-fvs info)))))

;;; add a type constraint
(define (full-add-leq node ctor args)
  (full-add-leq-ecr (full-ecr node) ctor args))
(define (full-add-leq-ecr ecr ctor args)
  (let* ((info (node-fetch-info ecr))
	 (old-type (info-fetch-type info)))
    (if (type-bottom? old-type)
	(info-set-type! info (new-type ctor args))
	(cond
	 ((eq? the-top-type old-type)
	  (for-each full-dynamize args))
	 ((equal? ctor (type-fetch-ctor old-type))
	  (for-each full-equate args (type-fetch-args old-type)))
	 (else
	  (full-dynamize-info info)
	  (for-each full-dynamize args))))))

;;; add constraint < arg-nodes -> res-node > \preceq node
(define (full-add-function node arg-nodes res-node)
  (let ((tv-arglist (new-node)))
    (full-add-leq node ctor-function (list tv-arglist res-node))
    (let loop ((tv-arglist tv-arglist)
	       (arg-nodes arg-nodes))
      (if (null? arg-nodes)
	  #f
	  (let ((tv-rest (new-node))
		(arg-node (car arg-nodes)))
	    (full-add-leq tv-arglist ctor-product (list arg-node tv-rest))
	    (loop tv-rest (cdr arg-nodes)))))))

;;; add constraint < arg-nodes . final-arg-node -> res-node > \preceq node
(define (full-add-vfunction node arg-nodes final-arg-node res-node)
  (let ((tv-arglist (new-node)))
    (full-add-leq node ctor-function (list tv-arglist res-node))
    (let loop ((tv-arglist tv-arglist)
	       (arg-nodes arg-nodes))
      (if (null? arg-nodes)
	  (full-equate tv-arglist final-arg-node)
	  (let ((tv-rest (new-node))
		(arg-node (car arg-nodes)))
	    (full-add-leq tv-arglist ctor-product (list arg-node tv-rest))
	    (loop tv-rest (cdr arg-nodes)))))))

;;; install a dependency node1 |> node2
(define (full-make-depend node1 node2)
  (let* ((ecr1 (full-ecr node1))
	 (ecr2 (full-ecr node2))
	 (info1 (node-fetch-info ecr1))
	 (info2 (node-fetch-info ecr2)))
    (if (info-dynamic? info1)
	(full-dynamize-info info2)
	(info-set-dlist! info1 (cons ecr2 (info-fetch-dlist info1))))))

(define (full-make-depend-ecr ecr nodes)
  (info-set-dlist! (node-fetch-info ecr) nodes))
(define (full-add-depend-ecr ecr node)
  (let ((info (node-fetch-info ecr)))
    (info-set-dlist! info (cons node (info-fetch-dlist info)))))

;;; equate two nodes --- the union part
(define (full-equate node1 node2)
  (let ((ecr1 (full-ecr node1))
	(ecr2 (full-ecr node2)))
    (if (eq? ecr1 ecr2)
	'nothing-to-do
	(let ((info1 (node-fetch-info ecr1))
	      (info2 (node-fetch-info ecr2)))
	  (if (info-dynamic? info1)
	      (if (info-dynamic? info2)
		  'nothing-to-do
		  (full-dynamize-info info2))
	      (if (info-dynamic? info2)
		  (full-dynamize-info info1)
		  (let* ((weight1 (info-fetch-weight info1))
			 (weight2 (info-fetch-weight info2))
			 (xc (< weight1 weight2))
			 (father (if xc ecr2 ecr1))
			 (son (if xc ecr1 ecr2))
			 (info (if xc info2 info1))
			 (old-info (if xc info1 info2)))
		    (node-set-father! son father)
		    (node-set-info! son #f)
		    (info-set-weight! info (+ weight1 weight2))
		    (info-set-dlist! info (append (info-fetch-dlist info1)
						  (info-fetch-dlist
						   info2)))
		    (info-set-fvs! info (append (info-fetch-fvs info1)
						(info-fetch-fvs info2)))
		    (let* ((old-type (info-fetch-type old-info)))
		      (if (not (type-bottom? old-type))
			  (full-add-leq-ecr father
					    (type-fetch-ctor old-type)
					    (type-fetch-args old-type)))))))))))

;;; generate type variables and constraints
(define full-collect-lift-list)
(define (full-collect-d* symtab d*)
  (let ((symtab 
	 (append
	  (map (lambda (d)
		 (annDefSetProcBTVar! d (new-node))
		 (cons (annDefFetchProcName d) (annDefFetchProcBTVar d)))
	       d*)
	  symtab)))
    (set! full-collect-lift-list '())
    (map (lambda (d) (full-collect-d symtab d)) d*)
    (full-collect-process-lifts)
    ;; (display "lift-list processed") (newline)
    (set! full-collect-lift-list '())))

(define (full-collect-process-lifts)
  (let loop ((the-lifts full-collect-lift-list)
	     (cur-lifts full-collect-lift-list)
	     (remaining-lifts '()))
    (if (null? cur-lifts)
	(begin
	  ;; (display "done?") (newline)
	(if (and-map2 eq? the-lifts (reverse remaining-lifts))
	    'DONE
	    (loop remaining-lifts remaining-lifts '())))
	(let* ((lift-pair (car cur-lifts))
	       (node1 (full-ecr (car lift-pair)))
	       (node2 (full-ecr (cdr lift-pair)))
	       (ctor1 (type-fetch-ctor (node-fetch-type node1)))
	       (ctor2 (type-fetch-ctor (node-fetch-type node2))))
	  ;; (display (list "lift" ctor1 ctor2))
	  (if (and (member ctor1 ctor-uninteresting)
		   (member ctor2 ctor-uninteresting))
	      (begin
		;; (display " no") (newline)
		(loop the-lifts (cdr cur-lifts) (cons lift-pair
						      remaining-lifts)))
	      (begin
		(full-equate node1 node2)
		;; (display (list "yes" (type-fetch-ctor (node-fetch-type (full-ecr node1)))))
		;; (newline)
		(loop the-lifts (cdr cur-lifts) remaining-lifts)))))))

(define (full-collect-d symtab d)
  (let ((formals (annDefFetchProcFormals d)))
    (if formals
	(let* ((argtvs (map (lambda (v) (cons v (new-node))) formals))
	       (bodytv (full-collect (annDefFetchProcBody d)
				     (append argtvs symtab)))
	       (proctv (annDefFetchProcBTVar d)))
	  (full-add-function proctv (map cdr argtvs) bodytv))
	(let ((bodytv (full-collect (annDefFetchProcBody d) symtab))
	      (proctv (annDefFetchProcBTVar d)))
	  (full-equate bodytv proctv)))))

;;; full-collect returns the type variable of e
(define (full-collect e symtab)
  (let loop ((e e))
    ;; (display "full-collect ") (display e) (newline)
    (let ((phi (new-node)))
      (annExprSetType! e phi)
      (cond
       ((annIsVar? e)
	(full-equate phi (cdr (assoc (annFetchVar e) symtab))))
       ((annIsConst? e)
	(full-make-base phi))
       ((annIsCond? e)
	(let ((phi-1 (loop (annFetchCondTest e)))
	      (phi-2 (loop (annFetchCondThen e)))
	      (phi-3 (loop (annFetchCondElse e))))
	  (full-equate phi-2 phi)
	  (full-equate phi-3 phi)))
       ((annIsOp? e)
	(let* ((phi* (map loop (annFetchOpArgs e)))
	       (property (annFetchOpProperty e))
	       (type (annFetchOpType e)))
	  (if (procedure? property)
	      (property phi phi*)
	      ;; !!! implement type
	      ;; standard operator
	      (begin
		;(full-make-base phi)
		(for-each (lambda (phi-i)
			    ;(full-make-base phi-i)
			    (full-make-depend phi-i phi))
			  phi*)
		(if (eq? INTERNAL-IDENTITY (annFetchOpName e))
		    (set! full-collect-lift-list
			  (cons (cons phi (car phi*))
				full-collect-lift-list)))))))
       ((annIsCall? e)
	(let ((phi* (map loop (annFetchCallArgs e))))
	  (full-add-function (cdr (assoc (annFetchCallName e) symtab))
			     phi* phi)))
       ((annIsLet? e)
	(let* ((phi-1 (loop (annFetchLetHeader e)))
	       (phi-2 (full-collect (annFetchLetBody e)
				    (cons (cons (annFetchLetVar e) phi-1)
					  symtab))))
	  (full-equate phi phi-2)))
       ((annIsVLambda? e)
	(let* ((fixed-formals (annFetchVLambdaFixedVars e))
	       (var-formal (annFetchVLambdaVar e))
	       (vars (cons var-formal fixed-formals))
	       (pvs (map (lambda (v) (new-node)) vars))
	       ;(fvs (map (lambda (v) (cdr (assoc (annFetchVar v) symtab))) (annFreeVars e)))
	       (phi-0 (full-collect (annFetchVLambdaBody e)
				    (append (map cons vars pvs) symtab))))
	  (annSetVLambdaBTVars! e pvs)
	  (full-add-vfunction phi (cdr pvs) (car pvs) phi-0)))
       ((annIsLambda? e)
	(let* ((vars (annFetchLambdaVars e))
	       (pvs (map (lambda (v) (new-node)) vars))
	       (fvs (annFreeVars e))
	       (phi-0 (full-collect (annFetchLambdaBody e)
				    (append (map cons vars pvs) symtab))))
	  (annSetLambdaBTVars! e pvs)
	  (full-add-function phi pvs phi-0)
	  (full-add-fvs phi fvs)))
       ((annIsApp? e)
	(let* ((phi-0 (loop (annFetchAppRator e)))
	       (phi* (map loop (annFetchAppRands e))))
	  (full-add-function phi-0 phi* phi)))
       ((annIsCtor? e)
	(let* ((phi* (map loop (annFetchCtorArgs e)))
	       (ctor (annFetchCtorName e))
	       (ctorDesc (annFetchCtorDesc e))
	       (nr-pre (desc-np ctorDesc))
	       (nr-arg (desc-nc ctorDesc))
	       (nr-post (- (desc-nt ctorDesc) (+ nr-pre nr-arg)))
	       (pp (append (nlist nr-pre new-node)
			   phi*
			   (nlist nr-post new-node))))
	  (full-add-leq phi (desc-type ctorDesc) pp)))
       ((annIsSel? e)
	(let* ((phi-0 (loop (annFetchSelArg e)))
	       (comp (annFetchSelComp e))
	       (desc (annFetchSelDesc e))
	       (nr-pre (desc-np desc))
	       (nr-arg (desc-nc desc))
	       (nr-post (- (desc-nt desc) (+ nr-pre nr-arg))))
	  (full-add-leq phi-0 (desc-type desc)
			(append (nlist nr-pre new-node)
				(nlist (- comp 1) new-node)
				(list phi)
				(nlist (- nr-arg comp) new-node)
				(nlist nr-post new-node)))))
       ((annIsTest? e)
	(let* ((phi-0 (loop (annFetchTestArg e)))
	       (desc (annFetchTestDesc e)))
	  (full-add-leq phi-0 (desc-type desc) (nlist (desc-nt desc) new-node))
	  (full-make-base phi)))
       ((annIsEval? e)
	(let ((phi-0 (loop (annFetchEvalBody e))))
	  (full-make-base phi-0)))
       (else
	(error "full-collect: unrecognized syntax")))
      phi)))

;;; step 2
;;; well-formedness constraints

(define wft-visited 0)
(define (wft-t type)
  (let ((targs (type-fetch-args  type))
	(btann (type-fetch-btann type))
	(stann (type-fetch-stann type)))
    (if (eq? type the-top-type)
	'nothing-to-do
	(if (ann->visited btann)
	    'nothing-to-do		;break recursion for recursive types
	    (begin
	      (set! wft-visited (+ 1 wft-visited))
	      (ann->visited! btann wft-visited)
	      (ann->visited! stann (+ 1000000 wft-visited))
	      (let loop ((args targs)
			 (dlist (cons stann (ann->dlist btann)))) ;beta <= sigma
		(if (null? args)
		    (begin
		      (ann->dlist! btann dlist)
		      (for-each (lambda (node) (wft-t
						(node-fetch-type
						 (full-ecr node)))) targs))
		    (let ((arg (node-fetch-type (full-ecr (car args)))))
		      (let ((arg-stann (type-fetch-stann arg)))
			(ann->dlist!
			 arg-stann
			 (cons stann (ann->dlist arg-stann)))) ; sigma_i <= sigma
		      (loop (cdr args) (cons (type-fetch-btann arg) dlist))))))))))
					;beta <= beta_i

(define (wft-e e)
  (let loop ((e e))
    ;; (display "wft") (display e) (newline)
    (let* ((ecr (full-ecr (annExprFetchType e)))
	   (type (info-fetch-type (node-fetch-info ecr))))
      (annExprSetType! e ecr)
      (wft-t type)
      (cond
       ((annIsVar? e)
	;; this is really unfortunate:
	;; how do we lift a variable????
	;; >>> we apply an invisible primitive to each variable!!!
	)
       ((annIsCond? e)
	(let ((test-exp (annFetchCondTest e)))
	  (loop test-exp)
	  (loop (annFetchCondThen e))
	  (loop (annFetchCondElse e))
	  (if (not *bta-user-memoization*)
	      (let ((btann (type-fetch-btann type))
		    (test-type (info-fetch-type
				(node-fetch-info
				 (annExprFetchType test-exp)))))
		    (if (eq? test-type the-top-type)
			(bta-note-dynamic! btann)
			(let ((test-btann (type-fetch-btann test-type))
			      (test-stann (type-fetch-stann test-type)))
    			  (ann->dlist!
			   test-stann
			   (cons test-btann (ann->dlist test-stann))) ; sigma_1 <= beta_1
			  (ann->dlist!
			   test-btann
			   (cons btann (ann->dlist test-btann))))))))) ; beta_1 <= beta
       ((annIsOp? e)
	(let ((op-args (annFetchOpArgs e))
	      (op-property (annFetchOpProperty e))
	      (op-name (annFetchOpName e)))
	  (for-each loop op-args)
	  (let ((btann (type-fetch-btann type))
		(stann (type-fetch-stann type)))
	    (if (eq? op-name INTERNAL-IDENTITY)
		(let* ((arg-type  (info-fetch-type
				   (node-fetch-info
				    (annExprFetchType (car op-args)))))
		       (arg-btann (type-fetch-btann arg-type))
		       (arg-stann (type-fetch-stann arg-type)))
		  (ann->dlist! arg-btann (cons btann (ann->dlist
						      arg-btann))) 
		  (ann->dlist! arg-stann (cons stann (ann->dlist
						      arg-stann))))
		(begin
		  (if (and op-property (equal? op-property 'dynamic))
		      (bta-note-dynamic! btann))
		  (ann->dlist! stann (cons btann (ann->dlist stann)))
		  ; sigma <= beta 
		  (for-each
		   (lambda (arg)
		     (let ((arg-type (info-fetch-type
				      (node-fetch-info
				       (annExprFetchType arg)))))
		       (let ((arg-btann (type-fetch-btann arg-type))
			     (arg-stann (type-fetch-stann arg-type)))
			 (ann->dlist!
			  arg-stann
			  (cons arg-btann (ann->dlist arg-stann))) ; sigma_i <= beta_i
			 (ann->dlist!
			  arg-btann
			  (cons btann (ann->dlist arg-btann))))))	; beta_i <= beta
		   op-args))))))
       ((annIsCall? e)
	(for-each loop (annFetchCallArgs e)))
       ((annIsLet? e)
	(loop (annFetchLetHeader e))
	(loop (annFetchLetBody e)))
       ((annIsVLambda? e)
	(loop (annFetchVLambdaBody e)))
       ((annIsLambda? e)
	(loop (annFetchLambdaBody e)))
       ((annIsApp? e)
	(loop (annFetchAppRator e))
	(for-each loop (annFetchAppRands e)))
       ((annIsCtor? e)
	(for-each loop (annFetchCtorArgs e)))
       ((annIsSel? e)
	(loop (annFetchSelArg e)))
       ((annIsTest? e)
	(loop (annFetchTestArg e)))
       ((annIsEval? e)
	(let ((stann (type-fetch-stann type))
	      (btann (type-fetch-btann type)))
	  (ann->dlist! stann (cons btann (ann->dlist stann)))) ; sigma <= beta
	(loop (annFetchEvalBody e)))
       (else
	'nothing-to-do)))))

(define (wft-d* d*)
  (set! wft-visited 0)
  (for-each
   (lambda (d)
     (wft-t (node-fetch-type (full-ecr (annDefFetchProcBTVar d))))
     (wft-e (annDefFetchProcBody d)))
   d*))

;;; error
(define btc-propagate
  (lambda (bt ann)
    (debug-level 3 (display "btc-propagate ") (display bt) (display " ("))
    (let loop ((ann ann))
      (debug-level 3 (display (ann->visited ann)) (display " "))
      (if (< (ann->bt ann) bt)
	  (let ((dlist (ann->dlist ann)))
	    (debug-level 3 (display (map ann->visited dlist)) (display " "))
	    (ann->bt! ann bt)
	    (if dlist
		(for-each loop dlist)))))
    (debug-level 3 (display ")") (newline))))

;;; step 3
;;; bta-solve-d* evaluates the normalized constraint set and inserts
;;; memoization points unless manually overridden.
(define (bta-solve-d* d*)
  (debug-level 1 (display "bta-solve") (newline))
  (debug-level 2 (display-bts-d* d*) (newline))
  (for-each (lambda (d) (bta-solve (annDefFetchProcBody d))) d*)
  (debug-level 1 (display "bta-solve done") (newline))) 

;;; bta-solve returns #t for serious expressions
;;; only dynamic ifs and lambdas which guard serious expressions are
;;; considered for memoization points
;;; we rely of wft-e to have stored the ecrs everywhere
;;; sets the level of every expression to the binding time of its result
;;; include memo analysis
(define (bta-solve e)
  (let* ((type (info-fetch-type
	       (node-fetch-info
		(annExprFetchType e))))
	 (btann (type-fetch-btann type))
	 (bt  (ann->bt btann)))
    (annExprSetLevel! e bt)
    (cond
     ((annIsVar? e)
      #f)
     ((annIsConst? e)
      (if (> 0 bt)
	  (annIntroduceLift e 0 bt))
      #f)
     ((annIsCond? e)
      (let* ((e-test (annFetchCondTest e))
	     (r-test (bta-solve e-test))
	     (r-then (bta-solve (annFetchCondThen e)))
	     (r-else (bta-solve (annFetchCondElse e))))
	(if (or r-test r-then r-else)
	    (begin
	      (if (not *bta-user-memoization*)
		  (let ((fvs (annFreeVars e))
			(level (annExprFetchLevel e-test)))
		    (annIntroduceMemo e bt level fvs)
		    (map (bta-memo-var level) fvs)))
	      #t)
	    #f)))
     ((annIsOp? e)
      (let ((args (map bta-solve (annFetchOpArgs e)))
	    (property (annFetchOpProperty e)))
	;; (newline) (display (list "op" (annFetchOpName e)))
	(map (introduce-lift-if-needed bt) (annFetchOpArgs e))
	(if property
	    (cond
	     ((equal? property 'MEMO)
	      (let* ((args (annFetchOpArgs e))
		     (lv (annFetchConst (car args)))
		     (body (cadr args)))
		(annIntroduceMemo1 e bt lv (annFreeVars body) body)
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
	     (dyn-lambda (< 0 bt)))
	(annSetLambdaBTVars! e (map (lambda (node)
				      (type-fetch-btann (node-fetch-type (full-ecr node))))
				    (annFetchLambdaBTVars e)))
	(if (and dyn-lambda body (not *bta-user-memoization*))
	    (annIntroduceMemo e bt bt (annFreeVars e))))
      #f)
     ((annIsVLambda? e)
      (let* ((body (bta-solve (annFetchVLambdaBody e)))
	     (dyn-lambda (< 0 bt)))
	(annSetVLambdaBTVars! e (map (lambda (node)
				       (type-fetch-btann (node-fetch-type (full-ecr node))))
				    (annFetchVLambdaBTVars e)))
	(if (and dyn-lambda body (not *bta-user-memoization*))
	    (annIntroduceMemo e bt bt (annFreeVars e))))
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
      (bta-solve (annFetchEvalBody e))))))

(define introduce-lift-if-needed
  (lambda (bt)
    (lambda (e)
      (let ((lv (annExprFetchLevel e)))
	;; (display (list lv "->" bt))
	(if (= bt lv)
	    'nothing-to-do
	    (annIntroduceLift e lv (- bt lv)))))))

(define bta-memo-var
  (lambda (level)
    (letrec
	((bta-memo-var
	  (lambda (var)
	    (bta-memo-node (annExprFetchType var))))
	 (bta-memo-node
	  (lambda (node)
	    (let* ((info (node-fetch-info (full-ecr node)))
		   (type (info-fetch-type info)))
	      (let* ((memo (type->memo type))
		     (memo-time (ann->bt memo)))
		(if (<= level memo-time)
		    'nothing-to-do
		    (begin
		      (ann->bt! memo level)
		      (if (type-function? type)
			  (map bta-memo-var (info-fetch-fvs info))
			  (map bta-memo-node (type-fetch-args type))))))))))
      bta-memo-var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; for debugging
;;;

(define (display-bts-e e)
  (let loop ((e e))
    ;; (display "wft") (display e) (newline)
    (let* ((ecr (full-ecr (annExprFetchType e)))
	   (type (info-fetch-type (node-fetch-info ecr))))
      (cons (display-bts-t ecr)
      (cond
       ((annIsVar? e)
	(annFetchVar e))
       ((annIsConst? e)
	`(',(annFetchConst e)))
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
       ((annIsVLambda? e)
	`(LAMBDA (,@(annFetchVLambdaFixedVars e)
		  . ,(annFetchVLambdaVar e))
	   ,(loop (annFetchVLambdaBody e))))
       ((annIsLambda? e)
	`(LAMBDA ,(annFetchLambdaVars e)
	   ,(loop (annFetchLambdaBody e))))
       ((annIsApp? e)
	`(,(loop (annFetchAppRator e))
	  ,@(map loop (annFetchAppRands e))))
       ((annIsCtor? e)
	`(,(annFetchCtorName e)
	  ,@(map loop (annFetchCtorArgs e))))
       ((annIsSel? e)
	`(,(annFetchSelName e)
	  ,(loop (annFetchSelArg e))))
       ((annIsTest? e)
	`(,(annFetchTestName e)
	  ,(loop (annFetchTestArg e))))
       ((annIsEval? e)
	`(EVAL ,(loop (annFetchEvalBody e))))
       (else
	'unknown-expression))))))

(define (display-bts-d* d*)
  (for-each
   (lambda (d)
     (let ((name (annDefFetchProcName d))
	   (formals (annDefFetchProcFormals d))
	   (body (annDefFetchProcBody d)))
     (display `(define (,name ,@formals : ,(display-bts-t
					    (annDefFetchProcBTVar d)))
		 ,(display-bts-e body)))
     (newline)))
   d*))

(define (display-bts-t node)
  (let loop ((node node) (seenb4 '()))
    (let ((type (node-fetch-type (full-ecr node))))
      (let* ((args (type-fetch-args type))
	     (ctor (type-fetch-ctor type))
	     (btann (type-fetch-btann type))
	     (dlist (ann->dlist btann))
	     (seen (cons node seenb4)))
	(if (memq node seenb4)
	    `(*** ,ctor ,(ann->visited btann))
	    `(,ctor ,(ann->visited btann)
		    ,(ann->bt btann)
		    ,(map ann->visited dlist)
		    ,@(map loop args (map (lambda (foo) seen) args))))))))
