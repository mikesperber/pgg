;;; binding-time analysis based on annotated type systems


;;; step 0
;;; the driver for the bta

(define *bta-max-bt* #f)

;;; debugging and trace aids
;;; (define *bta-display-level* 1)
(define-syntax bta-debug-level
  (syntax-rules ()
    ((_ level arg ...)
     (if (>= *bta-display-level* level)
	 (begin arg ...)))))

(define *bta-bt-points* '())
(define (bta-note-dynamic! ann)
  (set! *bta-bt-points* (cons (cons *bta-max-bt* ann) *bta-bt-points*)))
(define (bta-note-level! lv ann)
  (set! *bta-bt-points* (cons (cons lv ann) *bta-bt-points*)))

;;; binding-time analysis
;;; `d*' list of function definitions
;;; `symtab' is an initial symbol table where only constructors,
;;; selectors, and constructor tests are defined
;;; `skeleton' function call with arguments replaced by binding times
;;; `def-type*' is a list of type declarations
(define (bta-run d* symtab skeleton def-type*)
  (bta-debug-level 1 (display "bta-run") (newline))
  (let ((goal-proc (car skeleton))
	(bts (cdr skeleton)))
    (set! *bta-max-bt*
	  (apply max (cons 1 bts)))
    (type->btann! the-top-type (make-ann))
    (set! *bta-bt-points* '())
    (bta-note-dynamic! (type->btann the-top-type))
    (let* ((d (annDefLookup goal-proc d*))
	   (formals (or (annDefFetchProcFormals d)
			(let ((body (annDefFetchProcBody d)))
			  (cond
			   ((annIsLambda? body)
			    (annFetchLambdaVars body))
			   (else
			    (error "specified goal is not a procedure"))))))
	   (d0 (annMakeDef '$goal formals
			   (bta-insert-def-mutable
			    d* (bta-insert-def-data
				def-type* (ann-maybe-coerce (annMakeCall goal-proc (map annMakeVar formals)))))))
	   (mutable-defs (collect-mutable-defs d*))
	   (d* (cons d0 d*))
	   (do-type-inference (full-collect-d* d*))
	   (proc-node (full-ecr (annDefFetchProcBTVar d0)))
	   (proc-type (node-fetch-type proc-node))
	   (proc-type-tcargs (type-fetch-args proc-type))
	   (proc-type-args
	    (let loop ((node (car proc-type-tcargs)))
	      (let ((type (node-fetch-type node)))
		(let ((args (type-fetch-args type))
		      (ctor (type-fetch-ctor type)))
		  (if (equal? ctor ctor-product)
		      (cons (node-fetch-type (car args))
			    (loop (cadr args)))
		      '())))))
	   (proc-type-result (full-ecr (cadr proc-type-tcargs)))
	   (dynamize-result (bta-note-dynamic!
			     (type-fetch-btann (node-fetch-type proc-type-result))))
	   ;; (do-type-inference (full-make-base proc-type-result))
	   (do-effect-analysis
	    (if *scheme->abssyn-static-references*
		(effect-analysis d* (+ *scheme->abssyn-label-counter* 1)))) 
	   (construct-bt-constraints (wft-d* d*))
					;	 (SHOW-BT_CONSTRAINTS
					;	  (with-output-to-file "/tmp/display-bt-constraints.scm"
					;	    (lambda ()
					;	      (p (display-bts-d* d*)))))
	   (bt-ann* (bt-ann-sort
		     (append (map (lambda (bt type)
				    (cons bt (type-fetch-btann type)))
				  bts proc-type-args)
			     *bta-bt-points*))))
      (btc-propagate *bta-max-bt* (type-fetch-btann
				   (node-fetch-type proc-type-result)))
      (for-each (lambda (bt-ann)
		  (btc-propagate (car bt-ann) (cdr bt-ann)))
		bt-ann*)
;;;    (for-each (bta-typesig d* symtab) def-typesig*)
;;;    (p (display-bts-d* d*))
      (bta-solve-d* d*)
      (append mutable-defs d*))))

(define bt-ann-sort
  (lambda (bt-ann*)
    (generic-sort (lambda (bt-ann1 bt-ann2)
		    (>= (car bt-ann1) (car bt-ann2))) bt-ann*)))

(define (bta-insert-def-data def-datatype* body)
  (let ((make-op (annMakeOp1 #f
			     wft-define-data-property
			     #f
			     (parse-type '(all t t)))))
  (let loop ((defs def-datatype*))
    (if (null? defs)
	body
	(let ((def (car defs)))
	  (if (eq? (car def) 'define-data)
	      (annMakeLet (gensym 'begin)
			  (make-op '_DEFINE_DATA
				   (list (annMakeConst (cdr def))))
			  (loop (cdr defs)))
	      (loop (cdr defs))))))))

(define (bta-insert-def-mutable d* body)
  (let ((make-op (annMakeOp1 #f
			     wft-define-mutable-property
			     #f
			     (parse-type '(all t t)))))
  (let loop ((d* d*))
    (if (null? d*)
	body
	(let ((def (car d*))
	      (d* (cdr d*)))
	  (if (annIsDefMutable? def)
	      (annMakeLet (gensym 'begin)
			  (make-op '_DEFINE
				   (list (annMakeVar (annDefFetchProcName def))
					 (ann-maybe-coerce (annDefFetchProcBody def))))
			  (loop d*))
	      (loop d*)))))))

(define (collect-mutable-defs d*)
  (let loop ((d* d*) (defs '()))
    (if (null? d*)
	defs
	(let ((def (car d*))
	      (d* (cdr d*)))
	  (if (annIsDefMutable? def)
	      (let ((name (annDefFetchProcName def)))
		(loop d*
		      (cons
		       (annMakeDef name
				   #f
				   (annMakeConst name))
		       defs)))
	      (loop d* defs))))))

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
  (memo  (make-ann))
  (effect #f))				;only for function types
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
(define type-fetch-effect type->effect)

(define (ann+>dlist! x ann)
  (ann->dlist! ann (cons x (ann->dlist ann))))

(define ctor-bot '***bot***)
(define ctor-top '***top***)
(define ctor-basic '***basic***)
(define ctor-eval '***eval-dont-lift-me***)
(define ctor-function '->)
(define ctor-product '*)
(define ctor-reference 'ref)
(define ctor-uninteresting (list ctor-bot ctor-basic))

(define type-bottom?
  (lambda (type)
    (equal? (type-fetch-ctor type) ctor-bot)))

(define type-function?
  (lambda (type)
    (equal? (type-fetch-ctor type) ctor-function)))

(define node-fetch-type
  (lambda (node)
    (info-fetch-type (node-fetch-info (full-ecr node)))))

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

(define (full-make-top node)
  (full-add-leq node ctor-top '()))

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
	 (old-type (info-fetch-type info))
	 (old-ctor (type-fetch-ctor old-type)))
    (if (or (type-bottom? old-type)
	    (eq? ctor-eval old-ctor))
	(info-set-type! info (new-type ctor args))
	(cond
	 ((eq? the-top-type old-type)
	  (for-each full-dynamize args))
	 ((eq? ctor old-ctor)
	  (for-each full-equate args (type-fetch-args old-type)))
	 ((eq? ctor ctor-eval)
	  'ready)
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
(define full-collect-lift-list #f)
(define (full-collect-d* d*)
  (let ((symtab 
	 (extend-env*
	  (map annDefFetchProcName d*)
	  (map (lambda (d)
		 (let ((node (new-node)))
		   (annDefSetProcBTVar! d node)
		   node))
	       d*)
	  the-empty-env)))
    (set! full-collect-lift-list '())
    (for-each (lambda (d) (full-collect-d symtab d)) d*)
    (full-collect-process-lifts)
    ;; (display "lift-list processed") (newline)
    (set! full-collect-lift-list '())))

(define (full-collect-process-lifts)
  ;; (display-line full-collect-process-lifts)
  (let ((changes #f))
    (let loop ((cur-lifts full-collect-lift-list)
	       (remaining-lifts '()))
      (if (null? cur-lifts)
	  (begin
	    ;;(display-line "done?")
	    (if (not changes)
		'DONE
		(begin
		  (set! changes #f)
		  (loop remaining-lifts '()))))
	  (let* ((lift-pair (car cur-lifts))
		 (node1 (full-ecr (car lift-pair)))
		 (node2 (full-ecr (cdr lift-pair)))
		 (ctor1 (type-fetch-ctor (node-fetch-type node1)))
		 (ctor2 (type-fetch-ctor (node-fetch-type node2))))
	    ;;(display (list "lift" ctor2 (info-fetch-id (node-fetch-info node2)) "to" ctor1 (info-fetch-id (node-fetch-info node1)) "?"))
	    (if (eq? ctor2 ctor-bot)
		(begin
		  ;;(display-line " no")
		  (loop (cdr cur-lifts) (cons lift-pair remaining-lifts)))
		(begin
		  (set! changes #t)
		  (if (eq? ctor2 ctor-basic)
		      (full-make-base node1)
		      (full-equate node1 node2))
		  ;;(display-line (list "yes" (type-fetch-ctor (node-fetch-type (full-ecr node1)))))
		  (loop (cdr cur-lifts) remaining-lifts))))))))

(define (full-collect-d symtab d)
  (let ((formals (annDefFetchProcFormals d)))
;;    (display (list 'full-collect-d (annDefFetchProcName d)
;;		   (annDefFetchProcFormals d))) (newline)
    (if formals
	(let* ((arg-nodes (map (lambda (v) (new-node)) formals))
	       (bodytv (full-collect (annDefFetchProcBody d)
				     (extend-env* formals arg-nodes symtab)))
	       (proctv (annDefFetchProcBTVar d)))
	  (full-add-function proctv arg-nodes bodytv))
	(let ((bodytv (full-collect (annDefFetchProcBody d) symtab))
	      (proctv (annDefFetchProcBTVar d)))
	  (full-equate bodytv proctv)
	  (if (annIsDefMutable? d)
	      (full-make-top proctv))))))

;;; full-collect returns the type variable of e
;;; this is ___ONLY___ concerned with type inference
(define (full-collect e symtab)
  (let loop ((e e))
    ;;(display (list "full-collect" (vector-ref e 0))) (newline)
    (let ((phi (new-node)))
      (annExprSetType! e phi)
      (cond
       ((annIsVar? e)
	(call-with-current-continuation
	 (lambda (k)
	   (full-equate phi (apply-env symtab (annFetchVar e)
				       (lambda ()
					 (bta-debug-level 1 (display-line "unknown var: " (annFetchVar e) " considered TOP"))
					 (annSetVarGlobal! e #t)
					 (full-make-top phi)
					 (k 'ignored)))))))
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
	       (type (annFetchOpType e)))
	  ;; !!! implement type
	  ;(full-make-base phi)
	  ;(for-each (lambda (phi-i) (full-make-base phi-i)) phi*)
	  (if (eq? INTERNAL-IDENTITY (annFetchOpName e))
	      (bta-internal-identity-property phi phi*))))
       ((annIsCall? e)
	(let ((phi* (map loop (annFetchCallArgs e))))
	  (full-add-function
	   (apply-env symtab (annFetchCallName e) (lambda () (error "unknown proc")))
			     phi* phi)))
       ((annIsLet? e)
	(let* ((phi-1 (loop (annFetchLetHeader e)))
	       (phi-2 (full-collect (annFetchLetBody e)
				    (extend-env (annFetchLetVar e) phi-1 symtab))))
	  (full-equate phi phi-2)))
       ((annIsVLambda? e)
	(let* ((fixed-formals (annFetchVLambdaFixedVars e))
	       (var-formal (annFetchVLambdaVar e))
	       (vars (cons var-formal fixed-formals))
	       (pvs (map (lambda (v) (new-node)) vars))
	       ;(fvs (map (lambda (v) (cdr (assoc (annFetchVar v) symtab))) (annFreeVars e)))
	       (phi-0 (full-collect (annFetchVLambdaBody e)
				    (extend-env* vars pvs symtab))))
	  (annSetVLambdaBTVars! e pvs)
	  (full-add-vfunction phi (cdr pvs) (car pvs) phi-0)))
       ((annIsLambda? e)
	(let* ((vars (annFetchLambdaVars e))
	       (pvs (map (lambda (v) (new-node)) vars))
	       (fvs (annFreeVars e))
	       (phi-0 (full-collect (annFetchLambdaBody e)
				    (extend-env* vars pvs symtab))))
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
	  (full-make-base phi-0)
	  (full-add-leq phi ctor-eval '())))
       ((annIsRef? e)
	(let ((phi-0 (loop (annFetchRefArg e))))
	  (full-add-leq phi ctor-reference (list phi-0))))
       ((annIsDeref? e)
	(let ((phi-0 (loop (annFetchDerefArg e))))
	  (full-add-leq phi-0 ctor-reference (list phi))))
       ((annIsAssign? e)
	(let ((phi-0 (loop (annFetchAssignRef e)))
	      (phi-1 (loop (annFetchAssignArg e))))
	  (full-add-leq phi-0 ctor-reference (list phi-1))
	  (full-make-base phi)))
       ((annIsCellEq? e)
	(let* ((phi* (map loop (annFetchCellEqArgs e)))
	       (phi-1 (car phi*))
	       (phi-2 (cadr phi*)))
	  (full-add-leq phi-1 ctor-reference (list (new-node)))
	  (full-equate phi-1 phi-2)))
       (else
	(error "full-collect: unrecognized syntax")))
      phi)))

;;; property functions for built-in operators
(define bta-internal-identity-property
  (lambda (phi phi*)
    (full-make-depend (car phi*) phi)
    (set! full-collect-lift-list
	  (cons (cons phi (car phi*))
		full-collect-lift-list))))

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
		      (for-each (lambda (node) (wft-t (node-fetch-type node)))
				targs)
		      (let ((effect (type-fetch-effect type)))
			(if effect
			    (begin
			      (let ((ctor (type-fetch-ctor type)))
				(cond
				 ((eq? ctor ctor-function)
				  ;; (display (list "effect on function" (display-bts-eff effect))) (newline)
				  (effect-for-each
				   (lambda (lab)
				     (let* ((reftype
					     (effect-label->type lab))
					    (rbtann (type-fetch-btann
						     reftype)))
				       (ann+>dlist! rbtann btann)))
				   effect))
				 ((eq? ctor ctor-top)
				  ;; (display (list "effect on top" (display-bts-eff effect))) (newline)
				  (effect-for-each
				   (lambda (lab)
				     (let* ((reftype
					     (effect-label->type lab))
					    (rbtann (type-fetch-btann
						     reftype)))
				       (bta-note-dynamic! rbtann)))
				   effect))
				 ((eq? ctor ctor-reference)
				  'nothing-to-do)
				 (else
				  (error "effect on " ctor))))))))
		    (let ((arg (node-fetch-type (car args))))
		      (let ((arg-stann (type-fetch-stann arg)))
			(ann+>dlist! stann arg-stann)) ; sigma_i <= sigma
		      (loop (cdr args) (cons (type-fetch-btann arg) dlist))))))))))
					;beta <= beta_i

(define (wft-e e auto-memo)
  (let loop ((e e))
    ;; (display "wft") (display e) (newline)
    (let* ((ecr (full-ecr (annExprFetchType e)))
	   (type (node-fetch-type ecr)))
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
	  (let* ((btann (type-fetch-btann type))
		 (test-type (node-fetch-type (annExprFetchType test-exp)))
		 (test-btann (type-fetch-btann test-type))
		 (test-stann (type-fetch-stann test-type)))
	    (if auto-memo
		(ann+>dlist! btann test-btann)) ; beta => beta_1
	    (ann+>dlist! test-btann test-stann)))) ; sigma_1 <= beta_1
       ((annIsOp? e)
	(let ((op-arg-types (map loop (annFetchOpArgs e)))
	      (op-property (annFetchOpProperty e))
	      (op-name (annFetchOpName e)))
	  (cond
	   ((procedure? op-property)
	    (op-property type op-arg-types))
	   ((eq? op-name INTERNAL-IDENTITY)
	    (let* ((arg-type (car op-arg-types))
		   (arg-btann (type-fetch-btann arg-type))
		   (arg-stann (type-fetch-stann arg-type)))
	      (ann+>dlist! (type-fetch-btann type) arg-btann)
	      (ann+>dlist! (type-fetch-stann type) arg-stann)))
	   (else
	    (wft-depend-property type op-arg-types)))))
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
	(let* ((btann (type-fetch-btann type))
	       (arg (annFetchTestArg e))
	       (arg-type (node-fetch-type (annExprFetchType arg)))
	       (arg-btann (type-fetch-btann arg-type)))
	  (loop arg)
	  (ann+>dlist! btann arg-btann))) ; beta_arg <= beta
       ((annIsEval? e)
	(let ((stann (type-fetch-stann type))
	      (btann (type-fetch-btann type)))
	  (ann+>dlist! btann stann)) ; sigma <= beta
	(loop (annFetchEvalBody e)))
       ((annIsRef? e)
	(loop (annFetchRefArg e)))
       ((annIsDeref? e)
	(loop (annFetchDerefArg e)))
       ((annIsAssign? e)
	(let* ((btann (type-fetch-btann type))
	       (ref (annFetchAssignRef e)))
	  (loop ref)
	  (loop (annFetchAssignArg e))
	  (let* ((ref-type (node-fetch-type (annExprFetchType ref)))
		 (ref-btann (type-fetch-btann ref-type)))
	    (ann+>dlist! btann ref-btann)))) ;beta_ref <= beta
       ((annIsCellEq? e)
	(for-each loop (annFetchCellEqArgs e)))
       (else
	'nothing-to-do))
      type)))

(define (wft-d* d*)
  (set! wft-visited 0)
  (for-each
   (lambda (d)
     (wft-t (node-fetch-type (annDefFetchProcBTVar d)))
     (wft-e (annDefFetchProcBody d) (annDefFetchProcAutoMemo d)))
   d*))

;;; wft property functions for built-in operators
;;; each function accepts the result type and the list of argument types
(define wft-depend-property
  (lambda (type type*)
    (let ((btann (type-fetch-btann type))
	  (stann (type-fetch-stann type)))
      (ann+>dlist! btann stann)		; sigma <= beta 
      (for-each
       (lambda (arg-type)
	 (let ((arg-btann (type-fetch-btann arg-type))
	       (arg-stann (type-fetch-stann arg-type)))
	   (ann+>dlist! arg-btann arg-stann) ; sigma_i <= beta_i
	   (ann+>dlist! btann arg-btann)	; beta_i <= beta
	   (ann+>dlist! arg-btann btann)	; beta <= beta_i
	   ))
       type*))))

(define wft-apply-property
  (lambda (type type*)
    (bta-debug-level 3 (display "wft-apply-property") (newline))
    (wft-depend-property type type*)
    (let ((type-fun (car type*))
	  (type-args (cdr type*)))
      (let ((btann (type-fetch-btann type))
	    (fun-btann (type-fetch-btann type-fun)))
	(ann+>dlist! fun-btann btann))))) ; btann <= fun-btann

(define wft-dynamic-property
  (lambda (type type*)
    (bta-debug-level 3 (display "wft-dynamic-property") (newline))
    (for-each (lambda (type)
		(bta-note-dynamic! (type-fetch-btann type)))
	      (cons type type*))))

(define wft-error-property
  (lambda (type type*)
    (bta-debug-level 3 (display "wft-error-property") (newline))
    (bta-note-dynamic! (type-fetch-btann type))))

(define wft-define-data-property
  (lambda (type type*)
    (bta-note-level! (- *bta-max-bt* 1) (type-fetch-btann type))))

(define wft-define-mutable-property
  (lambda (type type*)
    (bta-note-level! (- *bta-max-bt* 1) (type-fetch-btann type))
    (for-each (lambda (arg-type)
		(bta-note-level! *bta-max-bt* (type-fetch-btann arg-type)))
	      type*)))

(define (wft-make-memo-property level)
  (lambda (type type*)
    (for-each (lambda (type)
		(if (<= level *bta-max-bt*)
		    (bta-note-level!
		     level
		     (type-fetch-btann type))))
	      (cons type type*))))

(define wft-property-table
  `((apply   . ,wft-apply-property)
    (dynamic . ,wft-dynamic-property)
    (error   . ,wft-dynamic-property)
    (opaque  . ,wft-dynamic-property)))

;;; propagate binding-time constraints
(define btc-propagate
  (lambda (bt ann)
    (bta-debug-level 3 (display "btc-propagate ") (display bt) (display " ("))
    (let loop ((ann ann))
      (bta-debug-level 3 (display (ann->visited ann)) (display " "))
      (if (< (ann->bt ann) bt)
	  (let ((dlist (ann->dlist ann)))
	    (bta-debug-level 3 (display (map ann->visited dlist)) (display " "))
	    (ann->bt! ann bt)
	    (if dlist
		(for-each loop dlist)))))
    (bta-debug-level 3 (display ")") (newline))))

;;; step 3
;;; bta-solve-d* evaluates the normalized constraint set and inserts
;;; memoization points unless manually overridden.
(define (bta-solve-d* d*)
  (bta-debug-level 1 (display "bta-solve") (newline))
  (bta-debug-level 2 (with-output-to-file "/tmp/bta-pre-solve.scm"
		   (lambda () (display-bts-d* d*) (newline))))
  (for-each bta-solve-d d*)
  (bta-debug-level 1 (display "bta-solve done") (newline))) 

(define (bta-solve-d d)
  (let* ((body (annDefFetchProcBody d))
	 (auto-memo? (annDefFetchProcAutoMemo d)))
    (bta-solve body)
    (bta-introduce-memo body auto-memo?)))

(define (bta-make-memo-postprocessor lv)
  (lambda (e bt)
    (let* ((args (annFetchOpArgs e))
	   (body (car args))
	   (fvs (annFreeVars body)))
      (annIntroduceMemo1 e bt lv fvs body)
      (for-each (bta-memo-var lv) fvs)
      #f)))

;;; bta-solve
;;; we rely of wft-e to have stored the ecrs everywhere
;;; sets the level of every expression to the binding time of its result
(define (bta-solve e)
  (let loop ((e e))
    (let* ((type (info-fetch-type
		  (node-fetch-info
		   (annExprFetchType e))))
	   (btann (type-fetch-btann type))
	   (bt  (ann->bt btann)))
      (annExprSetLevel! e bt)
      (cond
       ((annIsVar? e))
       ((annIsConst? e))
       ((annIsCond? e)
	(loop (annFetchCondTest e))
	(loop (annFetchCondThen e))
	(loop (annFetchCondElse e)))
       ((annIsOp? e)
	(for-each loop (annFetchOpArgs e)))
       ((annIsCall? e)
	(for-each loop (annFetchCallArgs e)))
       ((annIsLet? e)
	(loop (annFetchLetHeader e))
	(loop (annFetchLetBody e)))
       ((annIsLambda? e)
	(loop (annFetchLambdaBody e))
	(annSetLambdaBTVars! e (map (lambda (node)
				      (type-fetch-btann (node-fetch-type node)))
				    (annFetchLambdaBTVars e))))
       ((annIsVLambda? e)
	(loop (annFetchVLambdaBody e))
	(annSetVLambdaBTVars! e (map (lambda (node)
				       (type-fetch-btann (node-fetch-type node)))
				     (annFetchVLambdaBTVars e))))
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
	(loop (annFetchEvalBody e)))
       ((annIsRef? e)
	(loop (annFetchRefArg e)))
       ((annIsDeref? e)
	(loop (annFetchDerefArg e)))
       ((annIsAssign? e)
	(loop (annFetchAssignRef e))
	(loop (annFetchAssignArg e)))
       ((annIsCellEq? e)
	(for-each loop (annFetchCellEqArgs e)))))))

;;; bta-introduce-memo returns #t for serious expressions
;;; only dynamic ifs and lambdas which guard serious expressions are
;;; considered for memoization points
;;; introduces lifts, too
(define (bta-introduce-memo e auto-memo)
  (let new-loop ((e e)
		 (i-must-memo #t))	;you must memoize!
    (let loop ((e e))	
      (let ((bt (annExprFetchLevel e)))
	(cond
	 ((annIsVar? e)
	  #f)
	 ((annIsConst? e)
	  (if (> 0 bt)
	      (annIntroduceLift e 0 bt))
	  #f)
	 ((annIsCond? e)
	  (let* ((e-test (annFetchCondTest e))
		 (level (annExprFetchLevel e-test))
		 (inner-i-must-memo (and i-must-memo (zero? level)))
		 (r-test (new-loop e-test inner-i-must-memo))
		 (r-then (new-loop (annFetchCondThen e) inner-i-must-memo))
		 (r-else (new-loop (annFetchCondElse e) inner-i-must-memo))
		 (need-memo (or r-test r-then r-else)))
	    (cond
	     ((or (zero? level) (not i-must-memo))
	      need-memo)
	     (need-memo
	      (if auto-memo
		  (let ((fvs (annFreeVars e)))
		    (annIntroduceMemo e bt level fvs)
		    (for-each (bta-memo-var level) fvs)))
	      #f)
	     (else
	      #f))))
	 ((annIsOp? e)
	  (let ((args (map loop (annFetchOpArgs e)))
		(postprocess (annFetchOpPostprocessor e)))
	    ;; (newline) (display (list "op" (annFetchOpName e)))
	    (map (introduce-lift-if-needed bt) (annFetchOpArgs e))
	    (if postprocess
		(postprocess e bt)
		(list-or args))))
	 ((annIsCall? e)
	  (for-each loop (annFetchCallArgs e))
	  #t)
	 ((annIsLet? e)
	  (let ((header (loop (annFetchLetHeader e)))
		(body (loop (annFetchLetBody e))))
	    (or header body)))
	 ((annIsLambda? e)
	  (let* ((dyn-lambda (< 0 bt))
		 (body (new-loop (annFetchLambdaBody e) (not dyn-lambda))))
	    (if (and dyn-lambda body auto-memo)
		(let ((fvs  (annFreeVars e)))
		  (annIntroduceMemo e bt bt fvs)
		  (for-each (bta-memo-var bt) fvs))))
	  #f)
	 ((annIsVLambda? e)
	  (let* ((dyn-lambda (< 0 bt))
		 (body (new-loop (annFetchVLambdaBody e) (not dyn-lambda))))
	    (if (and dyn-lambda body auto-memo)
		(let ((fvs  (annFreeVars e)))
		  (annIntroduceMemo e bt bt fvs)
		  (for-each (bta-memo-var bt) fvs))))
	  #f)
	 ((annIsApp? e)
	  (let* ((rator (annFetchAppRator e))
		 (r-rator (loop rator))
		 (r-rands (map loop (annFetchAppRands e)))
		 (level (annExprFetchLevel rator)))
	    (and (zero? level)		;only static apps are dangerous
		 (or r-rator (or-map id r-rands)))))
	 ((annIsCtor? e)
	  (strict-or-map loop (annFetchCtorArgs e)))
	 ((annIsSel? e)
	  (loop (annFetchSelArg e)))
	 ((annIsTest? e)
	  (loop (annFetchTestArg e)))
	 ((annIsEval? e)
	  (loop (annFetchEvalBody e)))
	 ((annIsRef? e)
	  (loop (annFetchRefArg e)))
	 ((annIsDeref? e)
	  (loop (annFetchDerefArg e)))
	 ((annIsAssign? e)
	  (let ((r1 (loop (annFetchAssignRef e)))
		(r2 (loop (annFetchAssignArg e))))
	    (or r1 r2)))
	 ((annIsCellEq? e)
	  (strict-or-map loop (annFetchCellEqArgs e))))))))

(define introduce-lift-if-needed
  (lambda (bt)
    (lambda (e)
      (let ((lv (annExprFetchLevel e)))
	;; (display (list lv "->" bt))
	(if (> bt lv)
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
    (let* ((ecr (full-ecr (annExprFetchType e))))
      (list (display-bts-eff (annExprFetchEffect e))
	    (display-bts-t ecr)
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
       ((annIsRef? e)
	`(MAKE-CELL ,(annFetchRefLabel e) ,(loop (annFetchRefArg e))))
       ((annIsDeref? e)
	`(CELL-REF ,(loop (annFetchDerefArg e))))
       ((annIsAssign? e)
	`(CELL-SET! ,(loop (annFetchAssignRef e))
		    ,(loop (annFetchAssignArg e))))
       ((annIsCellEq? e)
	`(CELL-EQ? ,@(map loop (annFetchCellEqArgs e))))
       (else
	'unknown-expression))))))

(define (display-bts-d* d*)
  (for-each
   (lambda (d)
     (let ((name (annDefFetchProcName d))
	   (formals (or (annDefFetchProcFormals d) '(NO_ARGS)))
	   (body (annDefFetchProcBody d)))
     (display `(define (,name ,@formals : ,(display-bts-t
					    (annDefFetchProcBTVar d)))
		 ,(display-bts-e body)))
     (newline)))
   d*))

(define (display-bts-t node)
  (if #f
      (let ((effect (type-fetch-effect (node-fetch-type node))))
	(display-bts-eff effect))
      (let loop ((node node) (seenb4 '()))
	(let ((type (node-fetch-type node))
	      (info (node-fetch-info (full-ecr node))))
	  (let* ((args (type-fetch-args type))
		 (ctor (type-fetch-ctor type))
		 (effect (type-fetch-effect type))
		 (btann (type-fetch-btann type))
		 (dlist (ann->dlist btann))
		 (seen (cons node seenb4)))
	    (if #t ;;(memq node seenb4)
		`(*** ,ctor ,(info-fetch-id info))
		`(,ctor ,(ann->visited btann)
			,(ann->bt btann)
			,(map ann->visited dlist)
			,(display-bts-eff effect)
			,@(map loop args (map (lambda (foo) seen) args)))))))))

(define (display-bts-eff effect)
  (and effect
       (labset->list (effect->labset effect))))

