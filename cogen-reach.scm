;;; reaching definitions analysis

;;; !!!! does not respect cell-eq? everywhere !!!!!

;;; imports cogen-env cogen-labset

;;; definition of the abstract domain and its operations
;;; assumption: 
;;; reference creations and assignment operations are numbered from 
;;; 1 ... *reach-max-number*

(set-scheme->abssyn-let-insertion! #f)

(define *reach-max-number* '*reach-max-number*-undefined)

(define (set-reach-max-number! n)
  (set! *reach-max-number* (+ n 1)))

(define *base-type-label* 0)

(define-record definite-update
  (lab image))

(define-record indefinite-update
  (labset image))

(define empty-assignment '())
(define (empty-assignment? ass)
  (null? ass))
(define (first-update assignment)
  (car assignment))
(define (rest-assignment assignment)
  (cdr assignment))
(define (make-simple-assignment update)
  (list update))
(define (append-assignment ass1 ass2)
  (append ass1 ass2))
(define (make-assignment update ass)
  (cons update ass))

(define (apply-update assignment upd merge?)
  (if (empty-assignment? assignment)
      (make-simple-assignment upd)
      (if (definite-update? upd)
	  (apply-definite-update assignment
				 (definite-update->lab)
				 (definite-update->image)
				 merge?)
	  (apply-indefinite-update assignment
				   (indefinite-update->labset)
				   (indefinite-update->image)
				   merge?))))

(define (apply-definite-update ass lab image merge?)
  (let loop ((ass ass))
    (if (empty-assignment? ass)
	(make-simple-assignment (make-definite-update lab image))
	(let ((upd (first-update ass)))
	  (if (definite-update? upd)
	      (if (equal? (definite-update->lab upd) lab)
		  (if merge?
		      (make-assignment (make-definite-update
					lab
					(labset-union
					 image
					 (definite-update->image upd)))
				       (loop (rest-assignment ass)))
		      (make-assignment (make-definite-update lab image)
				       (loop (rest-assignment ass))))
		  (make-assignment upd
				   (loop (rest-assignment ass))))
	      (let ((labset (indefinite-update->labset upd))
		    (image2 (indefinite-update->image upd)))
		(if (labset-elem? lab labset)
		    (if merge?
			(make-assignment (make-indefinite-update
					  (labset-singleton lab)
					  (labset-union image image2))
					 (apply-indefinite-update
					  (rest-assignment ass)
					  (labset-remove lab labset)
					  image2
					  merge?))
			(make-assignment (make-definite-update lab image)
					 (apply-indefinite-update
					  (rest-assignment ass)
					  (labset-remove lab labset)
					  image2
					  merge?)))
		    (make-assignment upd
				     (loop (rest-assignment ass))))))))))

(define (apply-indefinite-update ass labset1 image merge?)
  (if (labset-empty? labset1)
      ass
      (let loop ((ass ass))
	(if (empty-assignment? ass)
	    (make-simple-assignment (make-indefinite-update labset1 image))
	    (let ((upd (first-update ass)))
	      (if (definite-update? upd)
		  (let ((lab (definite-update->lab upd)))
		    (if (labset-elem? lab labset1)
			(make-assignment (make-indefinite-update
					  (labset-singleton lab)
					  (labset-union image
							(definite-update->image upd)))
					 (apply-indefinite-update
					  (rest-assignment ass)
					  (labset-remove lab labset1)
					  image
					  merge?))
			(make-assignment upd
					 (loop (rest-assignment ass)))))
		  ;; indefinite-update
		  (let* ((labset2 (indefinite-update->labset upd))
			 (labset0 (labset-intersection labset1 labset2)))
		    (if (labset-empty? labset0)
			(make-assignment upd
					 (loop (rest-assignment ass)))
			(let ((image2 (indefinite-update->image upd)))
			  (make-assignment
			   (make-indefinite-update
			    labset0
			    (labset-union image image2))
			   (apply-indefinite-update
			    (apply-indefinite-update
			     (rest-assignment ass)
			     (labset-subtract labset2 labset0)
			     image2
			     merge?)
			    (labset-subtract labset1 labset0)
			    image
			    merge?)))))))))))

(define (merge-assignments ass1 ass2)
  (if (empty-assignment? ass1)
      ass2
      (merge-assignments (rest-assignment ass1)
			 (apply-update ass2
				       (first-update ass1)
				       #t))))

(define (assignment-equal? ass1 ass2)
  (let ((empty1? (empty-assignment? ass1))
	(empty2? (empty-assignment? ass2)))
    (or (and empty1? empty2?)
	(and (not empty1?) (not empty2?)
	     (let* ((upd1 (first-update ass1))
		    (rest-ass2 (remove-update upd1 ass2)))
	       (and rest-ass2
		    (assignment-equal? (rest-assignment ass1)
				       rest-ass2)))))))

(define (remove-update upd ass)
  (if (definite-update? upd)
      (remove-definite-update (definite-update->lab upd)
			      (definite-update->image upd)
			      ass)
      (remove-indefinite-update (indefinite-update->labset upd)
				(indefinite-update->image upd)
				ass)))

(define (remove-definite-update lab image ass)
  (let loop ((ass ass) (result empty-assignment))
    (if (empty-assignment? ass)
	#f
	(let ((upd2 (first-update ass)))
	  (if (and (definite-update? upd)
		   (equal? lab (definite-update->lab upd)))
	      (if (labset-equal? image (definite-update->image upd))
		  (append-assignment (rest-assignment ass) result)
		  #f)
	      (loop (rest-assignment ass)
		    (make-assignment upd result)))))))

(define (remove-indefinite-update labset image ass)
  (let loop ((ass ass) (result empty-assignment))
    (if (empty-assignment? ass)
	#f
	(let ((upd (first-update ass)))
	  (if (and (indefinite-update? upd)
		   (labset-equal? labset (indefinite-update->labset upd)))
	      (if (labset-equal? image (indefinite-update->image upd))
		  (append-assignment (rest-assignment ass) result)
		  #f)
	      (loop (rest-assignment ass)
		    (make-assignment upd result)))))))

(define (coerce-indefinite-assignment ass)
  (let loop ((ass ass) (result empty-assignment))
    (if (empty-assignment? ass)
	result
	(let ((upd (first-update ass)))
	  (if (definite-update? upd)
	      (loop (rest-assignment ass)
		    (make-assignment
		     (make-indefinite-update (labset-singleton
					      (definite-update->lab upd))
					     (definite-update->image upd))
		     result))
	      (loop (rest-assignment ass)
		    (make-assignment upd result)))))))

(define (filter-assignment ass refset)
  (let ((refset (list->labset refset)))
    (let loop ((ass ass) (result empty-assignment))
      (if (empty-assignment? ass)
	  result
	  (let ((upd (first-update ass))
		(rest (rest-assignment ass)))
	    (if (definite-update? upd)
		(let ((lab (definite-update->lab upd)))
		  (if (labset-elem? lab refset)
		      (loop rest result)
		      (loop rest (make-assignment upd result))))
		(let* ((labset (indefinite-update->labset upd))
		       (labset (labset-subtract labset refset)))
		  (if (labset-empty? labset)
		      (loop rest result)
		      (loop rest (make-assignment
				  (make-indefinite-update
				   labset (indefinite-update->image upd))
				  result))))))))))

(define (list->labset xs)
  (let loop ((xs xs) (result empty-labset))
    (if (null? xs)
	result
	(loop (cdr xs) (labset-add (car xs) result)))))

(define (type->labset type)
  (let* ((vec (ps-node->vector type))
	 (refset (let loop ((i 0) (result empty-labset))
		   (if (>= i *reach-max-number*)
		       result
		       (if (true-entry? (vector-ref vec i))
			   (loop (+ i 1) (labset-add i result))
			   (loop (+ i 1) result))))))
    refset))
;;; restrict the assignment "ass" to those mentioned in "type"
(define (project-assignment ass type)
  (let* ((refset (type->labset type)))
    (let loop ((ass ass) (result empty-assignment))
      (if (empty-assignment? ass)
	  result
	  (let ((upd (first-update ass))
		(rest (rest-assignment ass)))
	    (if (definite-update? upd)
		(let ((lab (definite-update->lab upd)))
		  (if (labset-elem? lab refset)
		      (loop rest (make-assignment upd result))
		      (loop rest result)))
		(let* ((labset (indefinite-update->labset upd))
		       (labset (labset-intersection labset refset)))
		  (if (labset-empty? labset)
		      (loop rest result)
		      (loop rest (make-assignment
				  (make-indefinite-update
				   labset (indefinite-update->image upd))
				  result))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; solver algorithm of Palsberg and Schwartzbach

(define-record ps-node
  (neighbors vector))

(define (ps-node-add-neighbor node new)
  (let ((neighbors (ps-node->neighbors node)))
    (if (member new neighbors)
	#f
	(begin (ps-node->neighbors! node (cons new neighbors))
	       #t))))

(define (new-ps-node)
  (make-ps-node '() (make-vector *reach-max-number* '())))
;;; in the vector, a list represents "false" and #f represents "true"!
(define true-entry #f)
(define (true-entry? x) (not x))
(define (false-entry? x) x)
;;; the lists contain pairs of graph nodes

(define (ps-print-node ps-node)
  (let ((vec (ps-node->vector ps-node)))
    (let loop ((i 0))
      (if (< i *reach-max-number*)
	  (let ((vec_i (vector-ref vec i)))
	    (if (true-entry? vec_i)
		(cons i (loop (+ i 1)))
		(loop (+ i 1))))
	  '()))))

(define (ps-insert-one-label label node) ; label in node
  (let* ((vec (ps-node->vector node))
	 (vec_label (vector-ref vec label)))
    (if (true-entry? vec_label)
	'nothing-to-do
	(begin
	  (vector-set! vec label true-entry)
	  (for-each (lambda (v-w)
		      (ps-add-constraint (car v-w) (cdr v-w)))
		    vec_label)
	  (for-each (lambda (node)
		      (ps-insert-one-label label node))
		    (ps-node->neighbors node)))))) 

(define (ps-add-constraint v w)		;v <= w
  (if (ps-node-add-neighbor v w)
      (ps-insert-vector (ps-node->vector v) w)))

(define (ps-insert-vector vec node)
  (let loop ((i 0))
    (if (< i *reach-max-number*)
	(let ((vec_i (vector-ref vec i)))
	  (if (true-entry? vec_i)
	      (ps-insert-one-label i node))
	  (loop (+ i 1))))))

(define (ps-add-conditional-constraint label node v w)
  ;; label \in node => v \subseteq w
  (let* ((vec (ps-node->vector node))
	 (vec_label (vector-ref vec label)))
    (if (true-entry? vec_label)
	(ps-add-constraint v w)
	(vector-set! vec label (cons (cons v w) vec_label)))))

;;; need an access vector that maps labels back to expressions
;;; it contains:
(define-record access-lambda
  (vars body))
(define-record access-ctor
  (ctor args))
(define-record access-ref
  (arg))
(define-record access-assign
  (ref arg))
;;; 1. construct the access vector & alist for procs
;;; 2. allocate the type variables
(define ps-access-vector 'ps-access-vector-not-initialized)
(define ps-access-procs 'ps-access-procs-not-initialized)
(define ps-lambda-labels 'ps-lambda-labels-not-initialized)
(define ps-ctor-labels 'ps-ctor-labels-not-initialized)
(define ps-ref-labels 'ps-ref-labels-not-initialized)
(define (ps-prepare-graph-d* d*)
  (set! ps-access-vector (make-vector *reach-max-number* #f))
  (set! ps-lambda-labels '())
  (set! ps-ctor-labels '())
  (set! ps-ref-labels '())
  (set! ps-access-procs (map ps-prepare-graph-d d*)))

(define (ps-prepare-graph-d d)
  (let* ((formals (annDefFetchProcFormals d))
	 (formal-nodes (map (lambda (var) (new-ps-node)) formals))
	 (body (annDefFetchProcBody d)))
    (ps-prepare-graph-e body (extend-env* formals
					  formal-nodes
					  the-empty-env))
    (let ((entry (make-access-lambda formal-nodes (annExprFetchType body))))
      (annDefSetProcBTVar! d entry)
      (cons (annDefFetchProcName d) entry))))

(define (ps-prepare-graph-e e symtab)
  (let loop ((e e))
    (if (annIsVar? e)
	(annExprSetType! e (apply-env symtab (annFetchVar e)
				      (lambda ()
					(error "undeclared var"
					       (annFetchVar e)))))  
	(begin
	  (annExprSetType! e (new-ps-node))
	  (cond
	   ((annIsConst? e)
	    'nothing-to-do)
	   ((annIsCond? e)
	    (loop (annFetchCondTest e))
	    (loop (annFetchCondThen e))
	    (loop (annFetchCondElse e)))
	   ((annIsOp? e)
	    (for-each loop (annFetchOpArgs e)))
	   ((annIsCall? e)
	    (for-each loop (annFetchCallArgs e)))
	   ((annIsLet? e)
	    (let ((header (annFetchLetHeader e)))
	      (loop header)
	      (let ((var-node (annExprFetchType header)))
		(ps-prepare-graph-e (annFetchLetBody e)
				    (extend-env (annFetchLetVar e)
						var-node
						symtab)))))
	   ((annIsBegin? e)
	    (loop (annFetchBeginHeader e))
	    (loop (annFetchBeginBody e)))
	   ;;(annIsVLambda? e)
	   ((annIsLambda? e)
	    (let* ((vars (annFetchLambdaVars e))
		   (var-nodes (map (lambda (var) (new-ps-node)) vars))
		   (body (annFetchLambdaBody e))
		   (label (annFetchLambdaLabel e)))
	      (ps-prepare-graph-e body (extend-env* vars var-nodes symtab))
	      (vector-set! ps-access-vector
			   label
			   (make-access-lambda var-nodes
					       (annExprFetchType
						body)))
	      (set! ps-lambda-labels (cons label ps-lambda-labels))))
	   ((annIsApp? e)
	    (loop (annFetchAppRator e))
	    (for-each loop (annFetchAppRands e)))
	   ((annIsCtor? e)
	    (let ((args (annFetchCtorArgs e))
		  (label (annFetchCtorLabel e)))
	      (for-each loop args)
	      (vector-set! ps-access-vector
			   label
			   (make-access-ctor (annFetchCtorName e)
					     (map annExprFetchType args)))
	      (set! ps-ctor-labels (cons label ps-ctor-labels))))
	   ((annIsSel? e)
	    (loop (annFetchSelArg e)))
	   ((annIsTest? e)
	    (loop (annFetchTestArg e)))
	   ((annIsRef? e)
	    (let ((arg (annFetchRefArg e))
		  (label (annFetchRefLabel e)))
	      (loop arg)
	      (vector-set! ps-access-vector
			   label
			   (make-access-ref (annExprFetchType arg)))
	      (set! ps-ref-labels (cons label ps-ref-labels))))
	   ((annIsDeref? e)
	    (loop (annFetchDerefArg e)))
	   ((annIsAssign? e)
	    (let ((ref (annFetchAssignRef e))
		  (arg (annFetchAssignArg e)))
	      (loop ref)
	      (loop arg)
	      (vector-set! ps-access-vector
			   (annFetchAssignLabel e)
			   (make-access-assign ref arg))))
	   ((annIsCellEq? e)
	    (for-each loop (annFetchCellEqArgs e)))
	   ((annIsEval? e)
	    (loop (annFetchEvalBody e))))))))

;;; generate PS constraints:

(define (ps-generate-constraints-d* d*)
  (for-each ps-generate-constraints-d d*))

(define (ps-generate-constraints-d d)
  (ps-generate-constraints-e (annDefFetchProcBody d)))
;;; returns the type (variable) of the expression
(define (ps-generate-constraints-e e)
  (let loop ((e e))
    (let ((type (annExprFetchType e)))
      (cond
       ((annIsVar? e)
	'nothing-to-do)
       ((annIsConst? e)
	(ps-insert-one-label *base-type-label* type))
       ((annIsCond? e)
	(let ((test-type (loop (annFetchCondTest e)))
	      (then-type (loop (annFetchCondThen e)))
	      (else-type (loop (annFetchCondElse e))))
	  (ps-add-constraint then-type type)
	  (ps-add-constraint else-type type)))
       ((annIsOp? e)
	(for-each loop (annFetchOpArgs e))
	(ps-insert-one-label *base-type-label* type)) ;???
       ((annIsCall? e)
	(let* ((arg-types (map loop (annFetchCallArgs e)))
	       (name (annFetchCallName e))
	       (name-access (annDefLookup name ps-access-procs)))
	  (for-each ps-add-constraint
		    arg-types (access-lambda->vars name-access))
	  (ps-add-constraint (access-lambda->body name-access) type)))
       ((annIsLet? e)
	(loop (annFetchLetHeader e))
	(ps-add-constraint (loop (annFetchLetBody e)) type))
       ((annIsBegin? e)
	(loop (annFetchBeginHeader e))
	(ps-add-constraint (loop (annFetchBeginBody e)) type))
       ;;(annIsVLambda? e)
       ((annIsLambda? e)
	(loop (annFetchLambdaBody e))
	(ps-insert-one-label (annFetchLambdaLabel e) type))
       ((annIsApp? e)
	(let ((rator-type (loop (annFetchAppRator e)))
	      (rand-types (map loop (annFetchAppRands e))))
	  (for-each (lambda (label)
		      (let* ((access (vector-ref ps-access-vector label))
			     (vars (access-lambda->vars access))
			     (body (access-lambda->body access)))
			(for-each (lambda (actual formal)
				    (ps-add-conditional-constraint
				     label rator-type actual formal))
				  rand-types vars)
			(ps-add-conditional-constraint
			 label rator-type body type)))
		    ps-lambda-labels)))
       ((annIsCtor? e)
	(loop (annFetchCtorArgs e))
	(ps-insert-one-label (annFetchCtorLabel e) type))
       ((annIsSel? e)
	(let* ((arg-type (loop (annFetchSelArg e)))
	       (desc (annFetchSelDesc e))
	       (comp (annFetchSelComp e))
	       (ctor-name (desc-ctor desc)))
	  (for-each (lambda (label)
		      (let* ((access (vector-ref ps-access-vector label))
			     (access-ctor (access-ctor->ctor access)))
			(if (eq? ctor-name access-ctor)
			    (let ((ctor-arg (list-ref (access-ctor->args access) comp)))
			      (ps-add-conditional-constraint
			       label arg-type ctor-arg type)))))
		    ps-ctor-labels)))
       ((annIsTest? e)
	(loop (annFetchTestArg e))
	(ps-insert-one-label *base-type-label* type))
       ((annIsRef? e)
	(loop (annFetchRefArg e))
	(ps-insert-one-label (annFetchRefLabel e) type))
       ((annIsDeref? e)
	(let ((arg-type (loop (annFetchDerefArg e))))
	  (for-each (lambda (label)
		      (let* ((access (vector-ref ps-access-vector label))
			     (ref-arg (access-ref->arg access)))
			(ps-add-conditional-constraint
			 label arg-type ref-arg type)))
		    ps-ref-labels)))
       ((annIsAssign? e)
	(let ((ref-type (loop (annFetchAssignRef e)))
	      (arg-type (loop (annFetchAssignArg e))))
	  (for-each (lambda (label)
		      (let* ((access (vector-ref ps-access-vector label))
			     (ref-arg (access-ref->arg access)))
			(ps-add-conditional-constraint
			 label ref-type arg-type ref-arg)))
		    ps-ref-labels)
	  (ps-insert-one-label *base-type-label* type)))
       ((annIsEval? e)
	(loop (annFetchEvalBody e))))
      type)))

;;;
(define (access->refset acc)
  (let ((vars (access-lambda->vars acc))
	(body (access-lambda->body acc)))
    (let loop ((tvars (cons body vars)) (seen '()) (result '()))
      (if (null? tvars)
	  result
	  (let ((tvar (car tvars))
		(tvars (cdr tvars)))
	    (if (memq tvar seen)
		(loop tvars seen result)
		(let ((vec (ps-node->vector tvar)))
		  (let recur ((i 0) (vars '()))
		    (if (< i *reach-max-number*)
			(if (true-entry? (vector-ref vec i))
			    (let ((entry (vector-ref ps-access-vector i)))
			      (cond
			       ((access-lambda? entry)
				(recur (+ i 1)
				       (cons (access-lambda->body entry)
					     (append
					      (access-lambda->vars entry)
					      vars))))
			       ((access-ctor? entry)
				(recur (+ i 1)
				       (append (access-ctor->args entry)
					       vars)))
			       ((access-ref? entry)
				(set! result (cons i result))
				(recur (+ i 1)
				       (cons (access-ref->arg entry)
					     vars)))
			       (else
				(recur (+ i 1) vars))))
			    (recur (+ i 1) vars))
			(loop (append vars tvars) (cons tvar seen) result))))))))))

;;; refined effect analysis

(define *effect-vector* 'undefined-effect-vector)

(define *rea-changes* #f)

(define (rea-fix d*)
  (rea-prepare-d* d*)
  (let loop ()
    (set! *rea-changes* #f)
    (rea-do-d* d*)
    (if *rea-changes*
	(loop))))

(define (rea-prepare-d* d*)
  (set! *effect-vector* (make-vector *reach-max-number* empty-assignment))
  (for-each rea-prepare-d d*))

(define (rea-prepare-d d)
  (annDefSetProcEVar! d empty-assignment))

(define (rea-do-d* d*)
  (for-each (lambda (d) (rea-do-d d d*)) d*))

(define (rea-do-d d d*)
  (let* ((ass-out (rea-do-e (annDefFetchProcBody d) d*))
	 (ass-out (coerce-indefinite-assignment ass-out))
	 (type (annDefFetchProcBTVar d))
	 (refset (access->refset type))
	 (ass-out (filter-assignment ass-out refset)))
    (if (assignment-equal? ass-out (annDefFetchProcEVar d))
	'nothing-to-do
	(begin
	  (annDefSetProcEVar! d ass-out)
	  (set! *rea-changes* #t)))))
;;; transforms initial assignment to final assignment
(define (rea-do-e e d*)
  (let loop ((e e) (ass-in empty-assignment))
    (display (list "rea-do-e" ass-in)) (newline)
    ;; (annExprSetEffect! e (make-rea empty-assignment empty-assignment))
    (cond
     ((annIsVar? e)
      ass-in)
     ((annIsConst? e)
      ass-in)
     ((annIsCond? e)
      (let* ((test-out (loop (annFetchCondTest e) ass-in))
	     (then-out (loop (annFetchCondThen e) test-out))
	     (else-out (loop (annFetchCondElse e) test-out)))
	(merge-assignments then-out else-out)))
     ((annIsOp? e)
      (thread-map loop ass-in (annFetchOpArgs e))) ;assumes pure ops
     ((annIsCall? e)
      (let ((call-in (thread-map loop ass-in (annFetchCallArgs e)))
	    (def (annDefLookup (annFetchCallName e) d*)))
	(merge-assignments call-in (annDefFetchProcEVar def))))
     ((annIsLet? e)
      (loop (annFetchLetBody e) (loop (annFetchLetHeader e) ass-in)))
     ((annIsBegin? e)
      (loop (annFetchBeginBody e) (loop (annFetchBeginHeader e) ass-in)))
     ;;(annIsVLambda? e)
     ((annIsLambda? e)
      (let ((ass-latent (loop (annFetchLambdaBody e) empty-assignment))
	    (label (annFetchLambdaLabel e)))
	;; remove local latent updates!
	(if (assignment-equal? ass-latent
			       (vector-ref *effect-vector* label))
	    'nothing-to-do
	    (begin
	      (vector-set! *effect-vector* label ass-latent)
	      (set! *rea-changes* #t))))
      ass-in)
     ((annIsApp? e)
      (let* ((rator-out (loop (annFetchAppRator e) ass-in))
	     (rands-out (thread-map loop rator-out (annFetchAppRands e)))
	     (ass-out #f))
	(for-each (lambda (label)
		    (if (true-entry? (vector-ref
				      (ps-node->vector
				       (annExprFetchType e))
				      label))
			(let ((ass-latent (vector-ref *effect-vector* label)))
			  (if ass-out
			      (set! ass-out (merge-assignments ass-out
							       ass-latent))
			      (set! ass-out ass-latent)))))
		  ps-lambda-labels)
	(or ass-out empty-assignment)))
     ((annIsCtor? e)
      (thread-map loop ass-in (annFetchCtorArgs e)))
     ((annIsSel? e)
      (loop (annFetchSelArg e) ass-in))
     ((annIsTest? e)
      (loop (annFetchTestArg e) ass-in))
     ((annIsRef? e)
      (let ((ass-before (loop (annFetchRefArg e) ass-in))
	    (label (annFetchRefLabel e)))
	(apply-definite-update ass-in
			       label
			       (labset-singleton label)
			       #f)))
     ((annIsDeref? e)
      (let* ((arg (annFetchDerefArg e))
	     (ass-out (loop arg ass-in))
	     (type (annExprFetchType arg))
	     (proj-ass (project-assignment ass-out type)))
	(annExprSetEffect! e proj-ass)
	;; only store the assignment of the dereferenced reference
	ass-out))
     ((annIsAssign? e)
      (let* ((ref (annFetchAssignRef e))
	     (ref-out (loop ref ass-in))
	     (arg-out (loop (annFetchAssignArg e) ref-out))
	     (label (annFetchAssignLabel e))
	     (type (annExprFetchType ref)))
	(apply-indefinite-update arg-out
				 (type->labset (annExprFetchType ref))
				 (labset-singleton label)
				 #t)))
     ((annIsEval? e)
      (loop (annFetchEvalBody e) ass-in)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (find-ecr v groups)
  (let loop ((v v) (parents '()))
    (let ((next_v (vector-ref groups v)))
      (if next_v
	  (loop next_v (cons v parents))
	  (let loop ((parents parents))	;path compression
	    (if (null? parents)
		v
		(begin
		  (vector-set! groups (car parents) v)
		  (loop (cdr parents)))))))))
(define (union v1 v2 groups)
  (let ((ecr1 (find-ecr v1 groups))
	(ecr2 (find-ecr v2 groups)))
    (if (not (eq? ecr1 ecr2))
	(vector-set! groups ecr2 ecr1))
    ecr1))
(define (union-ps-node node groups)
  (let ((vec (ps-node->vector node)))
    (let loop ((i 0) (v #f))
      (if (>= i *reach-max-number*)
	  v
	  (loop (+ i 1)
		(if (true-entry? (vector-ref vec i))
		    (if v
			(union v i groups)
			(find-ecr i groups))
		     v))))))
(define (union-assignment ass groups)
  (let ((v #f))
    (let loop ((ass ass))
      (if (empty-assignment? ass)
	  v
	  (let* ((upd (first-update ass))
		 (image (if (definite-update? upd)
			    (definite-update->image upd)
			    (indefinite-update->image upd)))
		 (rest (rest-assignment ass)))
	    (labset-for-each (lambda (i)
			       (if v
				   (set! v (union v i groups))
				   (set! v (find-ecr i groups))))
			     image)
	    (loop rest))))))
(define (expand-group e groups)
  (if (not e)
      e
      (let ((ecr (find-ecr e groups)))
	(let loop ((i 0) (result '()))
	  (if (>= i *reach-max-number*)
	      (reverse result)
	      (if (eq? ecr (find-ecr i groups))
		  (loop (+ i 1) (cons i result))
		  (loop (+ i 1) result)))))))

(define *reference-groups* 'undefined-reference-groups)
(define *definition-groups* 'undefined-definition-groups)
(define *reference-group-span* 'undefined-reference-group-span)
(define *definition-numbering* 'undefined-definition-numbering)
(define (group-d* d*)
  (set! *reference-groups* (make-vector *reach-max-number* #f))
  (set! *definition-groups* (make-vector *reach-max-number* #f))
  (for-each group-d d*)
  (set! *reference-group-span* (make-vector *reach-max-number* 0))
  (set! *definition-numbering* (make-vector *reach-max-number* 0))
  (let loop ((i 0))
    (if (< i *reach-max-number*)
	(if (vector-ref *definition-groups* i)
	    (loop (+ i 1))
	    ;; now we found an ecr
	    (let* ((ecr (find-ecr i *reference-groups*))
		   (nr (+ 1 (vector-ref *reference-group-span* ecr))))
	      (vector-set! *reference-group-span* ecr nr)
	      (vector-set! *definition-numbering* i nr)
	      (loop (+ i 1)))))))

(define (group-d d)
  (group-e (annDefFetchProcBody d)))

(define (group-e e)
  (let loop ((e e))
    (cond
     ((annIsVar? e)
      'nothing-to-do)
     ((annIsConst? e)
      'nothing-to-do)
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
     ((annIsBegin? e)
      (loop (annFetchBeginHeader e))
      (loop (annFetchBeginBody e)))
     ;;(annIsVLambda? e)
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
     ((annIsRef? e)
      (loop (annFetchRefArg e)))
     ((annIsDeref? e)
      (let* ((ref (annFetchDerefArg e))
	     (type (annExprFetchType ref))
	     (ass (annExprFetchEffect e)))
	(loop ref)
	(annExprSetType! ref (union-ps-node type *reference-groups*))
	(annExprSetEffect! e (union-assignment ass *definition-groups*))))
     ((annIsAssign? e)
      (let* ((ref (annFetchAssignRef e))
	     (type (annExprFetchType ref))
	     (arg (annFetchAssignArg e)))
	(loop ref)
	(loop arg)
	(annExprSetType! ref (union-ps-node type *reference-groups*))))
     ((annIsEval? e)
      (loop (annFetchEvalBody e))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-tuple-ctor n)
  (any->symbol "TUPLE-" n))

(define (make-tuple-sel n i)
  (any->symbol "TUPLE-" n "->" i))

(define (make-tuple-definition n)
  (let ((ctor (make-tuple-ctor n)))
  `(DEFINE-DATA ,ctor
     ,(cons ctor
	    (let loop ((i 1))
	      (if (> i n)
		  '()
		  (cons (make-tuple-sel n i)
			(loop (+ i 1)))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (split-d* d*)
  (map split-d d*))

(define (split-d d)
  (annMakeDef (annDefFetchProcName d)
	      (annDefFetchProcFormals d)
	      (split-e (annDefFetchProcBody d))))

(define (split-e e)
  (let loop ((e e))
    (cond
     ((annIsVar? e)
      e)
     ((annIsConst? e)
      e)
     ((annIsCond? e)
      (let ((test-exp (loop (annFetchCondTest e)))
	    (then-exp (loop (annFetchCondThen e)))
	    (else-exp (loop (annFetchCondElse e))))
	(annMakeCond test-exp then-exp else-exp)))
     ((annIsOp? e)
      (let ((args (map loop (annFetchOpArgs e))))
	(annMakeOp (annFetchOpName e) args)))
     ((annIsCall? e)
      (let ((args (map loop (annFetchCallArgs e))))
	(annMakeCall (annFetchCallName e) args)))
     ((annIsLet? e)
      (let ((header (loop (annFetchLetHeader e)))
	    (body (loop (annFetchLetBody e))))
	(annMakeLet (annFetchLetVar e) header body)))
     ((annIsBegin? e)
      (let ((header (loop (annFetchBeginHeader e)))
	    (body (loop (annFetchBeginBody e))))
	(annMakeBegin header body)))
     ;;(annIsVLambda? e)
     ((annIsLambda? e)
      (annMakeLambda (annFetchLambdaLabel e)
		     (annFetchLambdaVars e)
		     (loop (annFetchLambdaBody e))))
     ((annIsApp? e)
      (let ((rator (loop (annFetchAppRator e)))
	    (rands (map loop (annFetchAppRands e))))
	(annMakeApp rator rands)))
     ((annIsCtor? e)
      (let ((args (map loop (annFetchCtorArgs e))))
	(annMakeCtor (annFetchCtorName e)
		     (annFetchCtorLabel e)
		     (annFetchCtorDesc e)
		     args)))
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
      (let* ((label (annFetchRefLabel e))
	     (span (vector-ref *reference-group-span*
			       (find-ecr label *reference-groups*)))
	     (arg (loop (annFetchRefArg e))))
	(if (<= 1 span)
	    (annMakeRef label arg)
	    (annMakeLet 'span arg
			(annMakeOp (make-tuple-ctor span)
				   (let loop ((i 1))
				     (if (> i span)
					 '()
					 (begin
					   (annMakeRef
					    label (annMakeVar 'span))
					   (loop (+ i 1))))))))))
     ((annIsDeref? e)
      (let* ((ref (annFetchDerefArg e))
	     (type-ecr (annExprFetchType ref))
	     (effect-ecr (annExprFetchEffect e))
	     (span (vector-ref *reference-group-span*
			       (find-ecr type-ecr
					 *reference-groups*)))
	     (index (vector-ref *definition-numbering*
				(find-ecr effect-ecr
					  *definition-groups*)))
	     (arg (loop ref)))
	(if (<= 1 span)
	    (annMakeDeref arg)
	    (annMakeDeref (annMakeOp (make-tuple-sel span index arg))))))
     ((annIsAssign? e)
      (let* ((ref (annFetchAssignRef e))
	     (type-ecr (annExprFetchType ref))
	     (arg (annFetchAssignArg e))
	     (label (annFetchAssignLabel e))
	     (span (vector-ref *reference-group-span*
			       (find-ecr type-ecr
					 *reference-groups*)))
	     (index (vector-ref *definition-numbering*
				(find-ecr label
					  *definition-groups*)))
	     (ref-exp (loop ref))
	     (arg-exp (loop arg)))
	(if (<= 1 span)
	    (annMakeAssign label ref-exp arg-exp)
	    (annMakeAssign label
			   (annMakeOp (make-tuple-sel span index ref-exp))
			   arg-exp))))
     ((annIsEval? e)
      (annMakeEval '??? (loop (annFetchEvalBody e)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (ann-print-d* d*)
  (map ann-print-d d*))

(define (ann-print-d d)
  (let ((formals (annDefFetchProcFormals d))
	(name (annDefFetchProcName d))
	(body (annDefFetchProcBody d)))
    `(DEFINE (,name ,@formals) ,(ann-print-e body))))

(define (ann-print-e e)
  (let loop ((e e))
    ;; (ps-print-node (annExprFetchType e))
    (cond
     ((annIsVar? e)
      (annFetchVar e))
     ((annIsConst? e)
      (let ((lit (annFetchConst e)))
	(if (pair? lit)
	    `',lit
	    lit)))
     ((annIsCond? e)
      (let* ((test-out (loop (annFetchCondTest e)))
	     (then-out (loop (annFetchCondThen e)))
	     (else-out (loop (annFetchCondElse e))))
	`(IF ,test-out ,then-out ,else-out)))
     ((annIsOp? e)
      (let ((args (map loop (annFetchOpArgs e))))
	`(,(annFetchOpName e) ,@args)))
     ((annIsCall? e)
      (let ((args (map loop (annFetchCallArgs e)))
	    (name (annFetchCallName e)))
	`(,name ,@args)))
     ((annIsLet? e)
      (let ((var (annFetchLetVar e))
	    (header (loop (annFetchLetHeader e)))
	    (body (loop (annFetchLetBody e))))
	`(LET ((,var ,header)) ,body)))
     ((annIsBegin? e)
      (let ((header (loop (annFetchBeginHeader e)))
	    (body (loop (annFetchBeginBody e))))
	`(BEGIN ,header ,body)))
     ;;(annIsVLambda? e)
     ((annIsLambda? e)
      (let ((vars (annFetchLambdaVars e))
	    (body (loop (annFetchLambdaBody e)))
	    (label (annFetchLambdaLabel e)))
	`(LAMBDA ,label ,vars ,body)))
     ((annIsApp? e)
      (let* ((rator (loop (annFetchAppRator e)))
	     (rands (map loop (annFetchAppRands e))))
	`(,rator ,@rands)))
     ((annIsCtor? e)
      (let ((args (map loop (annFetchCtorArgs e)))
	    (ctor (annFetchCtorName e))
	    (label (annFetchCtorLabel e)))
	`(,ctor ,label ,@args)))
     ((annIsSel? e)
      (let ((arg (loop (annFetchSelArg e)))
	    (sel (annFetchSelName e)))
	`(,sel ,arg)))
     ((annIsTest? e)
      (let ((arg (loop (annFetchTestArg e)))
	    (tst (annFetchTestName e)))
	`(,tst ,arg)))
     ((annIsRef? e)
      (let ((arg (loop (annFetchRefArg e)))
	    (label (annFetchRefLabel e)))
	`(MAKE-CELL ,label ,arg)))
     ((annIsDeref? e)
      (let* ((ref (annFetchDerefArg e))
	     (ref-group (annExprFetchType ref))
	     (def-group (annExprFetchEffect e)))
	`(CELL-REF ,(expand-group ref-group *reference-groups*)
		   ,(expand-group def-group *definition-groups*)
		   ,(loop ref))))
     ((annIsAssign? e)
      (let* ((ref (annFetchAssignRef e))
	     (arg (loop (annFetchAssignArg e)))
	     (label (annFetchAssignLabel e))
	     (ref-group (annExprFetchType ref)))
	`(CELL-Set! ,(expand-group ref-group *reference-groups*)
		    ,label ,(loop ref) ,arg)))
     ((annIsEval? e)
      `(EVAL ,(loop (annFetchEvalBody e)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cogen-driver job-file/files skeleton)
  (set-scheme->abssyn-let-insertion! #f)
  (set-abssyn-maybe-coerce! #f)
  (let* ((source-files
	  (if (string? job-file/files)
	      (map symbol->string (file->list job-file/files))
	      job-file/files))
	 (full-source
	  (apply append (map file->list source-files)))
	 (def-function*
	   (filter (lambda (defn) (or (equal? (car defn) 'define)
				      (equal? (car defn) 'define-without-memoization)))
		   full-source))
	 (def-datatype*
	   (filter (lambda (defn) (equal? (car defn) 'define-data))
		   full-source))
	 (def-typesig*
	   (filter (lambda (defn) (equal? (car defn) 'define-type))
		   full-source))
	 (def-opsig*
	   (filter (lambda (defn) (equal? (car defn) 'define-primitive))
		   full-source))
	 (def-memo*
	   (filter (lambda (defn) (equal? (car defn) 'define-memo))
		   full-source))
	 (symbol-table
	  (scheme->abssyn-define-type
	   def-datatype* def-typesig* def-opsig* def-memo*))
	 (abssyn-d* (scheme->abssyn-d def-function* symbol-table)))
    (set-reach-max-number! *scheme->abssyn-label-counter*)
    (set-labset-size! (+ *scheme->abssyn-label-counter* 1))
    (ps-prepare-graph-d* abssyn-d*)
    (ps-generate-constraints-d* abssyn-d*)
    (with-output-to-file "/tmp/rea.scm"
      (lambda () (rea-fix abssyn-d*)))
    (group-d* abssyn-d*)
    (with-output-to-file "/tmp/ps-output.scm"
      (lambda ()
	(p (ann-print-d* abssyn-d*))))
    ;; (split-d* abssyn-d*)
    ))
