;;; cogen-library.scm

;;; copyright © 1996-2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact


;;; projection functions for memoization
;;; parameterized over `result'
;;;


;;; code that must be executed at run time of a static lambda or
;;; constructor with dynamic free variables
;;; improvements:
;;; - specialize this guy wrt `label' and `bts', as well
;;;   as project-static, project-dynamic, and clone-dynamic!
;;; - use delay/force to memoize value, static and dynamic
;;;
;;; static-constructor : Ident \times (2Value^* -> (K Code)^* -> K
;;; Code) \times 2Value^* \times BT^* -> Value
(define (static-constructor ctor closed-value vvs bts)
  ;; (let ((closed-value (lambda fvs (lambda (arg) body)))) ...)
  (let ((ctor-vvs (cons ctor vvs)))
     (lambda (what)
       (case what
	 ((value)   (apply closed-value vvs))
	 ((static)  (project-static ctor-vvs bts))
	 ((dynamic dynamic-ref) (project-dynamic ctor-vvs bts what))
	 ((clone)
	  (static-constructor ctor
			      closed-value
			      (cdr (clone-dynamic ctor-vvs bts))
			      bts))
	 ((clone-with)
	  (lambda (clone-map)
	    (static-constructor ctor
				closed-value
				(cdr (clone-with clone-map ctor-vvs bts))
				bts)))
	 ((serialize)
	  `(static-constructor ',ctor ,ctor
			       (list ,@(map serialize-one vvs bts))
			       ',bts))
	 (else
	  (error (string-append "static-constructor: bad argument " (symbol->string what)))))))) 

(define (hidden-constructor ctor closed-value vvs bts)
  (let ((v (static-constructor ctor closed-value vvs bts)))
    (lambda (what)
      (case what
	((static)
	 '(hidden))
	((clone)
	 (hidden-constructor ctor
			     closed-value
			     (cdr (clone-dynamic (cons ctor vvs) bts))
			     bts))
	((serialize)
	 (list 'unquote
	       `(hidden-constructor ',ctor ,ctor
				    (list ,@(map serialize-one vvs bts))
				    ',bts)))
	(else
	 (v what))))))

(define *poly-registry* 'undefined-poly-registry)
(define (poly-registry-reset!)
  (set! *poly-registry* '()))
;; an entry of the *poly-registry*
;; (ctor index-map spec-procs)
;; ctor: unique label of the lambda-poly
;; spec-procs: list of procedures that perform the actual specialization
;; index-map: list of (statics pp index-value body-statics value-template)
(define (poly-constructor ctor arity bts body-level proc the-residual-piece dyn)
  (let* ((boxed-gensym-counter (gensym-local-hold))
	 (ctor (gensym 'poly))		;not always correct
	 (spec-proc
	  (lambda (pp old-body-statics-boxed value-template-boxed)
	    (with-held-gensym-local boxed-gensym-counter
	      (lambda ()
		(let* ((cloned (clone-dynamic pp bts))
		       (dynamics (map car (project-dynamic cloned bts 'dynamic))))
		  (set-cdr! the-residual-piece (list '***))
		  (set! the-residual-piece (cdr the-residual-piece))
		  (let ((my-residual-piece the-residual-piece))
		    (set-car!
		     my-residual-piece
		     `(LAMBDA
		       ,dynamics
		       ,(reset
			 (let ((v (apply proc (cdr cloned))))
			   (if (not (zero? body-level))
			       v
			       (let* ((rv `(RETURN ,v))
				      (body-statics
				       (top-project-static rv (list body-level)))
				      (body-dynamics
				       (top-project-dynamic rv (list body-level)))
				      (body-actuals
				       (map car body-dynamics))
				      (old-body-statics
				       (car old-body-statics-boxed)))
				 (if old-body-statics
				     (if (not (equal? body-statics old-body-statics))
					 (error "return type mismatch in lambda-poly"))
				     (begin
				       (set-car! old-body-statics-boxed body-statics)
				       (set-car! value-template-boxed rv)))
				 (if (= 1 (length body-actuals))
				     (car body-actuals)
				     `(VALUES ,@body-actuals))))))))))))))
	 (ctor-entry
	  (or (assoc ctor *poly-registry*)
	      (let* ((ctor-entry (list ctor '() '())))
		(set! *poly-registry* (cons ctor-entry *poly-registry*))
		ctor-entry)))
	 (index-map (cadr ctor-entry))
	 (spec-procs (caddr ctor-entry)))
    (set-car! (cddr ctor-entry) (cons spec-proc spec-procs))
    (for-each (lambda (index-entry)
		(let ((pp (cadr index-entry))
		      (old-body-statics-boxed (cdddr index-entry))
		      (value-template-boxed (cddddr index-entry)))
		  (spec-proc pp old-body-statics-boxed value-template-boxed)))
	      index-map)
    (poly-constructor-internal ctor arity bts body-level dyn ctor-entry)))

(define poly-constructor-internal
  (lambda (ctor arity bts body-level dyn registry-entry)
    (lambda (what)
      (case what
	((value)
	 (lambda args
	   (let* ((pp (cons ctor args))
		  (index (project-static pp bts))
		  (the-index-map (cadr registry-entry))
		  (spec-procs (caddr registry-entry))
		  (found
		   (or (assoc index the-index-map)
		       (let* ((len (length the-index-map))
			      (entry (list index pp len #f #f))
			      (old-body-statics-boxed (cdddr entry))
			      (value-template-boxed (cddddr entry)))
			 (set-car! (cdr registry-entry)
				   (cons entry the-index-map))
			 (for-each
			  (lambda (proc)
			    (proc pp old-body-statics-boxed value-template-boxed))
			  spec-procs)
			 entry)))
		  (actuals (map car (project-dynamic pp bts 'dynamic))))
	     (let loop ()	;; busy waiting would not work with true concurrency
	       (if (not (car (cddddr found)))
		   (begin
		     (relinquish-timeslice)
		     (loop))))
	     (let* ((cloned-return-v (top-clone-dynamic (car (cddddr found)) (list body-level)))
		    (dynamics (top-project-dynamic cloned-return-v (list body-level)))
		    (formals (map car dynamics)))
	       (if (= 1 (length formals))
		   (shift k
			  `(LET ((,(car formals) ((vector-ref ,dyn ,(caddr found)) ,@actuals)))
				,(k (cadr cloned-return-v))))
		   (shift k
			  `(CALL-WITH-VALUES
			    (LAMBDA () ((vector-ref ,dyn ,(caddr found)) ,@actuals))
			    (LAMBDA ,formals ,(k (cadr cloned-return-v))))))))))
	((static)  (list ctor))
	((dynamic dynamic-ref) (list (cons dyn 1)))
	((clone)
	 (poly-constructor-internal ctor
				    arity
				    bts body-level
				    (clone-one-dynamic dyn 1)
				    registry-entry))
	((clone-with)
	 (lambda (clone-map)
	   (poly-constructor-internal ctor
				      arity
				      bts body-level
				      (clone-with-one dyn 1 clone-map)
				      registry-entry)))
	(else
	 (error (string-append "poly-constructor: bad argument " (symbol->string what))))))))

(define (serialize-one val bt)
  (if (procedure? val)
      (val 'serialize)
      (if (zero? bt)
	  (if (or (symbol? val) (pair? val) (null? val))
	      `',val
	      val)
	  ''dyn)))

(define (serialize pp bts)
  `(LIST ',(car pp) ,@(map serialize-one (cdr pp) bts)))

;;; extract the static parts out of a partially static value which
;;; starts with some static tag and the rest of which is described by
;;; the binding times in bt-args
(define (project-static value bt-args)
  (cons (car value)
	(let loop ((values (cdr value))
		   (bt-args bt-args))
	  (if (null? values)
	      '()
	      (cons (project-one-static (car values) (car bt-args))
		    (loop (cdr values) (cdr bt-args)))))))

(define (project-one-static value bt)
  (if (zero? bt)
      (if (procedure? value)
	  (value 'STATIC)
	  value)
      'DYN))

;;; extract the dynamic parts of value
;;; new: return list of pairs (value, bt)
(define (project-dynamic value bts message)
  (apply append
	 (map (lambda (v bt)
		(project-one-dynamic v bt message))
	      (cdr value) bts)))

(define (project-one-dynamic value bt message)
  (if (zero? bt)
      (if (procedure? value)
	  (value message)
	  '())
      (list (cons value bt))))

;;; clone the dynamic parts of a list of values 
;;; return a value with identical static skeleton, but all dynamic
;;; parts replaced by a fresh variable
(define (clone-one-dynamic value bt)
  (if (= 0 bt)
      (if (procedure? value)
	  (value 'CLONE)
	  value)
      (if (symbol? value)
	  (make-residual-variable (gensym-local-trimmed value))
	  (make-residual-variable (gensym-local 'clone)))))

(define (clone-dynamic value bts)
  (cons (car value)
	(let loop ((values (cdr value)) (bts bts))
	  (if (null? values)
	      '()
	      (let* ((skeleton (loop (cdr values) (cdr bts)))
		     (new-value (clone-one-dynamic (car values) (car bts))))
		(cons new-value skeleton))))))

;;; clone the dynamic parts of a list of values 
;;; return a value with identical static skeleton, but all dynamic
;;; parts replaced AS INDICATED BY CLONE-MAP 
(define (clone-with clone-map value bts)
  (cons (car value)
	(let loop ((values (cdr value)) (bts bts))
	  (if (null? values)
	      '()
	      (cons (clone-with-one (car values) (car bts) clone-map)
		    (loop (cdr values) (cdr bts)))))))

(define (clone-with-one value bt clone-map)
  (if (zero? bt)
      (if (procedure? value)
	  ((value 'CLONE-WITH) clone-map)
	  value)
      (cdr (assoc value clone-map))))

;;; procedures for dealing with references
(define (static-cell label value bt)
  (let ((static-address (gen-address label)))
    (static-cell-at static-address label value bt)))

(define (static-cell-at static-address label value bt)
  (let ((the-ref (make-cell value)))
    (creation-log-add! static-address the-ref bt)
    (lambda (what)
      (case what
	((value) the-ref)
	((cell-set!) (lambda (arg)
		       (reference-log-register static-address the-ref 0
						(cell-ref the-ref))
		       (cell-set! the-ref arg)))
	((static) (register-address
		   static-address
		   (lambda (normalized)
		     `(pointer ,normalized
			       ,(project-one-static (cell-ref the-ref) bt)))
		   (lambda (normalized)
		     `(back ,normalized))))
	((dynamic) (register-address
		    static-address
		    (lambda (normalized)
		      (project-one-dynamic (cell-ref the-ref) bt 'DYNAMIC))
		    (lambda (normalized)
		      '())))
	((dynamic-ref) (register-address static-address
					 (lambda (normalized) '())
					 (lambda (normalized) '())))
	((clone) (register-address
		  static-address
		  (lambda (normalized)
		    (let* ;; ordering is crucial below
			((value (cell-ref the-ref))
			 (new-cell (static-cell label 'DUMMY bt))
			 (new-ref (new-cell 'VALUE)))
		      (enter-address-map! normalized new-cell bt the-ref)
		      (cell-set! new-ref (clone-one-dynamic value bt))
		      new-cell))
		  (lambda (normalized)
		    (address-map->new-cell normalized))))
	((clone-with)
	 (lambda (clone-map)
	   (register-address
	    static-address
	    (lambda (normalized)
	      (let* ((value (cell-ref the-ref))
		     (new-cell (static-cell label 'DUMMY bt))
		     (new-ref (new-cell 'VALUE)))
		(enter-address-map! normalized new-cell bt the-ref)
		(cell-set! new-ref (clone-with-one value bt clone-map))
		new-cell))
	    (lambda (normalized)
	      (address-map->new-cell normalized)))))
	((serialize)
	 `(static-cell-at ',static-address ,label ,(serialize-one value bt) ,bt))
	(else
	 (error (string-append "static-cell: bad message " (symbol->string what))))))))

;;; procedures for dealing with vectors
(define (static-vector label size value bt)
  (let ((static-address (gen-address label))
	(the-vector (make-vector size value))
	(the-bts (let loop ((i 0) (r '()))
		   (if (< i size)
		       (loop (+ i 1) (cons bt r))
		       r))))
    (creation-log-add! static-address the-vector bt)
    (lambda (what)
      (case what
	((value)
	 the-vector)
	((vector-set!)
	 (lambda (index arg)
	   (reference-log-register static-address the-vector index
				   (vector-ref the-vector index))
	   (vector-set! the-vector index arg)))
	((vector-fill!)
	 (lambda (arg)
	   (let loop ((i (- size 1)))
	     (if (>= i 0)
		 (begin
		   (reference-log-register static-address the-vector i
					   (vector-ref the-vector i))
		   (loop (- i 1)))))
	   (vector-fill! the-vector arg)))
	((static)
	 (register-address
	  static-address
	  (lambda (normalized)
	    `(vector ,normalized
		     ,@(map (lambda (x) (project-one-static x bt))
			    (vector->list the-vector))))
	  (lambda (normalized)
	    `(back ,normalized))))
	((dynamic)
	 (register-address
	  static-address
	  (lambda (normalized)
	    (project-dynamic (cons 'vector (vector->list the-vector))
			     the-bts
			     'DYNAMIC))
	  (lambda (normalized)
	    '())))
	((dynamic-ref)
	 (register-address static-address
			   (lambda (normalized) '())
			   (lambda (normalized) '())))
	((clone clone-with)
	 (register-address
	  static-address
	  (lambda (normalized)
	    (let* ;; ordering is crucial below
		((new-value (static-vector label size 'DUMMY bt))
		   (new-vector (new-value 'VALUE)))
	      (enter-address-map! normalized new-value bt the-vector)
	      (let loop ((i 0))
		(if (< i size)
		    (begin
		      (vector-set! new-vector i
				   ((if (eq? what 'clone)
					clone-one-dynamic
					clone-with-one)
				    (vector-ref the-vector i)
				    bt))
		      (loop (+ i 1)))))
	      new-value))
	  (lambda (normalized)
	    (address-map->new-cell normalized))))))))

;;; maintain a mapping from global static addresses to local static addresses
(define *address-registry* 'undefined-address-registry)
(define *local-address-registry* 'undefined-local-address-registry)
(define (address-registry-reset!)
  (set! *address-registry* '())
  (set! *local-address-registry* 0))
(define (gen-local-address label)
  (set! *local-address-registry* (+ 1 *local-address-registry*))
  (cons label *local-address-registry*))
(define (register-address static-address first-cont next-cont)
  (let ((found (assoc static-address *address-registry*)))
    ;;(display found) (newline)
    (if found
	(next-cont (cdr found))
	(let ((local-address (gen-local-address (car static-address))))
	  (set! *address-registry*
		(cons (cons static-address local-address) *address-registry*))
	  (first-cont local-address)))))

;;; maintain a mapping from local addresses to new value and old ref
(define *address-map* 'undefined-address-map)
(define (address-map-reset!)
  (set! *address-map* '()))
(define (enter-address-map! key ref1 bt ref2)
  (set! *address-map*
	(cons (list key ref1 ref2) *address-map*)))
(define (address-map->new-cell key)
  (let ((found (assoc key *address-map*)))
    (if found
	(cadr found)
	(error "address-map->new-cell" key))))

(define *creation-log* #f)
(define (creation-log-initialize!)
  (set! *creation-log* '()))
(define (creation-log-push!)
  (set! *creation-log* (cons '() *creation-log*)))
(define (creation-log-pop!)
  (set! *creation-log* (cdr *creation-log*)))
(define (creation-log-add! static-address new-ref bt)
  (set-car! *creation-log* (cons (list static-address new-ref bt)
				 (car *creation-log*))))
(define (creation-log-top)
  (car *creation-log*))

;;; extract dynamic components from the static store
;;; assumes that creation-log is already restricted to the static effects
(define (project-dynamic-creation-log creation-log)
  ;; some initialization goes here
  (let loop ((creation-log creation-log))
    (if (null? creation-log)
	'()
	(let* ((entry (car creation-log))
	       (creation-log (cdr creation-log))
	       (static-address (car entry)))
	  (append (project-one-dynamic (cell-ref (cadr entry))
				       (caddr entry)
				       'DYNAMIC-REF)
		  (loop creation-log))))))
;;; the memolist needs to contain a copy of the restricted creation
;;; log. This must then be cloned in a special way.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define *reference-log* #f)
(define (reference-log-initialize!)
  (set! *reference-log* (list '())))
(define (reference-log-push!)
  (let ((checkpoint *reference-log*))
    (set! *reference-log* (cons '() *reference-log*))
    checkpoint))
(define (reference-log-pop!)
  (let ((entry (car *reference-log*)))
    (set! *reference-log* (cdr *reference-log*))
    entry))
(define (reference-log-register static-address the-ref index value)
  (set-car! *reference-log*
	    (cons (list the-ref index value)
		  (car *reference-log*))))
(define (reference-log-rollback! checkpoint)
  (let loop ()
    (cond
     ((eq? *reference-log* checkpoint) #t)
     ((null? *reference-log*)
      (error "reference-log-rollback! reached end of log"))
     (else
      (let inner-loop ((ref+values (reference-log-pop!)))
	(if (null? ref+values)
	    (loop)
	    (let ((ref+value (car ref+values))
		  (ref+values (cdr ref+values)))
	      (let ((ref (car ref+value)))
		(if (vector? ref)
		    (vector-set! ref (cadr ref+value) (caddr ref+value))
		    (cell-set! ref (caddr ref+value))))
	      (inner-loop ref+values))))))))

(define current-static-store! reference-log-push!)
(define install-static-store! reference-log-rollback!)
(define initialize-static-store! reference-log-initialize!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toplevel projection functions
(define (top-project-static value bts)
  (address-registry-reset!)
  (project-static value bts))
(define (top-project-dynamic value bts)
  (address-registry-reset!)
  (project-dynamic value bts 'DYNAMIC))
(define (top-clone-dynamic value bts)
  (address-registry-reset!)
  (address-map-reset!)
  (clone-dynamic value bts))
(define (top-clone-with clone-map value bts)
  (address-registry-reset!)
  (address-map-reset!)
  (clone-with clone-map value bts))
