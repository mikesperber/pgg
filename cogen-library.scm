;;; cogen-library.scm

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
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
	 (else
	  (error "static-constructor: bad argument ~a" what)))))) 

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
(define (project-dynamic value bt-args message)
  (let* ((values-by-bt
	  (project-dynamic-level (cdr value) bt-args 0))
	 (nested-dynamics
	  (let loop ((values (car values-by-bt)))
	    (if (null? values)
		'()
		(multi-append (project-one-dynamic (car values) 0 message)
			      (loop (cdr values)))))))
    (multi-append nested-dynamics (cdr values-by-bt))))

(define (project-one-dynamic value bt message)
  (if (zero? bt)
      (if (procedure? value)
	  (value message)
	  '())
      (let loop ((i 0))
	(if (= i bt)
	    (list (list value))
	    (cons '() (loop (+ i 1)))))))

;;; return `values' grouped according to `bt-args' starting with `level'
(define (project-dynamic-level values bt-args level)
  (let loop ((values values)
	     (bt-args bt-args)
	     (rvalues '())
	     (rbt-args '()))
    (if (null? values)
	(if (null? rvalues)
	    (list '())
	    (cons '()
		  (project-dynamic-level (reverse rvalues)
					 (reverse rbt-args)
					 (+ level 1))))
	(if (= level (car bt-args))
	    (let ((values-by-bt
		   (loop (cdr values) (cdr bt-args) rvalues rbt-args)))
	      (cons (cons (car values) (car values-by-bt))
		    (cdr values-by-bt)))
	    (loop (cdr values) (cdr bt-args)
		  (cons (car values) rvalues) (cons (car bt-args)
						    rbt-args))))))

;;; clone the dynamic parts of a list of values 
;;; return a value with identical static skeleton, but all dynamic
;;; parts replaced by a fresh variable
(define (clone-one-dynamic value bt)
  (if (= 0 bt)
      (if (procedure? value)
	  (value 'CLONE)
	  value)
      (if (symbol? value)
	  (gensym-local-trimmed value)
	  (gensym-local 'clone))))

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

;;; (multi-append x y) is almost like (map append x y) except when the
;;; lists x and y have different lengths in which case the result has
;;; the length of the longest argument list
(define (multi-append xss yss)
  (if (pair? xss)
      (let ((xs (car xss)))
	(if (pair? yss)
	    (let ((ys (car yss)))
	      (cons (append xs ys)
		    (multi-append (cdr xss) (cdr yss))))
	    xss))
      yss)) 

;;; reconstruct binding times from a list of value blocks
(define (binding-times blocks)
  (let loop ((blocks blocks) (i 0))
    (if (null? blocks)
	'()
	(let inner-loop ((values (car blocks)))
	  (if (null? values)
	      (loop (cdr blocks) (+ i 1))
	      (cons i (inner-loop (cdr values))))))))

;;; procedures for dealing with references
(define (static-cell label value bt)
  (let ((static-address (gen-address label))
	(the-ref (make-cell value)))
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
	      (address-map->new-cell normalized)))))))))

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
	  (multi-append (project-one-dynamic (cell-ref (cadr entry))
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
