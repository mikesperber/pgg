;;; $Id$
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
	 ((dynamic) (project-dynamic ctor-vvs bts))
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
(define (project-dynamic value bt-args)
  (let* ((values-by-bt
	  (project-dynamic-level (cdr value) bt-args 0))
	 (nested-dynamics
	  (let loop ((values (car values-by-bt)))
	    (if (null? values)
		'()
		(let ((value (car values))
		      (values (cdr values)))
		  (if (procedure? value)
		      (let ((this-dynamics (value 'DYNAMIC)))
			(multi-append this-dynamics (loop values)))
		      (loop values)))))))
    (multi-append nested-dynamics (cdr values-by-bt)))) 

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
	  (gensym-local value)
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
    (lambda (what)
      (case what
	((value) the-ref)
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
		      (project-dynamic (list 'FOO (cell-ref the-ref))
				       (list bt)))
		    (lambda (normalized)
		      '())))
	((clone) (register-address
		  static-address
		  (lambda (normalized)
		    (let* ((value (cell-ref the-ref))
			   (cloned-value (clone-one-dynamic value bt))
			   (new-cell (static-cell label cloned-value bt)))
		      (enter-address-map! normalized new-cell the-ref)
		      new-cell))
		  (lambda (normalized)
		    (address-map->new-cell normalized))))
	((clone-with)
	 (lambda (clone-map)
	   (register-address
	    static-address
	    (lambda (normalized)
	      (let* ((value (cell-ref the-ref))
		     (cloned-value (clone-with-one value bt clone-map))
		     (new-cell (static-cell label cloned-value bt)))
		(enter-address-map! normalized new-cell the-ref)
		new-cell))
	    (lambda (normalized)
	      (address-map->new-cell normalized)))))))))

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
(define (enter-address-map! key val1 val2)
  (set! *address-map*
	(cons (list key val1 val2) *address-map*)))
(define (address-map->new-cell key)
  (let ((found (assoc key *address-map*)))
    (if found
	(cadr found)
	(error "address-map->new-cell" key))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; toplevel projection functions
(define (top-project-static value bts)
  (address-registry-reset!)
  (project-static value bts))
(define (top-project-dynamic value bts)
  (address-registry-reset!)
  (project-dynamic value bts))
(define (top-clone-dynamic value bts)
  (address-registry-reset!)
  (address-map-reset!)
  (clone-dynamic value bts))
(define (top-clone-with clone-map value bts)
  (address-registry-reset!)
  (address-map-reset!)
  (clone-with clone-map value bts))
