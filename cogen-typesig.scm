;;; cogen-typesig.scm

;;; copyright © 1996, 1997, 1998, 1999, 2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; type declarations

(define (process-type-declarations def-type*)
  (let loop ((def-type* def-type*)
	     (symtab '()))
    (if (pair? def-type*)
	(let ((def (car def-type*))
	      (def-type* (cdr def-type*)))
	  (case (car def)
	    ((define-type)
	     (loop def-type* (cons (one-deftype def) symtab)))
	    ((define-primitive)
	     (loop def-type* (cons (one-defop def) symtab)))
	    ((define-memo)
	     (loop def-type* (cons (one-defmemo def) symtab)))
	    ((define-data)
	     (loop def-type* (append (one-defdata def) symtab)))
	    (else
	     (error "illegal declaration" (car def)))))
	symtab)))

;;; process (defdata t (c1 s11 ... s1n) ... (cm sm1 ... smn))
;;; a constructor description for ci is a list
;;; (t np nc nt)
;;; where np = sum of # args of c[1] ... c[i-1]
;;;       nc = # args of c[i]
;;;       nt = sum of # args of c[1] ... c[m]
;(define (desc-type desc) (list-ref desc 0))
;(define (desc-np desc) (list-ref desc 1))
;(define (desc-nc desc) (list-ref desc 2))
;(define (desc-nt desc) (list-ref desc 3))
(define (get-nc ctor)
  (- (length ctor) 1))
(define (get-nt ctors)
  (apply + (map get-nc ctors)))

(define (one-defdata dc)
  (let* ((type-name (cadr dc))
	 (rest (cddr dc))
	 (hidden (and (pair? rest) (eq? (car rest) 'hidden)))
	 (ctors (if hidden (cdr rest) rest))
	 (nt (get-nt ctors)))
    (let loop ((ctors ctors) (np 0))
      (if (null? ctors)
	  '()
	  (let* ((ctor (car ctors))
		 (nc (get-nc ctor)))
	    (append (one-ctor
		     (list type-name (car ctor) np nc nt hidden) ctor)
		    (loop (cdr ctors) (+ np nc))))))))
(define (one-ctor desc ctor)
  (let* ((the-ctor (car ctor))
	 (the-test (string->symbol
		    (string-append (symbol->string the-ctor) "?")))
	 (selectors (cdr ctor)))
  (cons
   (list the-ctor (scheme->abssyn-make-ctor1 desc) (length selectors))
   (cons
    (list the-test (annMakeTest1 the-ctor desc) 1)
    (let loop ((selectors selectors) (i 1))
      (if (null? selectors)
	  '()
	  (cons
	   (list (car selectors) (annMakeSel1 the-ctor desc i) 1)
	   (loop (cdr selectors) (+ i 1))))))))) 
;;; process (define-type (P B1 ... Bn) B0)
(define (one-deftype dt)
  (let ((template (cadr dt))
	(result-type (caddr dt)))
    (list (car template) scheme->abssyn-make-call (length (cdr template)))))

;;; process (define-primitive ...)
;;; accepts the following syntax for definitions of primitive operators
;;; D  ::= (define-primitive O T [dynamic|error|opaque|apply|pure|0|1|2|...])
(define (one-defop dt)
  (let* ((op-name (cadr dt))
	 (op-type (caddr dt))
	 (op-optional (cdddr dt))
	 (op-option (and (pair? op-optional) (car op-optional)))
	 (op-apair (assoc op-option wft-property-table)) ;defined in cogen-eq-flow
	 (st-entry (list op-name
			 ((if (or (eq? op-option 'apply)
				  (number? op-option))
			      annMakeOp1
			      annMakeOpCoerce)
			  (eq? op-option 'pure) ;opacity
			  (if (number? op-option)
			      op-option
			      (and op-apair (cdr op-apair))) ;property (a function)
			  #f
			  (parse-type op-type))	          ;type
			 -1
			 ;;(length op-rand-types)
			 )))
    st-entry))

;;; (define-memo M level [active])
;;; - M		name of the function
;;; - level	binding time of the memoization point
;;; - active	memo point is active in specializations with level >= active
;;;             if active is 'deferred (verbatim!)
;;;		then make a deferred memoization point
(define (one-defmemo dm)
  (let* ((memo-name (cadr dm))
	 (memo-level (caddr dm))
	 (active-level (if (pair? (cdddr dm))
			   (cadddr dm)
			   0))
	 (memo-type (if (number? active-level)
			'(all t t)
			'(all t (-> b (-> b b t) t)))))
    (list memo-name
	  (annMakeOp1 #t
		      (wft-make-memo-property memo-level active-level)
		      (bta-make-memo-postprocessor memo-level active-level)
		      (parse-type memo-type))
	  1)))


;;; T  ::= (all TV T) | (rec TV T) | (TC T*) | TV
;;; TV type variable (must be bound by rec or all)
;;; TC type constructor
;;; abstract syntax:
(define-record primop (op-name op-type op-prop))
(define-record type-app (tcon types))
(define-record type-rec (tvar type))
(define-record type-all (tvar type))
(define-record type-var (tvar))
;;; typical type: (-> b b b) interprets as   (b b) -> b

(define parse-type
  (lambda (texp)
    (and
     (not (eq? texp '-))
     (let loop ((texp texp)
		(tenv the-empty-env))
       (cond
	((pair? texp)
	 (if (member (car texp) '(all rec))
	     (let ((tvar (cadr texp)))
	       ((if (equal? (car texp) 'rec)
		    make-type-rec
		    make-type-all)
		tvar
		(loop (caddr texp)
		      (extend-env tvar (make-type-var tvar) tenv))))
	     (make-type-app (car texp)
			    (map (lambda (texp)
				   (loop texp tenv))
				 (cdr texp)))))
	(else
	 (apply-env tenv texp
		    (lambda ()
		      (make-type-app texp '())))))))))


;;; a constructor description for ci is a list
;;; (t ctor np nc nt)
;;; where np = sum of # args of c[1] ... c[i-1]
;;;       nc = # args of c[i]
;;;       nt = sum of # args of c[1] ... c[m]
(define (desc-type desc) (list-ref desc 0))
(define (desc-ctor desc) (list-ref desc 1))
(define (desc-np desc) (list-ref desc 2))
(define (desc-nc desc) (list-ref desc 3))
(define (desc-nt desc) (list-ref desc 4))
(define (desc-hidden desc) (list-ref desc 5))
