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
	 (ctors (cddr dc))
	 (nt (get-nt ctors)))
    (let loop ((ctors ctors) (np 0))
      (if (null? ctors)
	  '()
	  (let* ((ctor (car ctors))
		 (nc (get-nc ctor)))
	    (append (one-ctor
		     (list type-name (car ctor) np nc nt) ctor)
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
;;; D  ::= (define-primitive O T [dynamic|error|opaque|apply|pure])
(define (one-defop dt)
  (let* ((op-name (cadr dt))
	 (op-type (caddr dt))
	 (op-optional (cdddr dt))
	 (op-option (and (pair? op-optional) (car op-optional)))
	 (op-apair (assoc op-option wft-property-table)) ;defined in cogen-eq-flow
	 (st-entry (list op-name
			 (annMakeOpCoerce
			  (eq? op-option 'pure) ;opacity
			  (and op-apair (cdr op-apair))          ;property (a function)
			  #f
			  (parse-type op-type))	          ;type
			 -1
			 ;;(length op-rand-types)
			 )))
    st-entry))

(define (one-defmemo dm)
  (let* ((memo-name (cadr dm))
	 (level (caddr dm)))
    (list memo-name
	  (annMakeOp1 #t
		      (wft-make-memo-property level)
		      (bta-make-memo-postprocessor level)
		      (parse-type '(all t t)))
	  1)))


;;; T  ::= (all TV T) | (rec TV T) | (TC T*) | TV
;;; TV type variable (must be bound by rec or all)
;;; TC type constructor
;;; abstract syntax:
(define-record primop (op-name op-type op-prop))
(define-record primop-tapp (tcon types))
(define-record primop-trec (tvar type))
(define-record primop-tall (tvar type))
(define-record primop-tvar (tvar))

(define parse-type
  (lambda (texp)
    (let loop ((texp texp)
	       (tenv the-empty-env))
      (cond
       ((pair? texp)
	(if (member (car texp) '(all rec))
	    (let ((tvar (cadr texp)))
	      ((if (equal? (car texp) 'rec)
		   make-primop-trec
		   make-primop-tall)
	       tvar
	       (loop (caddr texp)
		     (extend-env tvar (make-primop-tvar tvar) tenv))))
	    (make-primop-tapp (car texp)
			      (map (lambda (texp)
				     (loop texp tenv))
				   (cdr texp)))))
       (else
	(apply-env tenv texp
		   (lambda ()
		     (make-primop-tapp texp '()))))))))


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
