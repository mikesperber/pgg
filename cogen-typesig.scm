;;; operations on type signatures

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
;;; (t np nc nt)
;;; where np = sum of # args of c[1] ... c[i-1]
;;;       nc = # args of c[i]
;;;       nt = sum of # args of c[1] ... c[m]
(define (desc-type desc) (list-ref desc 0))
(define (desc-np desc) (list-ref desc 1))
(define (desc-nc desc) (list-ref desc 2))
(define (desc-nt desc) (list-ref desc 3))
