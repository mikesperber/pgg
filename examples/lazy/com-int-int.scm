; Similix Lazy combinator interpreter
; Copyright (C) 1993 Anders Bondorf
; Please see the file README for copyright notice, license and disclaimer.
; port to the PGG system 1996 Peter Thiemann
(define-primitive save (all t t) dynamic)
(define-primitive err (all t t) error)

(define-primitive dynamize - dynamic)

(define (generalize x)
  (if #t x (dynamize x)))

;-----------------------------------------------------------------------------
; P - Program
; D - Definition
; E - Expression
; C - Constant
; V - Variable
; F - FuncName
; B - Binop

; P ::= D*
; D ::= (F V* = E)
; E ::= C | V | F | (B E1 E2) | (if E1 E2 E3) | (E1 E2)

; Parsed form:
; P ::= (D*)
; D ::= (F (V*) E)
; E ::= (cst C) | (var V) | (fct F) | (binop B E1 E2)
;     | (if E1 E2 E3) | (apply E1 E2)

;------------------------------------------------------------------------------

; (loadt "com-int.adt")
; (loadt "thunk.adt")

;------------------------------------------------------------------------------
; Values are delayed for two resons:
; (1) Environment updating is done by strict functions; therefore,
;     the value argument is delayed (and then forced at lookup-time).
; (2) The interpreted language is lazy so arguments to applications
;     are delayed.

(define (init-fenv)
  (lambda (name)
    (err 'init-fenv "Unbound function: ~s" name)))
(define (upd-fenv name value r)
  (lambda (name1)
    (if (equal? name name1)
	(value) ; force value
	(r name1))))

(define (init-venv)
  (lambda (name)
    (err 'init-venv "Unbound variable: ~s" name)))
(define (upd-venv name value r)
  (lambda (name1)
    (if (equal? name name1)
	(value) ; force value
	(r name1))))

;------------------------------------------------------------------------------

(define (_P P F v) (((fix (lambda (phi) (_D* P phi))) F) (lambda () v)))

(define (_D* D* phi)
  (if (null? D*)
      (init-fenv)
      (let ((D (car D*))
	    (D* (cdr D*)))
	(if (and (list? D) (= 3 (length D)))
	    (let ((F (car D))
		  (V* (cadr D))
		  (E (caddr D)))
	      (upd-fenv F
			(lambda () (_V* V* E (generalize (init-venv)) phi)) ; delay value
			(_D* D* phi)))
     
	    (err '_D* "Illegal program syntax: ~s" D*)))))

(define (_V* V* E r phi)
  (if (not (pair? V*))
      (_E E r phi)
      (let ((V (car V*))
	    (V* (cdr V*)))
	(lambda (s) (_V* V* E (upd-venv V (lambda () s) r) phi)))))

(define (_E E r phi)
  (let ((tag (car E))
	(args (cdr E)))
    (case tag
      ((cst) (let ((C (car args)))
	       C))
      ((var) (let ((V (car args)))
	       ((r V)))) ; force value
      ((fct) (let ((F (car args)))
	       (phi F)))
      ((binop) (let ((B (car args))
		     (E1 (cadr args))
		     (E2 (caddr args)))
		 (ext B (_E E1 r phi) (_E E2 r phi))))
      ((if) (let ((E1 (car args))
		  (E2 (cadr args))
		  (E3 (caddr args)))
	      (if (_E E1 r phi)
		  (_E E2 r phi)
		  (_E E3 r phi))))
      ((apply) (let ((E1 (car args))
		     (E2 (cadr args)))
		 ((_E E1 r phi)
		  (let ((tag (car E2))
			(args (cdr E2)))
		    (case tag
		      ((cst) (let ((C (car args)))
			       (lambda () C)))
		      ((var) (let ((V (car args)))
			       (r V)))
		      ;;((fct) (let ((F (car args)))
		      ;;       (lambda () (phi F))))
		      (else
		       (save (lambda () (_E E2 r phi)))))))))
      (else
       (err '_E "Illegal expression syntax: ~s" E)))))

(define (fix f) (lambda (x) ((f (fix f)) x)))

;-----------------------------------------------------------------------------
