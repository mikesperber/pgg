;;; support procedures for 2lazy.scm

(define (input->nr-cvars xs*)
  (if (null? xs*)
      0
      (let ((xs (car xs*))
	    (xs* (cdr xs*)))
	(if (and (pair? xs) (equal? (car xs) 'CV))
	    (max (cadr xs) (input->nr-cvars xs*))
	    (input->nr-cvars xs*)))))
;;; programs
;;; prg  ::= proc*
;;; proc ::= (f x1 ... xn = e)

(define (prg-lookup f prg)
  (if (null? prg)
      (static-error "function not found in program" f)
      (if (equal? (caar prg) f)
	  (car prg)
	  (prg-lookup f (cdr prg)))))

(define (proc->formals proc)
  (let loop ((parts (cdr proc)) (formals '()))
    (if (or (null? parts)
	    (equal? (car parts) '=))
	(reverse formals)
	(loop (cdr parts) (cons (car parts) formals)))))

(define (proc->expr proc)
  (let loop ((parts (cdr proc)))
    (if (null? parts)
	(static-error "syntax error in" proc)
	(if (equal? (car parts) '=)
	    (cadr parts)
	    (loop (cdr parts))))))

(define operator-list '(+ - * / cons bin-car bin-cdr equal?))
(define (static-ext op arg1 arg2)
  (case op
    ((+) (+ arg1 arg2))
    ((-) (- arg1 arg2))
    ((*) (* arg1 arg2))
    ((/) (/ arg1 arg2))
    ((cons) (cons arg1 arg2))
    ((bin-car) (car arg1))
    ((bin-cdr) (cdr arg1))
    ((equal?) (equal? arg1 arg2))
    (else 'undefined-primitive)))

;;; syntax accessors
(define (var-expr? e)
  (symbol? e))
(define (var->name e)
  e)
(define (constant-expr? e)
  (or (number? e)
      (boolean? e)
      (string? e)
      (and (pair? e) (eq? (car e) 'QUOTE))))
(define (constant->value e)
  (if (pair? e)
      (cadr e)
      e))
(define (cond-expr? e)
  (and (pair? e) (eq? (car e) 'IF)))
(define (binop-expr? e)
  (and (pair? e) (memq (car e) operator-list)))
(define (funcall-expr? e)
  (and (pair? e) (symbol? (car e))))
(define (static-error . args)
  (apply error args))

;;; example programs
(define lazy1
  '((f x    = (g x (loop x)))
    (g x y  = (+ x x))
    (loop x = (loop x))))

(define lazy2
  '((f x y z = (g x (+ y z) (- y z)))
    (g x y z = (if x (+ y y) (* z z)))))