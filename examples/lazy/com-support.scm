; Similix parser for Lazy combinator source programs
; Copyright (C) 1993 Anders Bondorf
; Please see the file README for copyright notice, license and disclaimer.


;-----------------------------------------------------------------------------
; This program is written as a .sim-program even though we do not
; intend to partially evaluate it.
; The advantage of writing it this way is that the Similix front-end
; can be used for syntax checking.

;-----------------------------------------------------------------------------
;;; (loadt "com-defs.adt")

(define (com-parse file)
  (define (parse-exp e vars fcts)
    (if (simple? e)
	(cond
	 ((com-cst? e)
	  (list 'cst (if (pair? e) (cadr e) e)))
	 ((symbol? e)
	  (cond
	   ((member e vars)
	    (list 'var e))
	   ((member e fcts)
	    (list 'fct e))
	   (else
	    (error 'com-parse "Unbound variable: ~s" e)))))
	(let ((hd (car e))
	      (tl (cdr e)))
	  (cond
	   ((com-binary-operator? hd)
	    (list 'binop
		  hd
		  (parse-exp (cadr e) vars fcts)
		  (parse-exp (caddr e) vars fcts)))
	   ((equal? hd 'if)
	    (list 'if
		  (parse-exp (cadr e) vars fcts)
		  (parse-exp (caddr e) vars fcts)
		  (parse-exp (cadddr e) vars fcts)))
	   (else
	    (let ((ln (length e)))
	      (cond
	       ((= ln 2)
		(list 'apply
		      (parse-exp (car e) vars fcts)
		      (parse-exp (cadr e) vars fcts)))
	       ((> ln 2)
		(parse-exp (cons (list (car e) (cadr e)) (cddr e)) vars fcts))
	       (else
		(error 'com-parse "Unknown expression: ~s" e))))))))) 

  (define (simple? e) (or (com-cst? e) (symbol? e)))

  (define (fct-names P)
    (map (lambda (fct)
	   (let ((n (car fct)))
	     (if (symbol? n)
		 n
		 (error 'com-parse "Not a function name: ~s" n))))
	 P))

  (define (com-binary-operator? b)
    (member b '(cons hack-car hack-cdr equal? + - * / = /= mod)))

  (define (com-cst? e)
    (or (number? e) (boolean? e) (string? e)
	(and (list? e) (= (length e) 2) (equal? (car e) 'quote))))

  (let ((P (file->list file)))
    (letrec
	((parse-fct
	  (lambda (fct)
	    (let ((Fname (car fct)))
	      (let search ((e (cdr fct))
			   (c (lambda (vars body)
				(list Fname vars
				      (parse-exp body vars (fct-names P))))))
		(let ((hd (car e))
		      (tl (cdr e)))
		  (cond
		    ((equal? hd '=)
		     (let ((elem (car tl)))
		       (c '()
			  (if (and (list? tl) (= (length tl) 1)
				   (simple? elem))
			      elem
			      tl))))
		    ((symbol? hd)
		     (search tl (lambda (vars e) (c (cons hd vars) e))))
		    (else
		     (error 'com-parse
				 "Variable name or = expected: ~s" e)))))))))
      (map parse-fct P))))

;-----------------------------------------------------------------------------
; Similix primitives for Lazy combinator interpreter
; Copyright (C) 1993 Anders Bondorf
; Please see the file README for copyright notice, license and disclaimer.


;-----------------------------------------------------------------------------

(define (ext binop value1 value2)
  (case binop
    ((cons) (cons value1 value2))
    ((hack-car) (car value1))
    ((hack-cdr) (cdr value1))
    ((equal?) (equal? value1 value2))
    ((+) (+ value1 value2))
    ((-) (- value1 value2))
    ((*) (* value1 value2))
    ((/) (/ value1 value2))
    ((=) (= value1 value2))
    ((/=) (not (= value1 value2)))
    ((mod) (modulo value1 value2))))

(define err error)

;-----------------------------------------------------------------------------
; Similix thunk primitive for Lazy combinator interpreter
; Copyright (C) 1993 Anders Bondorf
; Please see the file README for copyright notice, license and disclaimer.


;-----------------------------------------------------------------------------
(define (save s)
  (let ((v '())
	(tag #t))
    (lambda ()
      (if tag
	  (begin
	    (set! v (s))
	    (set! tag #f)))
      v)))

;-----------------------------------------------------------------------------
