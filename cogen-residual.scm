;;; cogen-residual.scm

;;; copyright © 1996, 1997, 1998, 1999 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;; functions to construct residual code

(define (make-residual-apply fn fa)
  (cond
   ((and (pair? fa) (eq? (car fa) 'QUOTE) (eq? (cadr fa) '()))
    `(,fn))
   ((and (pair? fa) (eq? (car fa) 'LIST))
    `(,fn ,@(cdr fa)))
   (else
    `(APPLY ,fn ,fa))))

(define (make-residual-let var exp body)
  (cond
   ((null? (gensym-local-hold))
    (let ((new-def
	   (if (and (pair? exp) (eq? (car exp) 'BEGIN))
	       `(DEFINE ,var ,@(cdr exp))
	       `(DEFINE ,var ,exp))))
      (add-to-residual-program! new-def))
    body)
   ((eq? var body)
    exp)
   ((and (pair? exp)			; kludge
	 (eq? (car exp) 'SET!))
    (if (and (pair? body) (eq? (car body) 'BEGIN))
	`(BEGIN ,exp ,@(cdr body))
	`(BEGIN ,exp ,body)))
   ((and (pair? body) (memq (car body) '(LET LET*)))
    (let ((header (cadr body))
	  (bodies (cddr body)))
      `(LET* ((,var ,exp) ,@header) ,@bodies)))
   ((and (pair? body) (eq? (car body) 'BEGIN))
    `(LET ((,var ,exp)) ,@(cdr body)))
   ((and (pair? body) (eq? (car body) 'OR) (eqv? var (cadr body)))
    `(OR ,exp ,@(cddr body)))		;unsafe: no guarantee that var does not occur in body
   (else
    `(LET ((,var ,exp)) ,body))))

(define (make-residual-let-serious var proc args body)
  (make-residual-let var (apply make-residual-call proc args)
		     body))
(define (make-residual-let-serious-apply var proc arg body)
  (make-residual-let var (make-residual-apply proc arg)
		     body))
(define make-residual-let-trivial make-residual-let)

(define (make-residual-begin exp1 exp2)
  (if (and (pair? exp1) (not (eq? (car exp1) 'QUOTE)))
      (let ((exp2-begin (and (pair? exp2) (eq? (car exp2) 'BEGIN))))
	(cond
	 ((eq? (car exp1) 'BEGIN)
	  (if exp2-begin
	      `(BEGIN ,@(cdr exp1) ,@(cdr exp2))
	      `(BEGIN ,@(cdr exp1) ,exp2)))
	 (else
	  (if exp2-begin
	      `(BEGIN ,exp1 ,@(cdr exp2))
	      `(BEGIN ,exp1 ,exp2)))))
      exp2))

(define (make-residual-cons exp1 exp2)
  (if (pair? exp2)
      (let ((tag (car exp2)))
	(cond
	 ((and (eq? tag 'QUOTE) (eq? (cadr exp2) '()))
	  `(LIST ,exp1))
	 ((eq? tag 'LIST)
	  `(LIST ,exp1 ,@(cdr exp2)))
	 (else
	  `(CONS ,exp1 ,exp2))))
      `(CONS ,exp1 ,exp2)))

(define (make-residual-generator-ve* name lv . args)
  `(,name ,lv ,@args))

(define (make-residual-generator-vve* name lv x1 . args)
  `(,name ,lv ,x1 ,@args))

(define (make-residual-generator-vve name v1 v2 e1)
  `(,name ,v1 ,v2 ,e1))

(define (make-residual-generator-vvve* name lv v1 v2 v3 . e1*)
  `(,name ,lv ,v1 ,v2 ,v3 ,@e1*))

(define (make-residual-generator-veve* name lv v1 e1 v2 . e2*)
  `(,name ,lv ,v1 ,e1 ,v2 ,@e2*))

(define (make-residual-generator-vvvve name lv v1 v2 v3 v4 e1)
  `(,name ,lv ,v1 ,v2 ,v3 ,v4 ,e1))

(define (make-residual-generator-vvee name lv v1 v2 e1 e2)
  `(,name ,lv ,v1 ,v2 ,e1 ,e2))

(define (make-residual-generator-vveqe name lv v1 v2 e1 q1 e2)
  `(,name ,lv ,v1 ,v2 ,e1 ',q1 ,e2))

(define (make-residual-generator-vqqeqe name v1 q1 q2 e1 q3 e2)
  `(,name ,v1 ',q1 ',q2 ,e1 ',q3 ,e2))

(define (make-residual-generator-vqqqeqe name lv q1 q2 q3 x4 q5 x6)
  `(,name ,lv ',q1 ',q2 ',q3 ,x4 ',q5 ,x6))



(define (make-residual-define-data lv arg)
  (add-to-support-code! `(define-data ,@arg))
  (if (zero? lv)
      'pooof				;ignored
      `(_OP ,(- lv 1) _DEFINE_DATA `,arg)))

(define (make-residual-define-mutable lv var arg)
  (add-to-support-code! `(define ,var ,arg))
  (if (zero? lv)
      'pooof				;ignored
      `(_OP ,(- lv 1) _DEFINE ,var ,arg)))

(define (make-branch branch) 
  (let ((code (cadr branch)))
    (if (and (pair? code)
	     (eq? 'BEGIN (car code)))
	(cons (car branch)
	      (cdr code))
	branch)))

(define (constant-expression? exp)
  (cond
   ((boolean? exp) #t)
   ((number? exp) #t)
   ((char? exp) #t)
   ((string? exp) #t)
   ((and (pair? exp)
	 (eq? 'QUOTE (car exp)))
    #t)
   (else #f)))

(define (constant-expression-constant exp)
  (if (pair? exp)
      (cadr exp)
      exp))
   
(define (detect-constant-test test)
  (and
   (pair? test)
   (case (car test)
     ((EQV?)
      (cond
       ((constant-expression? (cadr test))
	(cons 
	 (caddr test)
	 (list (constant-expression-constant (cadr test)))))
       ((constant-expression? (caddr test))
	(cons
	 (cadr test)
	 (list (constant-expression-constant (caddr test)))))
       (else #f)))
     ((MEMV)
      (cond
       ((constant-expression? (caddr test))
	(cons
	 (cadr test)
	 (constant-expression-constant (caddr test))))
       (else
	#f)))
     (else
      #f))))

(define (make-residual-if c t e)
  (cond
   ((eq? c #t)
    t)
   ((eq? c #f)
    e)
   ((eq? e #f)
    (if (and (pair? t) (eq? 'AND (car t)))
	`(AND ,c ,@(cdr t))
	`(AND ,c ,t)))
   ((detect-constant-test c)
    => (lambda (stuff)
	 (let ((exp (car stuff))
	       (constants (cdr stuff)))
	   (if (and (pair? e) (eq? 'CASE (car e))
		    (equal? (cadr e) exp))
	       (make-residual-case
		exp
		(cons (make-branch `(,constants ,t))
		      (cddr e)))
	       (make-residual-case
		exp
		(cons (make-branch `(,constants ,t))
		      `((else ,e))))))))
   ((eqv? c t)
    (if (and (pair? e) (eq? 'OR (car e)))
	`(OR ,c ,@(cdr e))
	`(OR ,c ,e)))
   ((and (pair? e) (eq? 'IF (car e)))
    `(COND
      ,(make-branch `(,c ,t))
      (,(cadr e) ,(caddr e))
      (else ,(cadddr e))))
   ((and (pair? e) (eq? 'COND (car e)))
     `(COND
       ,(make-branch `(,c ,t))
       ,@(cdr e)))
   (else
    `(IF ,c ,t ,e))))

;; assumption: (not (null? branches))
(define (make-residual-case exp branches)
  (let ((collapsed-branches
	 (let loop ((current-branch (car branches))
		    (branches (cdr branches))
		    (new-branches '()))
	   (if (not (null? branches))
	       (let ((new-branch (car branches)))
		 (if (and (not (eq? 'else (car new-branch)))
			  (equal? (cdr new-branch) (cdr current-branch)))
		     (loop (cons (append (car new-branch) (car current-branch))
				 (cdr current-branch))
			   (cdr branches)
			   new-branches)
		     (loop new-branch
			   (cdr branches)
			   (cons current-branch new-branches))))
	       (reverse (cons current-branch new-branches))))))
    `(CASE ,exp ,@collapsed-branches)))

(define (make-residual-call f . args)
  (cons f args))

(define (make-residual-primop op . args)
  (cons op args))

(define (make-lambda-body-list body)
  (if (and (pair? body) (eq? (car body) 'BEGIN))
      (cdr body)
      (list body)))

(define (make-residual-closed-lambda formals free body)
  `(LAMBDA ,formals ,@(make-lambda-body-list body)))

(define (make-residual-lambda formals fvs body)
  `(LAMBDA ,formals ,@(make-lambda-body-list body)))

(define (make-residual-literal val)
  (if (or (number? val) (string? val) (boolean? val))
      val
      `(QUOTE ,val)))

;; we need this abstraction because we need to actually *do* something
;; when directly generating object code
(define (make-residual-variable name)
  name)

(define (make-residual-definition! name formals body)
  (let ((new-def
	 (if (and (pair? body) (eq? (car body) 'BEGIN))
	     `(DEFINE (,name ,@formals) ,@(cdr body))
	     `(DEFINE (,name ,@formals) ,body))))
    (add-to-residual-program! new-def)))

;; kludge alert

(define (residual-definition-replace-name defn new-name)
  (let* ((old-defn-template (take 2 defn))
	 (new-defn-template
	  (list (car old-defn-template)
		(cons new-name (cdadr old-defn-template))))
	 (defn-body (list-tail defn 2)))
    (append new-defn-template defn-body)))

(define (residual-wrap-internal-definitions def internal-defs)
  (let* ((defn-template (take 2 def))
	 (defn-body (list-tail def 2)))
    (append defn-template
	    internal-defs
	    defn-body)))


	 