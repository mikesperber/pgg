(define (int exp values)
  (evaluate exp '() values))

(define (constant? x)
  (or (number? x)
      (boolean? x)
      (and (pair? x)
	   (equal? 'quote (car x)))))

(define (exp->constant x)
  (if (pair? x)
      (if (equal? 'quote (car x))
	  (cadr x)
	  x)
      x))

(define (env-lookup name env-names env-values)
  (if (equal? name (car env-names))
      (car env-values)
      (env-lookup name (cdr env-names) (cdr env-values))))

(define (evaluate exp env-names env-values)
  (cond
   ((constant? exp)
    (exp->constant exp))
   ((not (pair? exp))
    (env-lookup exp env-names env-values))
   ((equal? 'if (car exp))
    (let ((condition (cadr exp))
	  (then-branch (caddr exp))
	  (else-branch (cadddr exp)))
      (if (evaluate condition env-names env-values)
	  (evaluate then-branch env-names env-values)
	  (evaluate else-branch env-names env-values))))
   ((equal? 'lambda (car exp))
    (let ((arg-name (caadr exp))
	  (body (caddr exp)))
      (lambda (x)
	(evaluate body
		  (cons arg-name env-names)
		  (cons x env-values)))))
   ((equal? 'apply (car exp))
    (let ((operator (cadr exp))
	  (operand (caddr exp)))
      ((evaluate operator env-names env-values)
       (evaluate operand env-names env-values))))
   (else
    (let ((operator (car exp))
	  (operands (cdr exp)))
      (apply-primitive operator (evaluate* operands env-names env-values))))
   ))

(define (evaluate* exps env-names env-values)
  (if (null? exps)
      '()
      (cons (evaluate (car exps) env-names env-values)
	    (evaluate* (cdr exps) env-names env-values))))

(define (apply-primitive operator operands)
  (cond
   ((equal? 'null? operator) (null? (car operands)))
   ((equal? 'cons operator) (cons (car operands)
				  (cadr operands)))
   ((equal? 'car operator) (car (car operands)))
   ((equal? 'cdr operator) (cdr (car operands)))
   ((equal? '+ operator) (+ (car operands) (cadr operands)))
   ((equal? 'equal? operator) (equal? (car operands) (cadr operands)))))
	   
       

      

