;;; functions to construct residual code

(define (make-residual-let var exp body)
  (cond
   ((and (pair? body) (memq (car body) '(LET LET*)))
    (let ((header (cadr body))
	  (bodies (cddr body)))
      `(LET* ((,var ,exp) ,@header) ,@bodies)))
   ((and (pair? body) (eq? (car body) 'BEGIN))
    `(LET ((,var ,exp)) ,@(cdr body)))
   (else
    `(LET ((,var ,exp)) ,body))))

(define (make-residual-begin exp1 exp2)
  (if (and (pair? exp2) (eq? (car exp2) 'BEGIN))
      `(BEGIN ,exp1 ,@(cdr exp2))
      `(BEGIN ,exp1 ,exp2)))
