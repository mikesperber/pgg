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
   ((and (pair? body) (memq (car body) '(LET LET*)))
    (let ((header (cadr body))
	  (bodies (cddr body)))
      `(LET* ((,var ,exp) ,@header) ,@bodies)))
   ((and (pair? body) (eq? (car body) 'BEGIN))
    `(LET ((,var ,exp)) ,@(cdr body)))
   (else
    `(LET ((,var ,exp)) ,body))))

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

(define (make-residual-define-data lv arg)
  (let ((real-arg
	 (let loop ((arg arg))
	   (cond
	    ((and (pair? arg) (eq? (car arg) 'QUOTE))
	     (cadr arg))
	    ((and (pair? arg) (eq? (car arg) '_LIFT0))
	     (loop (caddr arg)))
	    ((and (pair? arg) (eq? (car arg) '_LIFT))
	     (loop (cadddr arg)))
	    (else
	     arg)))))
    (add-to-support-code! `(define-data ,@real-arg))
    (if (= lv 0)
	'pooof				;ignored
	`(_OP ,(- lv 1) _DEFINE_DATA ,arg))))

(define (make-residual-if c t e)
  (cond
   ((eq? c #t)
    t)
   ((eq? c #f)
    e)
   (else
    `(IF ,c ,t ,e))))
