(define-macro (define-data . clauses)
  (cons 'begin
	(apply
	 append
	 (ctors-generate-define clauses))))

(define (ctors-make-test ctor)
  (string->symbol (string-append (symbol->string ctor) "?")))

(define (ctors-generate-define defconstr-clause)
  (map (lambda (ctor-decl)
	 (cons
	  `(define ,ctor-decl (list ',(car ctor-decl) ,@(cdr ctor-decl)))
	  (cons
	   `(define (,(ctors-make-test (car ctor-decl)) arg)
	      (equal? (car arg) ',(car ctor-decl)))
	   (let loop ((sels (cdr ctor-decl)) (i 1))
	     (if (null? sels)
		 '()
		 (cons
		  `(define (,(car sels) x)
		     (list-ref x ,i))
		  (loop (cdr sels) (+ i 1))))))))
       (cdr defconstr-clause)))