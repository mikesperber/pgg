;; LL (1) parser

(define-memo _memo 1)
;; (define-primitive apply - apply)

(define (parse grammar input)
  (let ((first-map (compute-first grammar 1)))

    (define-without-memoization (parse-one symbol input)
      (let ((ch (car input)))
	(if (terminal? symbol grammar)
	    (if (eq? (grammar-symbol->name symbol grammar) ch)
		(values ch (cdr input))
		(error "parse error, expected: " symbol " got: " ch))
	    (let ((try
		   (lambda (production)
		     (call-with-values
		      (lambda ()
			(parse-sequence (production-rhs production) input))
		      (lambda (val* input)
			(values (apply (eval (production-attribution production)
					     (interaction-environment))
				       val*)
				input))))))
	      (let loop ((productions (grammar-productions-with-lhs symbol grammar)))
		(cond
		 ((null? productions)
		  (error "parse error, unexpected symbol" ch))
		 ((= 1 (length productions))
		  (try (car productions)))
		 ((memq ch (first-map symbol))
		  (try (car productions)))
		 (else
		  (loop (cdr productions)))))))))

    (define-without-memoization (parse-sequence symbol* input)
      (_memo
       (let loop ((symbol* symbol*) (val* '()) (input input))
	 (if (null? symbol*)
	     (values (reverse val*) input)
	     (call-with-values
	      (lambda ()
		(parse-one (car symbol*) input))
	      (lambda (val input)
		(loop (cdr symbol*) (cons val val*) input)))))))

    (parse-one (grammar-start grammar) input)))
