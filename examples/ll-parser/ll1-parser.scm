;; LL (1) parser

(define-memo _memo 1)
(define-primitive eq? - pure)
(define-primitive cdr - pure)
(define-primitive memv - pure)
;; (define-primitive apply - apply)

(define (parse grammar input)

  (define-without-memoization (parse-one symbol input)
    (_memo
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
	     (let loop ((productions (filter (lambda (production)
					       (eq? symbol (production-lhs production)))
					     (grammar-productions grammar))))
	       (cond
		((null? productions)
		 (error "parse error, unexpected symbol" ch))
		((= 1 (length productions))
		 (try (car productions)))
		((memv ch (map (lambda (x) (car x))
			       (nonterminal-first symbol 1 grammar)))
		 (try (car productions)))
		(else
		 (loop (cdr productions))))))))))

  (define-without-memoization (parse-sequence symbol* input)
    (let loop ((symbol* symbol*) (val* '()) (input input))
      (if (null? symbol*)
	  (values (reverse val*) input)
	  (call-with-values
	   (lambda ()
	     (parse-one (car symbol*) input))
	   (lambda (val input)
	     (loop (cdr symbol*) (cons val val*) input))))))

  (parse-one (grammar-start grammar) input))
