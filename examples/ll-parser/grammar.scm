; Start-separated grammars

; Guarantees:

; - the grammar symbols are numbers.
; - nonterminals and terminals are consecutive, respectively
; - the nonterminals come before the terminals
; - the start production is first

(define-record-type grammar
  (nonterminals
   terminals
   number-of-nonterminals
   number-of-symbols
   error start
   productions
   productions-by-lhs
   symbol->name-procedure
   terminal-attribution)
  ((properties '())))

(define (make-grammar nonterminals terminals
		      error start
		      productions
		      symbol->name-procedure
		      terminal-attribution)
  (let* ((number-of-nonterminals (length nonterminals))
	 (number-of-terminals (length terminals))
	 (productions-by-lhs (make-vector number-of-nonterminals)))
    
    (for-each
     (lambda (lhs)
       (vector-set! productions-by-lhs lhs
		    (really-productions-with-lhs lhs productions)))
     nonterminals)
		    
    (grammar-maker nonterminals terminals
		   number-of-nonterminals
		   (+ number-of-nonterminals number-of-terminals)
		   error start
		   productions
		   productions-by-lhs
		   symbol->name-procedure
		   terminal-attribution)))

(define (really-productions-with-lhs lhs productions)
  (filter (lambda (production)
	    (equal? lhs (production-lhs production)))
	  productions))

(define (grammar-fetch-property grammar name proc)
  (cond
   ((assoc name (grammar-properties grammar)) => cdr)
   (else
    (let ((value (proc grammar)))
      (set-grammar-properties! grammar
			       (cons (cons name value)
				     (grammar-properties grammar)))
      value))))

(define (grammar-symbol->name symbol grammar)
  ((grammar-symbol->name-procedure grammar) symbol))

(define (grammar-start-production grammar)
  (car (grammar-productions-with-lhs (grammar-start grammar) grammar)))

(define (terminal? symbol grammar)
  (>= symbol (grammar-number-of-nonterminals grammar)))

(define (nonterminal? symbol grammar)
  (< symbol (grammar-number-of-nonterminals grammar)))

(define (grammar-productions-with-lhs lhs grammar)
  (vector-ref (grammar-productions-by-lhs grammar) lhs))

; Productions are specialized and show up in the specialized output.
; Hence, they need equality and an external representation.

(define (make-production lhs rhs attribution)
  (vector lhs rhs attribution))
	  
(define (production-lhs p) (vector-ref p 0))
(define (production-rhs p) (vector-ref p 1))
(define (production-attribution p) (vector-ref p 2))

(define (attribution-arglist args)
  (let loop ((i (length args)) (l '()))
    (if (zero? i)
	l
	(loop (- i 1) (cons (concatenate-symbol "$" (number->string i)) l)))))

(define-syntax define-grammar
  (syntax-rules ()
    ((define-grammar grammar-name symbol-enum nts ts s rules)
     (define-grammar grammar-name symbol-enum nts ts s rules #f))
    ((define-grammar grammar-name symbol-enum
       (nonterminals ...)
       (terminals ...)
       start-symbol
       (((lhs rhs ...) expression) ...)
       terminal-attribution)
     (begin
       (define-enumeration symbol-enum
	 ($start nonterminals ... $error terminals ...))
       (define grammar-name
	 (make-grammar (list (enum symbol-enum $start)
			     (enum symbol-enum nonterminals) ...)
		       (list (enum symbol-enum $error)
			     (enum symbol-enum terminals) ...)
		       (enum symbol-enum $error)
		       (enum symbol-enum $start)
		       (list (make-production
			      (enum symbol-enum $start)
			      (list (enum symbol-enum start-symbol))
			      '(lambda (x) x))
			     (make-production
			      (enum symbol-enum lhs)
			      (list (enum symbol-enum rhs) ...)
			      `(lambda ,(attribution-arglist '(rhs ...))
				 expression))
			     ...)
		       (lambda (symbol)
			 (enumerand->name symbol symbol-enum))
		       'terminal-attribution))))))



; nullable computation

(define (compute-nonterminal-nullable? grammar)
  
  (let ((visited-vector
	 (make-vector (grammar-number-of-nonterminals grammar) #f))
	(nullable-vector
	 (make-vector (grammar-number-of-nonterminals grammar) #f)))
    
    (define (nullable? nonterminal)
      (if (vector-ref visited-vector nonterminal)
	  (vector-ref nullable-vector nonterminal)
	  (begin
	    (vector-set! visited-vector nonterminal #t)
	    (let loop ((productions (grammar-productions-with-lhs nonterminal grammar)))
	      (if (null? productions)
		  #f
		  (let ((production (car productions)))
		    (if (every? (lambda (symbol)
				  (and (nonterminal? symbol grammar)
				       (nullable? symbol)))
				(production-rhs production))
			(begin
			  (vector-set! nullable-vector nonterminal #t)
			  #t)
			(loop (cdr productions)))))))))

    (for-each nullable? (grammar-nonterminals grammar))

    (lambda (nonterminal)
      (vector-ref nullable-vector nonterminal))))

(define (nonterminal-nullable? nonterminal grammar)
  ((grammar-fetch-property grammar 'nonterminal-nullable?
			   compute-nonterminal-nullable?)
   nonterminal))

(define (sequence-nullable? sequence grammar)
  (not (any? (lambda (symbol)
	       (or (terminal? symbol grammar)
		   (not (nonterminal-nullable? symbol grammar))))
	     sequence)))

; First set computation

(define (really-nonterminal-first nonterminal first-map)
  (vector-ref first-map nonterminal))

(define (really-sequence-first sequence k grammar nonterminal-first)
  (let loop ((sequence-rest sequence))
    
    (if (null? sequence-rest)
	'(())
	(let ((cdr-first (loop (cdr sequence-rest)))
	      (s (car sequence-rest)))
	  (if (terminal? s grammar)
	      (uniq
	       (map (lambda (f)
		      (restricted-append k (list s) f))
		    cdr-first))
	      (list-union
	       (map
		(lambda (f-cdr)
		  (map
		   (lambda (f-car)
		     (restricted-append k f-car f-cdr))
		   (nonterminal-first s k grammar)))
		cdr-first)))))))

(define (lhs-next-first lhs k grammar old-first)
  (let loop ((ps (grammar-productions-with-lhs lhs grammar))
	     (first '()))
    (if (null? ps)
	first
	(let ((rhs-first
	       (really-sequence-first
		(production-rhs (car ps)) k grammar
		(lambda (nonterminal k grammar)
		  (really-nonterminal-first nonterminal old-first)))))
	  (loop (cdr ps)
		(union first rhs-first))))))

(define (initial-first-map grammar)
  ;; each nonterminal is associated with the empty set
  (make-vector (grammar-number-of-nonterminals grammar) '()))

(define (next-first-map grammar k last-first-map)
  ;; "Gesamtschritt" step in solving the flow equation system for first_k
  (let ((new-first-map (make-vector (grammar-number-of-nonterminals grammar))))
    (for-each
     (lambda (nonterminal)
       (vector-set! new-first-map nonterminal
		    (lhs-next-first nonterminal k grammar last-first-map)))
     (grammar-nonterminals grammar))
    new-first-map))
 
(define (map-equal? fm-1 fm-2)
  (let ((size (vector-length fm-1)))
    (let loop ((i 0))
      (or (= i size)
	  (and (list-set-equal? (vector-ref fm-1 i)
				(vector-ref fm-2 i))
	       (loop (+ 1 i)))))))

(define (compute-first grammar k)
  (if (= 1 k)
      (compute-first-1 grammar)
      ;; fixpoint iteration
      (let loop ((first-map (initial-first-map grammar)))
	(let ((new-first-map (next-first-map grammar k first-map)))
	  (if (not (map-equal? first-map new-first-map))
	      (loop new-first-map)
	      (lambda (nonterminal)
		(really-nonterminal-first nonterminal first-map)))))))

(define (nonterminal-first nonterminal k grammar)
  ((grammar-fetch-property grammar (cons 'nonterminal-first k)
			   (lambda (grammar)
			     (compute-first grammar k)))
   nonterminal))

(define (sequence-first sequence k grammar)
  (really-sequence-first sequence k grammar nonterminal-first))

; first_1 computation

(define (compute-first-1 grammar)
  (let ((first-map (make-vector (grammar-number-of-nonterminals grammar) '()))
	(depths (make-vector (grammar-number-of-nonterminals grammar) 0)))
    
    (define (for-each-nonterminal f)
      (for-each f (grammar-nonterminals grammar)))

    ;; each Y with X -> \alpha Y \beta with \alpha nullable
    (define (initial-symbols lhs)
      (let production-loop ((productions (grammar-productions-with-lhs lhs grammar))
			    (symbols '()))
	(if (not (null? productions))
	    (let loop ((rhs-rest (production-rhs (car productions)))
		       (symbols symbols))
	      (if (not (null? rhs-rest))
		  (let* ((symbol (car rhs-rest))
			 (visited? (or (= lhs symbol)
				       (memv symbol symbols)))
			 (symbols (if visited?
				      symbols
				      (cons symbol symbols))))
		    (if (and (nonterminal? symbol grammar)
			     (nonterminal-nullable? symbol grammar))
			(loop (cdr rhs-rest) symbols)
			(production-loop (cdr productions) symbols)))
		  (production-loop (cdr productions) symbols)))
	    symbols)))

    (define (for-each-induction f lhs)
      (for-each f
		(filter (lambda (symbol)
			  (nonterminal? symbol grammar))
			(initial-symbols lhs))))

    (define (associate-depth! nonterminal depth)
      (vector-set! depths nonterminal depth))
    
    (define (depth-association nonterminal)
      (vector-ref depths nonterminal))

    (define (overwrite-first! nonterminal-1 nonterminal-2)
      (vector-set! first-map nonterminal-1
		   (vector-ref first-map nonterminal-2)))

    (define (merge-firsts! lhs nonterminal)
      (vector-set! first-map lhs
		   (union (vector-ref first-map lhs)
			  (vector-ref first-map nonterminal))))

    (for-each
     (lambda (nonterminal)
       (let ((initial (filter-map
		       (lambda (symbol)
			 (and (terminal? symbol grammar)
			      (cons symbol '())))
		       (initial-symbols nonterminal))))
	 (vector-set! first-map nonterminal
		      (if (nonterminal-nullable? nonterminal grammar)
			  (cons '() initial)
			  initial))))
     (grammar-nonterminals grammar))

    (complete-subsets! for-each-nonterminal
		       =
		       for-each-induction
		       associate-depth! depth-association
		       overwrite-first! merge-firsts!)

    (lambda (nonterminal)
      (vector-ref first-map nonterminal))))

; Follow set computation

(define (initial-follow-map grammar)
  (let ((follow-map (make-vector (grammar-number-of-nonterminals grammar) '())))
    ;; start symbol must be followed by the empty string to get off the
    ;; ground
    (vector-set! follow-map (grammar-start grammar) '(()))
    follow-map))

;;; perform
;;; follow (k, A) = U { first (k, beta follow (k, B)) | B -> alpha A beta }
;;; by iterating over the right sides of all productions, updating the
;;; follow-set as appropriate

(define (next-follow-map grammar k last-follow-map)
  (let ((new-follow-map (copy-vector last-follow-map)))
    (let loop ((productions (grammar-productions grammar)))
      (if (not (null? productions))
	  (let ((lhs (production-lhs (car productions))))
	    (let rhs-loop ((rhs-rest (production-rhs (car productions))))
	      (if (null? rhs-rest)
		  (loop (cdr productions))
		  (let ((sym (car rhs-rest)))
		    (if (terminal? sym grammar)
			(rhs-loop (cdr rhs-rest))
			(let* ((fi-rest (sequence-first (cdr rhs-rest) k grammar))
			       (fo-lhs (vector-ref new-follow-map lhs))
			       (fo-sym (uniq
					(pair-map (lambda (xs ys)
						    (restricted-append k xs ys))
						  fi-rest fo-lhs))))
			  (vector-set! new-follow-map sym
				       (union fo-sym 
					      (vector-ref new-follow-map sym)))
			  (rhs-loop (cdr rhs-rest))))))))))
    new-follow-map))

(define (compute-follow grammar k)
  (if (= 1 k)
      (compute-follow-1 grammar)
      ;; fixpoint iteration
      (let loop ((follow-map (initial-follow-map grammar)))
	(let ((new-follow-map (next-follow-map grammar k follow-map)))
	  (if (not (map-equal? follow-map new-follow-map))
	      (loop new-follow-map)
	      (lambda (nonterminal)
		(vector-ref follow-map nonterminal)))))))

; follow_1 computation

(define (compute-follow-1 grammar)

  (let ((follow-map (make-vector (grammar-number-of-nonterminals grammar) '()))
	(depths (make-vector (grammar-number-of-nonterminals grammar) 0)))

    (define (for-each-nonterminal f)
      (for-each f (grammar-nonterminals grammar)))

    (define (for-each-induction f nonterminal)
      (for-each
       (lambda (lhs)
	 (if (any? (lambda (production)
		     (cond
		      ((last-memv nonterminal (production-rhs production))
		       => (lambda (rhs-rest)
			    (sequence-nullable? (cdr rhs-rest) grammar)))
		      (else #f)))
		   (grammar-productions-with-lhs lhs grammar))
	     (f lhs)))
       (grammar-nonterminals grammar)))

    (define (associate-depth! nonterminal depth)
      (vector-set! depths nonterminal depth))
    
    (define (depth-association nonterminal)
      (vector-ref depths nonterminal))

    (define (overwrite-follow! nonterminal-1 nonterminal-2)
      (vector-set! follow-map nonterminal-1
		   (vector-ref follow-map nonterminal-2)))


    (define (merge-follows! lhs nonterminal)
      (vector-set! follow-map lhs
		   (union (vector-ref follow-map lhs)
			  (vector-ref follow-map nonterminal))))

    (for-each
     (lambda (nonterminal)
       (let production-loop ((productions (grammar-productions grammar))
			     (follow '()))
	 (cond 
	  ((null? productions)
	   (vector-set! follow-map nonterminal follow))
	  ((memv nonterminal (production-rhs (car productions)))
	   => (lambda (rhs-rest)
		(let rhs-loop ((rhs-rest (cdr rhs-rest))
			       (follow follow))
		  (if (null? rhs-rest)
		      (production-loop (cdr productions) follow)
		      (let* ((first (delq '()
					  (sequence-first rhs-rest 1 grammar)))
			     (follow (union first follow)))
			(cond 
			 ((and (nonterminal? (car rhs-rest) grammar)
			       (nonterminal-nullable? (car rhs-rest) grammar))
			  (rhs-loop (cdr rhs-rest) follow))
			 ((memv nonterminal rhs-rest)
			  => (lambda (rhs-rest)
			       (rhs-loop (cdr rhs-rest) follow)))
			 (else
			  (production-loop (cdr productions) follow))))))))
	  (else
	   (production-loop (cdr productions) follow)))))
     (grammar-nonterminals grammar))

    (vector-set! follow-map (grammar-start grammar)
		 (cons '() (vector-ref follow-map (grammar-start grammar))))

    (complete-subsets! for-each-nonterminal
		       =
		       for-each-induction
		       associate-depth! depth-association
		       overwrite-follow! merge-follows!)

    (lambda (nonterminal)
      (vector-ref follow-map nonterminal))))

(define (nonterminal-follow nonterminal k grammar)
  ((grammar-fetch-property grammar (cons 'nonterminal-follow k)
			   (lambda (grammar)
			     (compute-follow grammar k)))
   nonterminal))

; List utilities

(define (last-memv x l)
  (let loop ((rest l) (tail #f))
    (cond
     ((memv x rest)
      => (lambda (rest)
	   (loop (cdr rest) rest)))
     (else tail))))
      
(define (pair-map f xs ys)
  (let xs-loop ((xs xs))
    (if (null? xs)
	'()
	(let ((x (car xs)))
	  (let ys-loop ((ys ys))
	    (if (null? ys)
		(xs-loop (cdr xs))
		(let ((y (car ys)))
		  (cons (f x y) (ys-loop (cdr ys))))))))))

(define (uniq l)
  (let loop ((l l) (r '()))
    (if (null? l)
	(reverse r)
	(loop (cdr l)
	      (if (member (car l) r)
		  r
		  (cons (car l) r))))))

(define (union l1 l2)
  (append (filter (lambda (x)
		    (not (member x l2)))
		  l1)
	  l2))

(define (list-union l)
  (let loop ((l l) (r '()))
    (if (null? l)
	r
	(loop (cdr l)
	      (union (car l) r)))))

(define (take n l)
    (cond
     ((or (zero? n) (null? l))
      '())
     (else
      (cons (car l) (take (- n 1) (cdr l))))))

(define (restricted-append k l1 l2)
  (if (null? l1)
      (take k l2)
      (if (zero? k)
	  '()
	  (cons (car l1)
		(restricted-append (- k 1)
				   (cdr l1)
				   l2)))))

(define (list-set-equal? f-1 f-2)
  (and (= (length f-1) (length f-2))
       (let loop ((f-1 f-1))
	 (or (null? f-1)
	     (and (member (car f-1) f-2)
		  (loop (cdr f-1)))))))

(define (copy-vector vector)
  (let* ((length (vector-length vector))
	 (copy (make-vector length)))
    (do ((i 0 (+ 1 i)))
	((= i length))
      (vector-set! copy i (vector-ref vector i)))
    copy))
