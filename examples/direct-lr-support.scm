;;; handle input

(define (input-char p) (car p))
(define (input-attr p) (cdr p))

;;; primitives

(define (parse-result the-lhs the-dot the-attribute the-inp)
  (vector the-lhs the-dot the-attribute the-inp))
(define (result-lhs result)
  (vector-ref result 0))
(define (result-dot result)
  (vector-ref result 1))
(define (result-att result)
  (vector-ref result 2))
(define (result-inp result)
  (vector-ref result 3))
(define (parse-result the-lhs the-dot the-attribute the-inp)
  (vector the-lhs the-dot the-attribute the-inp))
(define (result-lhs result)
  (vector-ref result 0))
(define (result-dot result)
  (vector-ref result 1))
(define (result-att result)
  (vector-ref result 2))
(define (result-inp result)
  (vector-ref result 3))

; Utilities
; =========

;;; (defprim (_sim-display-immediate arg)
;;;   (display arg))
;;; (defprim (_sim-write-immediate arg)
;;;   (write arg))
;;; (defprim (_sim-newline-immediate)
;;;   (newline))

(define (sort-generic leq? xs)
  (letrec
      ((insert
	(lambda (x xs)
	  (let loop ((xs xs))
	    (if (null? xs)
		(list x)
		(let ((y (car xs)))
		  (if (leq? x y)
		      (cons x xs)
		      (cons y (loop (cdr xs)))))))))
       (sort
	(lambda (xs)
	  (if (null? xs)
	      '()
	      (insert (car xs) (sort (cdr xs)))))))
    (sort xs)))

; Source grammars
; ===============

(define rule-production car)
(define (rule-contains-attribution? rule)
  (not (null? (cdr rule))))
(define rule-attribution cadr)

(define (make-attribution-arglist i)
  (let loop ((i i) (l '()))
    (if (zero? i)
	l
	(loop (- i 1)
	      (cons (string->symbol
		     (string-append "$" (number->string i)))
		    l)))))

(define (rule-make-attribution rule)
  (let* ((rhs-length (length (cdr (rule-production rule))))
	 (arglist (make-attribution-arglist rhs-length)))
    `(lambda ,arglist
       ,(if (rule-contains-attribution? rule)
	    (rule-attribution rule)
	    '$1))))

(define (make-$ n)
  (if (zero? n)
      '()
      (cons '$ (make-$ (- n 1)))))

(define (source-grammar->grammar grammar k)
  (let ((nonterminals (car grammar))
	(terminals (list-ref grammar 1))
	(rules (list-ref grammar 2))
	(start (list-ref grammar 3)))

    (let ((productions
	   (let loop ((i 0)
		      (rules (cons `((yummy-yummy ,start ,@(make-$ k)))
				   rules)))
	     (if (null? rules)
		 '()
		 (let* ((rule (car rules))
			(production (rule-production rule))
			(lhs (car production))
			(rhs (cdr production))
			(attribution (rule-make-attribution rule)))
		   (cons (vector i lhs rhs attribution)
			 (loop (+ i 1) (cdr rules))))))))

      (vector nonterminals terminals productions start))))

; Utilities
; =========

(define (filter pred? list)
  (let loop ((rest list) (result '()))
    (if (null? rest)
	(reverse result)
	(loop (cdr rest)
	      (if (pred? (car rest))
		  (cons (car rest) result)
		  result)))))

(define (or-map pred? list)
  (or (null? list)
      (pred? (car list))
      (or-map pred? (cdr list))))

(define (partition-list pred l)
  (let loop ((l l) (yes '()) (no '()))
    (cond ((null? l)
           (cons (reverse yes) (reverse no)))
          ((pred (car l))
           (loop (cdr l) (cons (car l) yes) no))
          (else
           (loop (cdr l) yes (cons (car l) no))))))
;;; this should be the real thing
;;; trie of depth k: k nested alists
;;; needs at least one item
(define (build-trie items)
  (let* ((item (car items))
	 (items (cdr items))
	 (trie (let loop ((la (item-lookahead item)))
		 (if (null? la)
		     (item-production item)
		     (list (cons (car la) (loop (cdr la))))))))
    (let loop ((items items) (trie trie))
      (if (null? items)
	  trie
	  (loop (cdr items)
		(let loop ((la (item-lookahead (car items))) (trie trie))
		  (if (null? la)
		      (item-production (car items))
		      (let ((car-la (car la)))
			(let inner-loop ((trie trie))
			  (if (null? trie)
			      (list (cons car-la (loop (cdr la) '())))
			    (if (equal? car-la (caar trie))
				(cons (cons car-la (loop (cdr la) (cadr trie)))
				      (cdr trie))
				(cons (car trie)
				      (inner-loop (cdr trie))))))))))))))

(define (sort-trie trie k)
  (let ((subject
	 (if (> k 1)
	     (map (lambda (p) (cons (car p) (sort-trie (cdr p) (- k 1)))) trie)
	     trie)))
    (sort-generic (lambda (p q)
		    (string<=? (symbol->string (car p))
			       (symbol->string (car q)))) subject)))

(define (collapse-trie trie k)
  (let ((subject
	 (if (> k 1)
	     (map (lambda (p) (cons (car p) (collapse-trie (cdr p) (- k 1)))) trie)
	     trie)))
    (let loop ((first (car subject)) (rest (cdr subject)))
      (let* ((first-la (car first))
	     (first-trie (cdr first))
	     (yes/no (partition-list (lambda (p) (equal? first-trie (cdr p))) rest))
	     (yes (car yes/no))
	     (no (cdr yes/no)))
	(cons (cons (cons first-la (map car yes)) first-trie)
	      (if (null? no)
		  '()
		  (loop (car no) (cdr no))))))))

(define (items->trie item-set k)
  (collapse-trie (sort-trie (build-trie item-set) k) k))


; Grammars
; ========

(define (grammar-nonterminals grammar)
  (vector-ref grammar 0))

(define (grammar-terminals grammar)
  (vector-ref grammar 1))

(define (grammar-productions grammar)
  (vector-ref grammar 2))

(define (grammar-start grammar)
  (vector-ref grammar 3))

(define (production-index p)
  (vector-ref p 0))

(define (production-lhs p)
  (vector-ref p 1))

(define (production-rhs p)
  (vector-ref p 2))

(define (production-attribution p)
  (vector-ref p 3))

(define (apply-attribution attribution arglist)
  (apply (eval attribution (interaction-environment))
	 arglist))

(define (productions-with-lhs lhs grammar)
  (filter (lambda (production)
	    (equal? lhs (production-lhs production)))
	  (grammar-productions grammar)))

; ---

(define (terminal? sym grammar)
  (or (eq? '$ sym)
      (eq? '@error@ sym)
      (member sym (grammar-terminals grammar))))

(define (nonterminal? sym grammar)
  (not (terminal? sym grammar)))

; ---

(define (symbols->string p)
  (let loop ((s "") (l p))
    (if (null? l)
	s
	(loop (string-append
	       (string-append s " ")
	       (symbol->string (car l)))
	      (cdr l)))))

(define (production<? p1 p2)
  (string<? (symbols->string (cons (production-lhs p1)
				   (production-rhs p1)))
	    (symbols->string (cons (production-lhs p2)
				   (production-rhs p2)))))

; LR items
; ========

(define (make-item prod pos la)
  (vector prod pos la))
(define (item-production item)
  (vector-ref item 0))
(define (item-position item)
  (vector-ref item 1))
(define (item-lookahead item)
  (vector-ref item 2))

(define (item-lhs item)
  (production-lhs (item-production item)))
(define (item-rhs item)
  (production-rhs (item-production item)))

(define (item-rhs-rest item)
  (list-tail (production-rhs (item-production item))
	     (item-position item)))

(define (item-shift item)
  (make-item (item-production item)
	     (+ 1 (item-position item))
	     (item-lookahead item)))

(define (item<? item-1 item-2)
  (or (production<? (item-production item-1)
		    (item-production item-2))
      (< (item-position item-1)
	 (item-position item-2))
      (string<? (symbols->string (item-lookahead item-1))
		(symbols->string (item-lookahead item-2)))))

(define (partition-items item items)
  (let ((production (item-production item))
	 (position (item-position item)))
  (partition-list
   (lambda (item)
     (and (equal? production (item-production item))
	  (equal? position (item-position item))))
   items)))

(define (insert-item item-set item)
  (if (null? item-set)
      (list item)
      (if (item<? item (car item-set))
	  (cons item item-set)
	  (cons (car item-set)
		(insert-item (cdr item-set) item)))))
		  
(define (sort-items item-set)
  (let loop ((sorted '())
	     (item-set item-set))
    (if (null? item-set)
	sorted
	(loop (insert-item sorted (car item-set))
	      (cdr item-set)))))

(define (items-merge is-1 is-2)
  (cond ((null? is-1) is-2)
	((member (car is-1) is-2)
	 (items-merge (cdr is-1) is-2))
	(else
	 (items-merge (cdr is-1) (cons (car is-1) is-2)))))

(define (compute-closure state grammar k first-map)

  (let ((foobar 'dummy)) ; AKA Similix sucks

    (define (initial-items symbol lookahead-suffix)

      (apply
       append
       (map (lambda (production)
	      (map
	       (lambda (la)
		 (make-item production 0 la))
	       (sf-first lookahead-suffix k grammar first-map)))
	    (productions-with-lhs symbol grammar))))

    (define (next-predict item-set)
      (let loop ((item-set item-set) (predict-set item-set))
	(if (null? item-set)
	    predict-set
	    (let* ((item (car item-set))
		   (rhs-rest (item-rhs-rest item)))
	      (if (null? rhs-rest)
		  (loop (cdr item-set) predict-set)
		  (let ((lhs (car rhs-rest)))
		    (if (terminal? lhs grammar)
			(loop (cdr item-set) predict-set)
			(let ((new-items
			       (initial-items
				lhs
				(append-k k (cdr rhs-rest) (item-lookahead item)))))
			  (loop (cdr item-set)
				(items-merge new-items predict-set))))))))))

    (define (predict-equal? is-1 is-2)
      (and (= (length is-1) (length is-2))
	   (let loop ((is-1 is-1))
	     (or (null? is-1)
		 (and (member (car is-1) is-2)
		      (loop (cdr is-1)))))))

    (let loop ((predict-set state))
      (let ((new-predict-set (next-predict predict-set)))
	(if (predict-equal? predict-set new-predict-set)
	    predict-set
	    (loop new-predict-set))))))


(define (goto state-closure symbol)
  (sort-items
   (map item-shift
	(filter (lambda (item)
		  (and (not (null? (item-rhs-rest item)))
		       (equal? symbol
			       (car (item-rhs-rest item)))))
		state-closure))))

(define (active state)
  (let loop ((item-set state)
	     (m 0))
    (if (null? item-set)
	m
	(loop (cdr item-set)
	      (max (item-position (car item-set)) m)))))

(define (next-symbols state-closure grammar)
  (let loop ((item-set state-closure)
	     (symbols '()))
    (if (null? item-set)
	symbols
	(let* ((item (car item-set))
	       (rhs-rest (item-rhs-rest item)))
	  (loop (cdr item-set)
		(if (and (not (null? rhs-rest))
			 (not (member (car rhs-rest) symbols)))
		    (cons (car rhs-rest) symbols)
		    symbols))))))

(define (next-terminals state-closure grammar)
  (filter (lambda (symbol)
	    (and (not (eq? '@error@ symbol))
		 (terminal? symbol grammar)))
	  (next-symbols state-closure grammar)))

(define (next-nonterminals state-closure grammar)
  (filter (lambda (symbol)
	    (nonterminal? symbol grammar))
	  (next-symbols state-closure grammar)))

(define (handles-error? state-closure grammar)
  (or-map (lambda (symbol)
	    (eq? '@error@ symbol))
	  (next-symbols state-closure grammar)))

(define (accept state-closure)
  (filter (lambda (item)
	    (null? (item-rhs-rest item)))
	  state-closure))

(define (items-lookaheads accept-items)
  (map item-lookahead accept-items))

(define (final? state grammar)
  (let loop ((item-set state))
    (if (null? item-set)
	#f
	(let ((item (car item-set)))
	  (or (and (equal? (item-lhs item)
			   'yummy-yummy)
		   (= 1 (item-position item)))
	      (loop (cdr item-set)))))))

; First set computation
; =====================

(define (restrict-k k l)
  (if (or (null? l)
	  (zero? k))
      '()
      (cons (car l) (restrict-k (- k 1) (cdr l)))))

(define (append-k k l1 l2)
  (if (null? l1)
      (restrict-k k l2)
      (if (zero? k)
	  '()
	  (cons (car l1)
		(append-k (- k 1)
			  (cdr l1)
			  l2)))))

(define (sf-union f-1 f-2)
  (let loop ((f f-1) (r f-2))
    (if (null? f)
	r
	(loop (cdr f)
	      (if (member (car f) r)
		  r
		  (cons (car f) r))))))

(define (sf-list-union l)
  (let loop ((l l) (r '()))
    (if (null? l)
	r
	(loop (cdr l)
	      (sf-union (car l) r)))))

(define (sf-list-uniq l)
  (let loop ((l l) (r '()))
    (if (null? l)
	r
	(loop (cdr l)
	      (if (member (car l) r)
		  r
		  (cons (car l) r))))))
	    
(define (sf-first rhs k grammar first-map)
  (let loop ((rhs-rest rhs))
    
    (if (null? rhs-rest)
	'(())
	(let ((cdr-first (loop (cdr rhs-rest)))
	      (s (car rhs-rest)))
	  (if (terminal? s grammar)
	      (sf-list-uniq
	       (map (lambda (f)
		      (append-k k (list s) f))
		    cdr-first))
	      (sf-list-union
	       (map
		(lambda (f-cdr)
		  (map
		   (lambda (f-car)
		     (append-k k f-car f-cdr))
		   (cdr (assoc s first-map))))
		cdr-first)))))))

(define (lhs-next-first lhs k grammar old-first)
  (let loop ((ps (productions-with-lhs lhs grammar))
	     (first '()))
    (if (null? ps)
	first
	(let ((rhs-first (sf-first (production-rhs (car ps)) k grammar old-first)))
	  (loop (cdr ps)
		(sf-union first rhs-first))))))

(define (initial-first-map grammar)
  ;; each nonterminal is associated with the empty set
  (map (lambda (nt)
	 (cons nt '()))
       (grammar-nonterminals grammar)))

(define (next-first-map grammar k last-first-map)
  ;; "Gesamtschritt" step in solving the flow equation system for first_k
  (map
   (lambda (first-map-entry)
     (let ((nonterm (car first-map-entry)))
       (cons nonterm
	     (lhs-next-first nonterm k grammar last-first-map))))
   last-first-map))
 
(define (first-equal? f-1 f-2)
  (and (= (length f-1) (length f-2))
       (let loop ((f-1 f-1))
	 (or (null? f-1)
	     (and (member (car f-1) f-2)
		  (loop (cdr f-1)))))))
	     
(define (first-map-equal? fm-1 fm-2)
  (let loop ((fm-1 fm-1))
    (or (null? fm-1)
	(let* ((nonterm-1 (caar fm-1))
	       (first-1 (cdar fm-1))
	       (first-2 (cdr (assoc nonterm-1 fm-2))))
	  (and (first-equal? first-1 first-2)
	       (loop (cdr fm-1)))))))


(define (compute-first grammar k)
  ;; fixpoint iteration
  (let loop ((first-map (initial-first-map grammar)))
    (let ((new-first-map (next-first-map grammar k first-map)))
      (if (first-map-equal? first-map new-first-map)
	  first-map
	  (loop new-first-map)))))

