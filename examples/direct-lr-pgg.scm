; Essential LR parsing in direct style
; ====================================

;;; requires direct-lr-support.scm

(deftype (_sim-error d) d)
(deftype (apply - *) *)

(defdata mylist (mynil) (mycons mycar mycdr))

;;; step 1: plain parser
;;; step 2: attribute evaluation

;;; primitives


;;; handle attribute stack
(define (empty-stack) '())
(define (push elem stack)
  (cons elem stack))
(define (top->mylist n stack)
  (cond
   ((zero? n) (mynil))
   ((= n 1) (mycons (car stack) (mynil)))
   (else (mycons (car stack) (top->mylist (- n 1) (cdr stack))))))
(define (reverse-mylist xs)
  (let loop ((xs xs) (acc (mynil)))
    (if (mynil? xs)
	acc
	(loop (mycdr xs) (mycons (mycar xs) acc)))))
(define (my-list->list l)
  (if (mynil? l)
      '()
      (cons (mycar l)
	    (my-list->list (mycdr l)))))

;;; handle input
(define (input-char p) (car p))
(define (input-attr p) (cdr p))

;;; utilities

(define (filter-2 pred? list)
  (filter-20 pred? list '()))
(define (filter-20 pred? rest result)
  (if (null? rest)
      (reverse result)
      (filter-20 pred?
		 (cdr rest)
		 (if (pred? (car rest))
		     (cons (car rest) result)
		     result))))

;; the trick need be applied to all calls of parse-bar!

(define (direct-parse grammar k first-map state
		      attribute-stack input)



  ;; body of direct-parse

  (if (final? state grammar)
      (if (equal? '$ (input-char (stream-car input)))
	  (car attribute-stack)
	  (_sim-error 'direct-parse "expecting eof"
		      (input-char (stream-car input))))
      
      (let* ((closure (compute-closure state grammar k first-map))
	     (ts (next-terminals closure grammar))
	     (ts (if (final? state grammar)
		     (filter-2 (lambda (t) (not (equal? t '$))) ts)
		     ts))
	     (accept-items (accept closure)))

;;; local definitions

  (define (select-lookahead-item item-set k input)
    (let loop ((trie (items->trie item-set k)) (pos 0) (input input))
      (if (null? trie)
	  (continue #f)
	  (if (= pos k)
	      (continue trie)
	      (let ((ch (stream-car input)))
		(let inner-loop ((trie trie))
		  (if (null? trie)
		      (continue #f)
		      (if (let loop ((la-set (caar trie)))
			    (or (equal? ch (car la-set))
				(if (null? (cdr la-set))
				    #f
				    (loop (cdr la-set)))))
			  (loop (cdar trie)
				(+ pos 1)
				(if (= (+ pos 1) k) '() (stream-cdr input)))
			  (inner-loop (cdr trie))))))))))

  (define (continue production)
    (if (not production)
	;; shift-terminal
	(let* ((p (stream-car input))
	       (ch (input-char p)))
	  (let loop ((ts ts))
	    (if (null? ts)
		(_sim-error 'direct-parse "can't shift on" ch)
		(if (equal? (car ts) ch)
		    (parse-bar grammar k first-map closure (car ts)
			       (input-attr p) attribute-stack (stream-cdr input))
		    (loop (cdr ts))))))
	(let* ((rhs-length (length (production-rhs production)))
	       (lhs (production-lhs production))
	       (attribution (production-attribution production))
	       (attribute-value (apply
				 (eval attribution (interaction-environment))
				 (my-list->list
				  (reverse-mylist
				   (top->mylist rhs-length attribute-stack))))))
	  
	  ;; reduce, then shift on lhs
	  (if (zero? rhs-length)
	      ;; shift-nonterminal
	      (parse-bar grammar k first-map closure lhs
			 attribute-value attribute-stack input)
	      (parse-result lhs rhs-length attribute-value input)))))

;;; body of let*
	
	(if (null? accept-items)
	    ;; shift-terminal
	    (let* ((p (stream-car input))
		   (ch (input-char p)))
	      (let loop ((ts ts))
		(if (null? ts)
		    (_sim-error 'direct-parse "can't shift on" ch)
		    (if (equal? (car ts) ch)
			(parse-bar grammar k first-map closure (car ts)
				   (input-attr p) attribute-stack (stream-cdr input))
			(loop (cdr ts))))))
	    (select-lookahead-item accept-items k input)))))

(define (parse-bar grammar k first-map closure sym
		   attribute-value attribute-stack input)
  (let* ((next-state (goto closure sym))
	 (result (direct-parse grammar k first-map next-state
			       (push attribute-value attribute-stack)
			       input)))
    (if (final? next-state grammar)
	result
	(let* ((the-lhs (result-lhs result))
	       (the-dot (result-dot result))
	       (the-att (result-att result))
	       (the-inp (result-inp result))
	       (nts (next-nonterminals closure grammar)))
	  (if (null? nts)
	      (parse-result the-lhs (- the-dot 1) the-att the-inp)
	      (if (< 1 the-dot)
		  (parse-result the-lhs (- the-dot 1) the-att the-inp)
	      (let loop ((nts nts))
		(if (null? (cdr nts))
		    (parse-bar grammar k first-map closure (car nts)
			       the-att attribute-stack the-inp)
		    (if (equal? (car nts) the-lhs)
			(parse-bar grammar k first-map closure (car nts)
				   the-att attribute-stack the-inp)
			(loop (cdr nts)))))))))))

(define (direct-parse-main source-grammar k input)
  (let* ((grammar (source-grammar->grammar source-grammar k))
	 (start-production (car (grammar-productions grammar))))
    (direct-parse grammar
		  k
		  (compute-first grammar k)
		  (list
		   (make-item start-production
			      0
			      (cdr (production-rhs start-production))))
		  '()
		  input)))


