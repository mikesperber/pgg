; Essential LR parsing in direct style
; ====================================

;;; requires direct-lr-support.scm

(deftype (_sim-error) -)

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
		      input)
  (if (final? state grammar)
      (if (equal? '$ (car input))
	  'success
	  (direct-parse-continue grammar k first-map state input))
      (direct-parse-continue grammar k first-map state input)))



(define (direct-parse-continue grammar k first-map state input)
  ;; body of direct-parse

      (let* ((closure (compute-closure state grammar k first-map))
	     (ts (next-terminals closure grammar))
	     (ts (if (final? state grammar)
		     (filter-2 (lambda (t) (not (equal? t '$))) ts)
		     ts))
	     (accept-items (accept closure)))

	(letrec
;;; local definitions

	    ((select-lookahead-item
	      (lambda (item-set k input)
		(select-lookahead-item1 item-set 0 k input)))
	     (select-lookahead-item1
	       (lambda (item-set pos k input)
		 (if (= pos k)
		     (continue (if (null? item-set) #f (car item-set)))	; RETURN
		     (if (null? item-set)
			 (continue #f)					; RETURN
			 (select-lookahead-item2 item-set pos k input
						 (car input) item-set '())))))
	     (select-lookahead-item2
	      (lambda (item-set pos k input t items used-lookaheads)
		(if (null? items)
		    (continue #f)				; RETURN
		    (let* ((item (car items))
			   (kth-lookahead (list-ref (item-lookahead item) pos)))
		      (if (member kth-lookahead used-lookaheads)
			  (select-lookahead-item2 item-set pos k input
						  t (cdr items) used-lookaheads)
			  (if (equal? t kth-lookahead)
			      (select-lookahead-item1
			       (filter-2 (lambda (item)
					   (equal?
					    (list-ref (item-lookahead item) pos)
					    kth-lookahead))
					 item-set)
			       (+ pos 1)
			       k
			       (if (= (+ 1 pos) k)
				   '() ; so as to not touch (stream-cdr input)
				   (cdr input)))
			      (select-lookahead-item2 item-set pos k input
						      t (cdr items) (cons kth-lookahead used-lookaheads))))))))

	     (continue
	      (lambda (item)
		(if (not item)
		    ;; shift-terminal
		    (let ((ch (car input)))
		      (let loop ((ts ts))
			(if (null? ts)
			    (_sim-error 'direct-parse "can't shift on" input)
			    (if (equal? (car ts) ch)
				(parse-bar grammar k first-map closure (car ts) (cdr input))
				(loop (cdr ts))))))
		    (let* ((rhs-length (length (item-rhs item)))
			   (lhs (item-lhs item)))
		      
		      ;; reduce, then shift on lhs
		      (if (zero? rhs-length)
			  ;; shift-nonterminal
			  (parse-bar grammar k first-map closure lhs input)
			  (parse-result lhs rhs-length input)))))))

;;; body of letrec
	
	(if (null? accept-items)
	    ;; shift-terminal
	    (let ((ch (car input)))
	      (let loop ((ts ts))
		(if (null? ts)
		    (_sim-error 'direct-parse "can't shift on" input)
		    (if (equal? (car ts) ch)
			(parse-bar grammar k first-map closure (car ts) (cdr input))
			(loop (cdr ts))))))
	    (select-lookahead-item accept-items k input)))))

(define (parse-bar grammar k first-map closure sym input)
  (let* ((next-state (goto closure sym))
	 (result (direct-parse grammar k first-map next-state input)))
    (if (final? next-state grammar)
	'success
	(let* ((the-lhs (result-lhs result))
	       (the-dot (result-dot result))
	       (the-inp (result-inp result))
	       (nts (next-nonterminals closure grammar)))
	  (if (null? nts)
	      (parse-result the-lhs (- the-dot 1) the-inp)
	      (if (< 1 the-dot)
		  (parse-result the-lhs (- the-dot 1) the-inp)
	      (let loop ((nts nts))
		(if (null? (cdr nts))
		    (parse-bar grammar k first-map closure (car nts) the-inp)
		    (if (equal? (car nts) the-lhs)
			(parse-bar grammar k first-map closure (car nts) the-inp)
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
		  input)))


