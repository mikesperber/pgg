(define-data maybe
  (just one)
  (nothing))
(define-data term
  (make-var ref)			; make-var : ref (maybe term) -> term
  (make-cst num)
  (make-bin term1 term2)
  (make-dyn dynterm))
(define-data pair
  (xcons xcar xcdr)
  (xnil))

(define (unify-driver s t)

  (let ((ref-s->ref-d (xnil))
	(FAIL #f)
	(SUCCESS #t))
;; unify returns #f on failure and #t on success

    (define (unify s t)
      (cond
       ((make-var? s)
	(let* ((ref-maybe-s (ref s))
	       (maybe-s (cell-ref ref-maybe-s)))
	  (if (just? maybe-s)
	      (unify (one maybe-s) t)
	      (begin
		(cell-set! ref-maybe-s (just (make-dyn t)))
		SUCCESS))))
       ((make-cst? s)
	(cond ((make-var? t)
	       (let* ((ref-maybe-t (ref t))
		      (maybe-t (cell-ref ref-maybe-t)))
		 (if (just? maybe-t)
		     (unify s (one maybe-t))
		     (begin
		       (cell-set! ref-maybe-t (just (make-cst (num s))))
		       SUCCESS))))
	      ((make-cst? t)
	       (= (num s) (num t)))
	      (else
	       FAIL)))
       ((make-bin? s)
	(cond ((make-var? t)
	       (let* ((ref-maybe-t (ref t))
		      (maybe-t (cell-ref ref-maybe-t)))
		 (if (just? maybe-t)
		     (unify s (one maybe-t))
		     (begin
		       (cell-set! ref-maybe-t (just (make-bin (coerce (term1 s))
							      (coerce (term2 s)))))
		       SUCCESS))))
	      ((make-bin? t)
	       (and (unify (term1 s) (term1 t))
		    (unify (term2 s) (term2 t))))
	      (else
	       FAIL)))
       ((make-dyn? s)
	(dynamic-unify (dynterm s) t))
       (else
	FAIL)))

    (define (coerce s)			;respects sharing
      (let s->d ((s s))
	(cond
	 ((make-cst? s)
	  (make-cst (num s)))
	 ((make-bin? s)
	  (make-bin (s->d (term1 s)) (s->d (term2 s))))
	 ((make-var? s)
	  (let* ((ref-maybe-s (ref s))
		 (seen-before (xassq-d ref-maybe-s ref-s->ref-d)))
	    (if (just? seen-before)
		(make-var (one seen-before))
		(let* ((maybe-s (cell-ref ref-maybe-s))
		       (ref-maybe-t
			(if (just? maybe-s)
			    (make-cell (just (s->d (one maybe-s))))
			    (make-cell (nothing)))))
		  (set! ref-s->ref-d (xcons (xcons ref-maybe-s ref-maybe-t) ref-s->ref-d))
		  (make-var ref-maybe-t)))))
	 ((make-dyn? s)
	  (dynterm s)))))
  
    ;; main program

    (unify s t)))

(define (xassq-d x l)
  (if (xnil? l)
      (nothing)
      (let* ((p (xcar l))
	     (y (xcar p)))
	(if (cell-eq? x y)
	    (just (xcdr p))
	    (xassq-d x (xcdr l))))))

;; input syntax
;; term ::= (var <number>) | (cst <constant>) | (bin <term> <term>)

(define (parse-term s)
  (letrec ((xlookup
	    (lambda (key keys refs)
	      (if (null? keys)
		  (nothing)
		  (if (eq? key (car keys))
		      (just (xcar refs))
		      (xlookup key (cdr keys) (xcdr refs)))))))
    (let loop ((s s) (keys '()) (refs (xnil)) (c (lambda (z keys refs) z)))
      (cond
       ((eq? (car s) 'VAR)
	(let* ((n (cadr s))
	       (maybe-ref (xlookup n keys refs)))
	  (if (just? maybe-ref)
	      (c (make-var (one maybe-ref)) keys refs)
	      (let ((newref (make-cell (nothing))))
		(c (make-var newref) (cons n keys) (xcons newref refs))))))
       ((eq? (car s) 'CST)
	(c (make-cst (cadr s)) keys refs))
       (else
	;; assuming (eq? (car s) 'BIN)
	(let ((s1 (cadr s))
	      (s2 (caddr s)))
	  (loop s1 keys refs
		(lambda (ss1 keys refs)
		  (loop s2 keys refs
			(lambda (ss2 keys refs)
			  (c (make-bin ss1 ss2) keys refs)))))))))))

(define (main s t)
  (let* ((ss (parse-term s)))
    (unify-driver ss t)))
