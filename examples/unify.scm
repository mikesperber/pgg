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

  (let ((ref-s->ref-d (xnil)))

    (define (unify s t)
      (cond
       ((make-var? s)
	(let ((ref-maybe-s (ref s)))
	  (if (just? (cell-ref ref-maybe-s))
	      (unify (one (cell-ref ref-maybe-s)) t)
	      (begin
		(cell-set! ref-maybe-s (just (make-dyn t)))
		'SUCCESS))))
       ((make-cst? s)
	(cond ((make-var? t)
	       (let ((ref-maybe-t (ref t)))
		 (if (just? (cell-ref ref-maybe-t))
		     (unify s (one (cell-ref ref-maybe-t)))
		     (begin
		       (cell-set! ref-maybe-t (just (make-cst (num s))))
		       'SUCCESS))))
	      ((make-cst? t)
	       (if (= (num s) (num t))
		   'SUCCESS
		   'FAIL))
	      (else
	       'FAIL)))
       ((make-bin? s)
	(cond ((make-var? t)
	       (let ((ref-maybe-t (ref t)))
		 (if (just? (cell-ref ref-maybe-t))
		     (unify s (one (cell-ref ref-maybe-t)))
		     (begin
		       (cell-set! ref-maybe-t (just (make-bin (coerce (term1 s))
							      (coerce (term2 s)))))
		       'SUCCESS))))
	      ((make-bin? t)
	       (let ((r1 (unify (term1 s) (term1 t)))
		     (r2 (unify (term2 s) (term2 t))))
		 (if (or (equal? r1 'FAIL) (equal? r2 'FAIL))
		     'FAIL
		     'SUCCESS)))
	      (else
	       'FAIL)))
       ((make-dyn? s)
	(dynamic-unify (dynterm s) t))
       (else
	'FAIL)))

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
		(let ((ref-maybe-t
		       (if (just? (cell-ref ref-maybe-s))
			   (make-cell (just (s->d (one (cell-ref ref-maybe-s)))))
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
	(if (eq? x y)
	    (just (xcdr p))
	    (xassq-d x (xcdr l))))))

(define (main t)
  (let* ((x (make-cell (nothing)))
	 (s (make-bin (make-var x) (make-var x))))
  (unify-driver s t)))
