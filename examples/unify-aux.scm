;; input syntax
;; term ::= (var <number>) | (cst <constant>) | (bin <term> <term>)

(define (dynamic-parse-term s)
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

(define (points-to rf term)
  (if (make-var? term)
      (let ((rft (ref term)))
	(or (eq? rf rft)
	    (let ((maybe-term (cell-ref rft)))
	      (and (just? maybe-term)
		   (points-to rf (one maybe-term))))))
      #f))

(define (dynamic-unify s t)
  (let ((FAIL #f)
	(SUCCESS #t))
    (cond
     ((make-var? s)
      (let* ((ref-maybe-s (ref s))
	     (maybe-s (cell-ref ref-maybe-s)))
	(if (just? maybe-s)
	    (dynamic-unify (one maybe-s) t)
	    (begin
	      (if (not (points-to ref-maybe-s t))
		  (cell-set! ref-maybe-s (just t)))
	      SUCCESS))))
     ((make-cst? s)
      (cond ((make-var? t)
	     (let* ((ref-maybe-t (ref t))
		    (maybe-t (cell-ref ref-maybe-t)))
	       (if (just? maybe-t)
		   (dynamic-unify s (one maybe-t))
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
		   (dynamic-unify s (one maybe-t))
		   (begin
		     (cell-set! ref-maybe-t (just (make-bin (term1 s) (term2 s))))
		     SUCCESS))))
	    ((make-bin? t)
	     (and (dynamic-unify (term1 s) (term1 t))
		  (dynamic-unify (term2 s) (term2 t))))
	    (else
	     FAIL)))
     (else
      FAIL))))
