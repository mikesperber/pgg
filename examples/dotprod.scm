(define (dotprod n v1 v2 v3)
  (let loop ((n n) (v1 v1) (v2 v2) (v3 v3))
    (if (zero? n)
	0
	(+ (* (* (car v1) (car v2)) (car v3))
	   (loop (- n 1) (cdr v1) (cdr v2) (cdr v3))))))
