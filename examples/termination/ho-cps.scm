(define-data my-list (nil) (cons hd tl))

(define (run e v)
  (int e (lambda (m) v) (lambda (z) z)))
(define (int e r c)
  (cond
   ((eq? (hd e) 'CONST) (c (hd (tl e))))
   ((eq? (hd e) 'VAR) (c (r (hd (tl e)))))
   ((eq? (hd e) 'CONS) (int (hd (tl e)) r
			    (lambda (w1)
			      (int (hd (tl (tl e))) r
				   (lambda (w2)
				     (c (cons w1 w2)))))))
   ((eq? (hd e) 'IF) (int (hd (tl e)) r
			  (lambda (b)
			    (if b
				(int (hd (tl (tl e))) r c)
				(int (hd (tl (tl (tl e)))) r c)))))
   ((eq? (hd e) 'ABS) (c (lambda (x)
			   (lambda (k)
			     (int (hd (tl (tl e)))
				  (upd (hd (tl e)) x r)
				  (lambda (z) (k z)))))))
   (else (int (hd (tl e)) r
	      (lambda (w1)
		(int (hd (tl (tl e))) r
		     (lambda (w2)
		       ((w1 w2) (lambda (z) (c z))))))))))
(define (upd n v r)
  (lambda (m) (if (eq? n m) v (r m))))
