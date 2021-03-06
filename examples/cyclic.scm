(define-data my-list (my-nil) (my-cons my-car my-cdr)) 
(define (main d)
  (let ((cycle (my-cons 1 (make-cell (my-nil)))))
    (cell-set! (my-cdr cycle) cycle)
    (zip d cycle)))
(define (zip d s)
  (if (null? d)
      '()
      (cons (cons (car d) (my-car s))
	    (zip (cdr d) (cell-ref (my-cdr s))))))

