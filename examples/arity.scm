(define (sum psl)
   (if (my-nil? psl)
       0
       (+ (my-car psl)
	  (sum (my-cdr psl)))))
