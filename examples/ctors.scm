(defdata my-list (my-nil) (my-cons my-car my-cdr)) 
(define (main ss dd)
  (my-length (list->my-list-by ss dd)))
(define (main2 d1 d2 d3)
  (my-length (my-cons d1 (my-nil))))
(define (list->my-list-by ss dd)
  (cond
   ((null? ss)
    (my-nil))
;;;   ((= 1 (length ss))
;;;    (my-cons (car dd)
;;;	     (my-nil)))
   (else
    (my-cons (car dd)
	     (list->my-list-by (cdr ss) (cdr dd))))))
(define (my-length l)
  (if (my-nil? l)
      0
      (+ 1 (my-length (my-cdr l)))))
