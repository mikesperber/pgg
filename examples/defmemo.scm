(defmemo _sim-memoize)
(define (app x y)
  (_sim-memoize 1
   (if (null? x)
       y
       (cons (car x) (app (cdr x) y)))))
