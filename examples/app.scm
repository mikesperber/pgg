(define (app x y)
  (if (null? x)
      y
      (cons (car x) (app (cdr x) y)))) 

(define (rapp y x)
  (app x y))
