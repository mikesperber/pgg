(define (twice f x) (f (f x)))
(define (main d y) ((twice twice) (lambda (x) (+ x y)) d))
