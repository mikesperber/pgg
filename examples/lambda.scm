(define (twice f x) (f (f x)))
(define (main d) (twice (lambda (x) (+ x x)) d))
