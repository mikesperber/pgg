(define-data my-list (nil) (cons hd tl)) 

(define (f x y)
  (g (lambda (z) (hd x)) y))
(define (g c y)
  (c y))
(define (h x)
  (h (f x 42)))
