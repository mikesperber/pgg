(define-data my-list (nil) (cons hd tl)) 

(define (flatten e)
  (f e (lambda (z) z)))

(define (f e c)
  (if (nil? e)
      (c (nil))
      (f (tl e) (lambda (z) (c (app (hd e) z))))))

(define (app xs ys)
  (if (nil? xs)
      ys
      (cons (hd xs) (app (tl xs) ys))))
