(define (f s d)
  (let ((x s))
    (set! x (+ x x))
    (let ((f (lambda (y)
	       (+ x y))))
      (lambda (z) (f d)))))


