(define (power x n)
  (if (= 0 n)
      1
      (* x (power x (- n 1)))))
