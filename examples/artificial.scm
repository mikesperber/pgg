;;; checks the optimized treatment of variables in lambda_memos

(define (main a x y)
  (foo a (lambda (d)
	   (if d
	       (lambda (z) (z x y))
	       (lambda () x)))))

(define (foo a success-fail)
  (if a
      ((success-fail #t) (lambda (x y) (+ x y)))
      ((success-fail #f))))
