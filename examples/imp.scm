;;(define (f s d)
;;  (let ((x s))
;;    (set! x (+ x x))
;;    (let ((f (lambda (y)
;;	       (+ x y))))
;;      (lambda (z) (f d)))))
(define-data tuple3
  (object set get add))

(define (g s d)
  (let ((class
	 (lambda ()
	   (let ((x 0))
	     (let ((mset (lambda (y) (set! x y)))
		   (madd (lambda (y) (set! x (+ x y))))
		   (mget (lambda () x)))
	       (let ((obj (object mset mget madd)))
		 obj))))))
    
    (let ((obj (class)))
      ((set obj) 21)
      ((add obj) (get obj))
      ((get obj)))))
