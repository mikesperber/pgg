(define-data tuple3
  (object set get add))

(define (main)
  (let ((counter-class
	 (lambda ()
	   (let* ((slot 0)
		  (mset (lambda (x) (set! slot x) x))
		  (mget (lambda () slot))
		  (madd (lambda (x) (set! slot (+ slot x)) x)))
	     (object mset mget madd )))))
    (let ((cnt (counter-class)))
      ((set cnt) 21)
      ((add cnt) ((get cnt)))
      ((get cnt)))))
