(define-data counter (counter set get add))

(define (counter-class)
  (let* ((slot 0)
	 (mset (lambda (x) (set! slot x)))
	 (mget (lambda () slot))
	 (madd (lambda (x) (set! slot (+ slot x)))))
    (counter mset mget madd)))

(define (main s)
  (let ((cnt (counter-class)))
    ((set cnt) s)
    ((add cnt) ((get cnt)))
    ((get cnt))))
