(define module1
  '((add . ((jz 1 finis)
	    (decr 1)
	    (incr 0)
	    (jump add)))))

(define module2
  '((finis . ())))