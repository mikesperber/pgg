(define exported-labels '((add . mod1) (finis . mod2)))

(define module1
  '((add . ((jz 1 finis)
	    (decr 1)
	    (incr 0)
	    (jump add)))))

(define module2
  '((finis . ())))