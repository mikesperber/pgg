(define exported-labels '((add . mod1) (finis . mod1) (copy . mod2)))

(define module1
  '((add . ((jz 1 copy)
	    (decr 1)
	    (incr 0)
	    (jump add)))
    (finis . ())))

(define module2
  '((copy . ((jz 2 test)
	     (incr 1)
	     (decr 2)
	     (jump copy)))
    (test . ((jz 1 finis)
	     (jump add)))))

;; same example with qualifying module names
(define module_1
  '((add . ((jz 1 (mod2 . copy))
	    (decr 1)
	    (incr 0)
	    (jump (mod1 . add))))
    (finis . ())))

(define module_2
  '((copy . ((jz 2 (mod2 . test))
	     (incr 1)
	     (decr 2)
	     (jump (mod2 . copy))))
    (test . ((jz 1 (mod1 . finis))
	     (jump (mod1 . add))))))

