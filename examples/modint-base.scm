(define-data list
  (nil) (:: hd tl))

(define-data my-pair
  (my-pair fst snd))

(define-memo _memo 1)
(define-memo _load 1 'deferred)

(define-primitive zero? - pure)	; (all t t); (all x (-> (* b x) b))
(define-primitive null? - pure)
(define-primitive eq? - pure)
(define-primitive eqv? - pure)
(define-primitive + - pure)
(define-primitive - - pure)

(define-primitive dyn-error (-> b b) dynamic)

;;; auxiliary

(define (split i xs)
  (let loop ((i i) (rxs (nil)) (xs xs))
    (if (zero? i)
	(my-pair (reverse rxs) xs)
	(loop (- i 1) (:: (hd xs) rxs) (tl xs)))))

(define (reverse xs)
  (let loop ((r (nil)) (xs xs))
    (if (nil? xs)
	r
	(loop (:: (hd xs) r) (tl xs)))))

(define (append xs ys)
  (let loop ((xs xs))
    (if (nil? xs)
	ys
	(:: (hd xs) (loop (tl xs))))))

(define (copy n l)
  (if (zero? n)
      (nil)
      (:: (car l) (copy (- n 1) (cdr l)))))

(define-without-memoization (exec jump instrs regs)
  (let loop ((instrs instrs) (regs regs))
    (if (null? instrs)
	(hd regs)
	(let ((instr (car instrs))
	      (instrs (cdr instrs)))
	  (case (car instr)
	    ((Incr)
	     (let* ((regno (cadr instr))
		    (xxx (split regno regs))
		    (reg1 (fst xxx))
		    (regsi (snd xxx))
		    (reg2 (hd regsi)))
	       (loop instrs
		     (append reg1 (:: (+ reg2 1) (tl regsi))))))
	    ((Decr)
	     (let* ((regno (cadr instr))
		    (xxx (split regno regs))
		    (reg1 (fst xxx))
		    (regsi (snd xxx))
		    (reg2 (hd regsi)))
	       (loop instrs
		     (append reg1 (:: (- reg2 1) (tl regsi))))))
	    ((Jz)
	     (let* ((regno (cadr instr))
		    (label (caddr instr))
		    (xxx (split regno regs))
		    (reg (hd (snd xxx))))
	       (if (zero? reg)
		   (jump label regs)
		   (loop instrs regs))))
	    ((Jump)
	     (let* ((label (cadr instr)))
	       (jump label regs)))
	    (else
	     (error "Illegal Instruction" instr)))))))