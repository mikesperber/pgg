(define-data list
  (nil) (:: hd tl))

(define-data pair
  (pair fst snd))

(define-memo _memo 1)
(define-memo _next 1 'deferred)

(define-primitive zero? - pure)

;;; auxiliary

(define (split i xs)
  (let loop ((i i) (rxs (nil)) (xs xs))
    (if (zero? i)
	(pair (reverse rxs) xs)
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

(define (length xs)
  (if (nil? xs) 0
      (+ 1 (length (tl xs)))))

(define (prepare n l)
  (if (zero? n)
      (nil)
      (:: (car l) (prepare (- n 1) (cdr l)))))

(define-without-memoization (exec mod mods instrs regs)
  (display-line (length regs))
  (let loop ((instrs instrs) (regs regs))
    (if (null? instrs)
	(hd regs)
	(let ((instr (car instrs))
	      (instrs (cdr instrs)))
	  (cond
	   ((eq? (car instr) 'Incr)
	    (let* ((regno (cadr instr))
		   (xxx (split regno regs))
		   (regs1 (fst xxx))
		   (regs2 (snd xxx))
		   (regi (hd regs2)))
	      (loop instrs
		    (append regs1 (:: (+ regi 1) (tl regs2))))))
	   ((eq? (car instr) 'Decr)
	    (let* ((r (cadr instr))
		   (xxx (split r regs))
		   (regs1 (fst xxx))
		   (regs2 (snd xxx))
		   (regr (hd regs2)))
	      (loop instrs
		    (append regs1 (:: (- regr 1) (tl regs2))))))
	   ((eq? (car instr) 'Jz)
	    (let* ((r (cadr instr))
		   (label (caddr instr))
		   (xxx (split r regs))
		   (regr (hd (snd xxx))))
	      (if (zer0? regr)
		  (jump mod mods label regs)
		  (loop instrs regs))))
	   ((eq? (car instr) 'Jump)
	    (let* ((label (cadr instr)))
	      (jump mod mods label regs)))
	   (else
	    (error "Illegal Instruction" instr)))))))

(define (jump mod mods lab regs)
  (let ((found (assoc lab mod)))
    (if found
	(_memo (exec mod mods (cdr found) regs))
	(_next mods
	       (if (null? mods)
		   (error "Undefined label")
		   (jump (car mods) (cdr mods) lab regs))))))

;;; main : 0 0 0 1
(define (main modules label nregs initial_registers)
  (let ((regs (prepare nregs initial_registers)))
    (jump '() modules label regs)))

;;;(define (_next l b) b)


