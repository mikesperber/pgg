;;; very simple modular interpreter
;;; requires modules in dependency order, main module first

(define-data list
  (nil) (:: hd tl))

(define-data pair
  (pair fst snd))

(define-memo _memo 1)
(define-memo _load 1 'deferred)

(define-primitive zero? - pure)	; (all t t); (all x (-> (* b x) b))
(define-primitive null? - pure)
(define-primitive + - pure)
(define-primitive - - pure)

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

(define (prepare n l)
  (if (zero? n)
      (nil)
      (:: (car l) (prepare (- n 1) (cdr l)))))

(define-without-memoization (exec mod mods instrs regs)
  (let loop ((instrs instrs) (regs regs))
    (if (null? instrs)
	(hd regs)
	(let ((instr (car instrs))
	      (instrs (cdr instrs)))
	  (case (car instr)
	    ((Incr)
	     (let* ((regno (cadr instr))
		    (xxx (split regno regs))
		    (regs1 (fst xxx))
		    (regs2 (snd xxx))
		    (regi (hd regs2)))
	       (loop instrs
		     (append regs1 (:: (+ regi 1) (tl regs2))))))
	    ((Decr)
	     (let* ((r (cadr instr))
		    (xxx (split r regs))
		    (regs1 (fst xxx))
		    (regs2 (snd xxx))
		    (regr (hd regs2)))
	       (loop instrs
		     (append regs1 (:: (- regr 1) (tl regs2))))))
	    ((Jz)
	     (let* ((r (cadr instr))
		    (label (caddr instr))
		    (xxx (split r regs))
		    (regr (hd (snd xxx))))
	       (if (zero? regr)
		   (jump mod mods label regs)
		   (loop instrs regs))))
	    ((Jump)
	     (let* ((label (cadr instr)))
	       (jump mod mods label regs)))
	    (else
	     (error "Illegal Instruction" instr)))))))

;;; jump : 0 1 0 [1]0
(define (jump mod mods lab regs)
  (let ((found (assoc lab mod)))
    (if found
	(_memo (exec mod mods (cdr found) regs))
	(_load mods
	       (lambda (next-mod)
		 (if (null? next-mod)
		     (error "Undefined label")
		     (jump next-mod mods lab regs)))))))

;;; main : 1 0 0 1
(define (main modules label nregs initial_registers)
  (let ((regs (prepare nregs initial_registers)))
    (jump '() modules label regs)))

