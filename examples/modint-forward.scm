;; interpreter for a simple language with modules
;; extended for mutually recursive modules

(load "examples/the-trick.scm")

(define-data list
  (nil) (:: hd tl))

(define-data tuple-2
  (tuple-2 fst snd))

(define-memo _memo 1)
(define-memo _next 1 'deferred)

(define-primitive eqv? - pure)
(define-primitive zero? - pure)
(define-primitive dyn-error - dynamic)

;;; auxiliary, partially static

(define (list-split i xs)
  (let loop ((i i) (rxs (nil)) (xs xs))
    (if (zero? i)
	(tuple-2 (list-reverse rxs) xs)
	(loop (- i 1) (:: (hd xs) rxs) (tl xs)))))

(define (list-reverse xs)
  (let loop ((r (nil)) (xs xs))
    (if (nil? xs)
	r
	(loop (:: (hd xs) r) (tl xs)))))

(define (list-append xs ys)
  (let loop ((xs xs))
    (if (nil? xs)
	ys
	(:: (hd xs) (loop (tl xs))))))

(define (list-length xs)
  (if (nil? xs) 0
      (+ 1 (list-length (tl xs)))))

(define (list-find k kvs)
  (if (nil? kvs)
      #f
      (let ((kv (hd kvs))
	    (kvs (tl kvs)))
	(or (equal? k (fst kv))
	    (list-find k kvs)))))

(define (list-access k kvs)		;crashes unless k in kvs!
  (let ((kv (hd kvs))
	(kvs (tl kvs)))
    (if (equal? k (fst kv))
	(snd kv)
	(list-access k kvs))))

(define (prepare n l)
  (if (zero? n)
      (nil)
      (:: (car l) (prepare (- n 1) (cdr l)))))

;; auxiliary, completely static

(define (domain mod)
  (map (lambda (x) (car x)) mod))

;; main program

(define-without-memoization (exec senv mods instrs regs)
  (let loop ((instrs instrs) (regs regs))
    (if (null? instrs)
	(hd regs)
	(let ((instr (car instrs))
	      (instrs (cdr instrs)))
	  (cond
	   ((eq? (car instr) 'Incr)
	    (let* ((regno (cadr instr))
		   (xxx (list-split regno regs))
		   (regs1 (fst xxx))
		   (regs2 (snd xxx))
		   (regi (hd regs2)))
	      (loop instrs
		    (list-append regs1 (:: (+ regi 1) (tl regs2))))))
	   ((eq? (car instr) 'Decr)
	    (let* ((r (cadr instr))
		   (xxx (list-split r regs))
		   (regs1 (fst xxx))
		   (regs2 (snd xxx))
		   (regr (hd regs2)))
	      (loop instrs
		    (list-append regs1 (:: (- regr 1) (tl regs2))))))
	   ((eq? (car instr) 'Jz)
	    (let* ((r (cadr instr))
		   (label (caddr instr))
		   (xxx (list-split r regs))
		   (regr (hd (snd xxx))))
	      (if (zer0? regr)
		  (jump-internal senv mods label regs)
		  (loop instrs regs))))
	   ((eq? (car instr) 'Jump)
	    (let* ((label (cadr instr)))
	      (jump-internal senv mods label regs)))
	   (else
	    (error "Illegal Instruction" instr)))))))

(define (memo-exec senv mods instrs regs)
  (_memo (exec senv mods instrs regs)))

;; I would prefer a recursive data definition, but PGG won't let me...
(define (make-environment mod0 mods senv)
  (:: 
   (let loop ((mod mod0))
     (if (null? mod)
	 (nil)
	 (let* ((chunk (car mod))
		(mod (cdr mod))
		(lab (car chunk))
		(instrs (cdr chunk)))
	   (:: (tuple-2 lab
			(lambda (regs)
			  (memo-exec (make-environment mod0 mods senv)
				     mods instrs regs)))
	       (loop mod)))))
   senv))

;;; alternative implementation with variables
;(define (make-environment mod0 mods senv)
;  (let* ((new-senv senv)
;	(new-entries
;	 (let loop ((mod mod0))
;	   (if (null? mod)
;	       (nil)
;	       (let* ((chunk (car mod))
;		      (mod (cdr mod))
;		      (lab (car chunk))
;		      (instrs (cdr chunk)))
;		 (:: (tuple-2 lab
;			      (lambda (regs)
;				(memo-exec new-senv
;					   mods instrs regs)))
;		     (loop mod)))))))
;    (set! new-senv (:: new-entries senv))
;    new-senv))

;; for lab : 0, this guarantees that internal jumps are dispatched at compile time
(define (jump-internal senv0 mods lab regs)
  (let loop ((env (hd senv0)) (senv (tl senv0)))
    (cond
     ((list-find lab env)
      ((list-access lab env) regs))
     ((::? senv)
      (loop (hd senv) (tl senv)))
     (else
      (_next mods
	     (if (null? mods)
		 (error (string-append "Undefined label: " (symbol->string lab)))
		 (let ((mod (car mods))
		       (mods (cdr mods)))
		   (jump-internal (make-environment mod mods senv0) mods lab regs))))))))

;; for lab : 1
;; this guarantees that all labels are compiled
;; never looks at old modules
(define (jump senv mod mods lab regs)
  (let ((senv (make-environment mod mods senv)))
    (let ((static-lab (maybe-the-member lab (domain mod))))
      (if static-lab
	  (memo-exec senv mods (cdr (assoc static-lab mod)) regs)
	  (_next mods
		 (if (null? mods)
		     (dyn-error "Undefined label")
		     (jump senv (car mods) (cdr mods) lab regs)))))))

;;; main : 0 1 0 1
(define (main mods label nregs initial_registers)
  (let ((regs (prepare nregs initial_registers)))
    (jump (nil) '() mods label regs)))

;;;(define (_next l b) b)


