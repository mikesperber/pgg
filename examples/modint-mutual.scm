;;; an improved modular interpreter
;;; recursive modules

;;; (load "modint-base.scm")

;;; jump-global : 0 0 0 [1]0
(define (jump-global exported-labels mod name args)
  (cond
   ((assoc name exported-labels)
    => (lambda (name-mod)
	 (_load (cdr name-mod)
		(lambda (mod-name this-mod)
		  (let ((found (assoc name this-mod)))
		    (exec (lambda (name args)
			    (jump-global exported-labels this-mod name args))
			  (cdr found)
			  args))))))
   ((assoc name mod)
    => (lambda (found)
	 (_memo (exec (lambda (name args)
			(jump-global exported-labels mod name args))
		      (cdr found)
		      args))))
   (else
    (error "Undefined label"))))

;;; jump-initial: 0 1 [1]0
(define-without-memoization (jump-initial exported-labels label args)
  (let loop ((exports exported-labels))
    (if (null? exports)
	(dyn-error "Unknown label")
	(let ((export (car exports)))
	  (if (eqv? label (car export))
	      (jump-global exported-labels '() (car export) args)
	      (loop (cdr exports)))))))

;;; main : 0 1 0 1
(define (main exported-labels label nregs initial_registers)
  (let ((regs (copy nregs initial_registers)))
    (jump-initial exported-labels label regs)))

