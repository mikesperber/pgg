;;; cogen-specialize.scm

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact


(define *memolist* '())
(define *residual-program* '())
(define *support-code* '())

;;(define (add-to-memolist! key value)
;;  (set! *memolist* (cons (cons key value) *memolist*)))
;;(define (clear-memolist!)
;;  (set! *memolist* '()))
;;(define (lookup-memolist key)
;;  (cond ((assoc key *memolist*) => cdr)
;;	(else #f)))
;; there is a problem if one key is a proper prefix of the other
;; within the number of stages
(define (clear-memolist!)
  (set! *memolist* (list '*memolist*)))
(define (add-to-memolist! key value)
  (let loop ((key key) (n *memolist-stages*) (memolist *memolist*))
    (if (zero? n)
	(let ((saved-cdr (cdr memolist)))
	  (set-cdr! memolist (cons (cons key value) saved-cdr)))
	(cond ((assoc (car key) (cdr memolist))
	       => (lambda (entry)
		    (loop (cdr key) (- n 1) (cdr entry))))
	      ((null? (cdr key))
	       (let ((saved-cdr (cdr memolist)))
		 (set-cdr! memolist
			   (cons (cons (car key) value) saved-cdr))))
	      (else
	       (let ((saved-cdr (cdr memolist))
		     (nested-memolist (list '***)))
		 (set-cdr! memolist
			   (cons (cons (car key) nested-memolist) saved-cdr))
		 (loop (cdr key) (- n 1) nested-memolist)))))))
(define (lookup-memolist key)
  (let loop ((key key) (n *memolist-stages*) (memolist (cdr *memolist*)))
    (if (zero? n)
	(cond ((assoc key memolist) => cdr)
	      (else #f))
	(cond ((assoc (car key) memolist)
	       => (lambda (entry)
		    (let ((cdr-key (cdr key)))
		      (if (null? cdr-key)
			  memolist
			  (loop cdr-key (- n 1) (cddr entry))))))
	      (else #f)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (set-residual-program! prg)
  (set! *residual-program* prg))
(define (add-to-residual-program! item)
  (set! *residual-program* (cons item *residual-program*)))
(define (clear-residual-program!)
  (set! *residual-program* '()))

(define (add-to-support-code! item)
  (set! *support-code* (cons item *support-code*)))
(define (clear-support-code!)
  (set! *support-code* '()))
(define *gen-address-counter* 0)
(define (gen-address-reset!)
  (set! *gen-address-counter* 0))
(define (gen-address label)
  (set! *gen-address-counter* (+ *gen-address-counter* 1))
  (cons label *gen-address-counter*))
