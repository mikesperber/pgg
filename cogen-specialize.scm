;;; cogen-specialize.scm

;;; copyright © 1996, 1997, 1998, 1999 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact


(define *memolist* '())
(define *residual-program* '())
(define *support-code* '())
(define *deferred-list* '())

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
(define (for-each-memolist proc)
  (let loop ((n *memolist-stages*) (prefix '()) (memolist *memolist*))
    (if (zero? n)
	(for-each (lambda (k-v)
		    (proc (append prefix (car k-v)) (cdr k-v)))
		  (cdr memolist))
	(for-each (lambda (k-m)
		    (let ((key (car k-m))
			  (rest (cdr k-m)))
		      (if (eq? (car rest) '***)
			  (proc (append prefix (list key)) rest)
			  (loop (- n 1) (append prefix (list key)) rest))))
		  (cdr memolist)))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (make-special-key key0 special)
  (let* ((bt (car special))
	 (key1 (cadr special)))
    (if (> bt 0) key0 (cons key1 key0))))
(define (clear-deferred-list!)
  (set! *deferred-list* '()))
(define (add-to-deferred-list! key0 value . special)
  (let ((key (if (null? special)
		 key0
		 (make-special-key key0 (car special)))))
    (set! *deferred-list*
	  (cons (cons key value) *deferred-list*))))
(define (lookup-deferred-list key0 special)
  (let* ((key (make-special-key key0 special)))
    (cond
     ((assoc key *deferred-list*) => cdr)
     (else #f))))
(define (for-each-deferred-list proc)
  (for-each (lambda (k-v) (proc (car k-v) (cdr k-v))) *deferred-list*))
