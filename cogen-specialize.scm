;;; cogen-specialize.scm

;;; copyright © 1996, 1997, 1998, 1999 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact


(define *memolist* #f)
(define *residual-program* (make-cell '()))
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

(define make-memo-table (make-table-maker equal? equal?-hash))

(define (clear-memolist!)
  (set! *memolist* (make-memo-table)))

(define (add-to-memolist! key value)
  (table-set! *memolist* key value))

(define (lookup-memolist key)
  (table-ref *memolist* key))

(define (for-each-memolist proc)
  (table-walk proc *memolist*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (get-residual-program)
  (cell-ref *residual-program*))
(define (set-residual-program! prg)
  (cell-set! *residual-program* prg))
(define (add-to-residual-program! item)
  (atomically!
   (provisional-cell-set! *residual-program*
			  (cons item (provisional-cell-ref *residual-program*)))))
(define (first-residual-procedure)
  (if (pair? (get-residual-program))
      (car (get-residual-program))))
(define (rest-residual-procedures)
  (if (pair? (get-residual-program))
      (cdr (get-residual-program))))
(define (clear-residual-program!)
  (cell-set! *residual-program* '()))

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
