;;; cogen-labset-bylist.scm

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a simple implementation of labset, should be replaced by a bitset impl 
;;; (define (labset-first? e l))
(define (set-labset-size! n)
  'nothing-to-do)
(define empty-labset '())
(define (labset-elem? e l)
  (member e l))
(define (labset-singleton e)
  (list e))
(define (labset-intersection l1 l2)
  (let loop ((l1 l1) (result '()))
    (if (null? l1)
	result
	(let ((e (car l1)))
	  (if (labset-elem? e l2)
	      (loop (cdr l1) (cons e result))
	      (loop (cdr l1) result))))))
(define (labset-empty? l)
  (null? l))
(define (labset-remove e l)
  (let loop ((l l) (result '()))
    (if (null? l)
	result
	(let ((ein (car l)))
	  (if (equal? e ein)
	      (append (cdr l) result)
	      (loop (cdr l) (cons ein result)))))))
(define (labset-add e l)
  (if (labset-elem? e l)
      l
      (cons e l)))
(define (labset-union l1 l2)
  (let loop ((l1 l1) (result l2))
    (if (null? l1)
	result
	(let ((e (car l1)))
	  (if (labset-elem? e l2)
	      (loop (cdr l1) result)
	      (loop (cdr l1) (cons e result)))))))
(define (labset-union* ll)
  (if (null? ll)
      empty-labset
      (let loop ((ll (cdr ll)) (result (car ll)))
	(if (null? ll)
	    result
	    (loop (cdr ll) (labset-union result (car ll)))))))
(define (labset-subtract l1 l2)
  (let loop ((l1 l1) (result '()))
    (if (null? l1)
	result
	(let ((e (car l1)))
	  (if (labset-elem? e l2)
	      (loop (cdr l1) result)
	      (loop (cdr l1) (cons e result)))))))
(define (labset-subset? l1 l2)
  (let loop ((l1 l1))
    (or (null? l1)
	(let ((e (car l1)))
	  (and (labset-elem? e l2)
	      (loop (cdr l1)))))))
(define (labset-equal? l1 l2)
  (and (labset-subset? l1 l2)
       (labset-subset? l2 l1)))
(define (labset-for-each proc labset)
  (for-each proc labset))

(define (labset->list l)
  l)
