;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; a simple implementation of labset, should be replaced by a bitset impl 
;;; (define (labset-first? e l))
(define (set-labset-size! n)
  (set! *labset-size* n)
  (set! empty-labset (new-labset)))
(define *labset-size* 'undefined-labset-size)
(define (new-labset) (make-string *labset-size* #\0))

(define empty-labset 'undefined-labset-size)

(define (labset-elem? e l)
  (eq? (string-ref l e) #\1))
(define (labset-singleton e)
  (let ((l (new-labset)))
    (string-set! l e #\1)
    l))
(define (labset-intersection l1 l2)
  (let ((result (new-labset)))
    (let loop ((i 0))
      (if (= *labset-size* i)
	  result
	  (begin
	    (if (and (eq? (string-ref l1 i) #\1)
		     (eq? (string-ref l2 i) #\1))
		(string-set! result i #\1))
	    (loop (+ i 1)))))))
(define (labset-empty? l)
  (let loop ((i 0))
    (if (= *labset-size* i)
	#f
	(if (eq? (string-ref l i) #\1)
	    #f
	    (loop (+ i 1))))))
(define (labset-remove e l)
  (let ((result (new-labset)))
    (let loop ((i 0))
      (if (= *labset-size* i)
	  result
	  (begin
	    (if (not (= i e))
		(string-set! result i (string-ref l i)))
	    (loop (+ i 1)))))))
(define (labset-add e l)
  (let ((result (new-labset)))
    (let loop ((i 0))
      (if (= *labset-size* i)
	  result
	  (begin
	    (if (= i e)
		(string-set! result i #\1)
		(string-set! result i (string-ref l i)))
	    (loop (+ i 1)))))))
(define (labset-union l1 l2)
  (let ((result (new-labset)))
    (let loop ((i 0))
      (if (= *labset-size* i)
	  result
	  (begin
	    (if (or (eq? (string-ref l1 i) #\1)
		    (eq? (string-ref l2 i) #\1))
		(string-set! result i #\1))
	    (loop (+ i 1)))))))
(define (labset-union* ll)
  (if (null? ll)
      empty-labset
      (let loop ((ll (cdr ll)) (result (car ll)))
	(if (null? ll)
	    result
	    (loop (cdr ll) (labset-union result (car ll)))))))
(define (labset-subtract l1 l2)
  (let ((result (new-labset)))
    (let loop ((i 0))
      (if (= *labset-size* i)
	  result
	  (begin
	    (if (and (eq? (string-ref l1 i) #\1)
		     (not (eq? (string-ref l2 i) #\1)))
		(string-set! result i #\1))
	    (loop (+ i 1)))))))
(define (labset-subset? l1 l2)
  (let loop ((i 0))
    (if (= *labset-size* i)
	#t
	(if (and (eq? (string-ref l1 i) #\1)
		 (not (eq? (string-ref l2 i) #\1)))
	    #f
	    (loop (+ i 1))))))

(define (labset-equal? l1 l2)
  (and (labset-subset? l1 l2)
       (labset-subset? l2 l1)))

(define (labset-for-each proc labset)
  (let loop ((i 0))
    (if (< *labset-size* i)
	(begin
	  (if (eq? (string-ref labset i) #\1)
	      (proc i))
	  (loop (+ i 1))))))

