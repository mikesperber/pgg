;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; identity
(define (id x) x)
;;; successor
(define (succ x) (+ x 1))
;;; unit of the continuation monad
(define (result-c v) (lambda (k) (k v)))
;;; symbol generation
(define gensym (let ((x 0))
		 (lambda (sym)
		   (set! x (+ x 1))
		   (string->symbol (string-append
				    (symbol->string sym)
				    "-"
				    (number->string x))))))
(define gencont (lambda () (gensym 'c)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;;; list of n results of applying thunk
(define (nlist n thunk)
  (let loop ((n n))
    (if (zero? n)
	'()
	(cons (thunk) (loop (- n 1)))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sets rep. by sets without repeated elements

(define (set-union s1 s2)
  (if (null? s1) s2
      (if (member (car s1) s2)
	  (set-union (cdr s1) s2)
	  (cons (car s1) (set-union (cdr s1) s2)))))

(define (set-subtract s e)
  (if (null? s)
      '()
      (if (equal? e (car s))
	  (cdr s)
	  (cons (car s) (set-subtract (cdr s) e)))))

(define (set-difference s1 s2)
  (if (null? s1)
      '()
      (let ((el (car s1))
	    (rest (set-difference (cdr s1) s2)))
	(if (member el s2)
	    rest
	    (cons el rest))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (filter p xs)
  (if (null? xs)
      '()
      (if (p (car xs))
	  (cons (car xs) (filter p (cdr xs)))
	  (filter p (cdr xs))))) 