;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; identity
(define (id x) x)
;;; successor
(define (succ x) (+ x 1))
(define (pred x) (- x 1))
;;; unit of the continuation monad
(define (result-c v) (lambda (k) (k v)))
;;; symbol generation
(define *gensym-counter* 0)
(define (gensym-reset!)
  (set! *gensym-counter* 0))
(define gensym (lambda (sym)
		 (set! *gensym-counter* (+ *gensym-counter* 1))
		 (string->symbol (string-append
				  (symbol->string sym)
				  "-"
				  (number->string *gensym-counter*)))))
(define *gensym-local* '())
(define gensym-local-reset! (lambda () (set! *gensym-local* '())))
(define gensym-local-push! (lambda () (set! *gensym-local* (cons 0 *gensym-local*))))
(define gensym-local-pop! (lambda () (set! *gensym-local* (cdr *gensym-local*))))
(define gensym-local (lambda (sym)
		       (set-car! *gensym-local* (+ (car *gensym-local*) 1))
		       (string->symbol (string-append
					(symbol->string sym)
					"-"
					(number->string (car *gensym-local*))))))
(define gencont (lambda () (gensym 'c)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary
(define *memolist* '())
(define *residual-program* '())
(define (add-to-memolist! item)
  (set! *memolist* (cons item *memolist*)))
(define (clear-memolist!)
  (set! *memolist* '()))
(define (add-to-residual-program! item)
  (set! *residual-program* (cons item *residual-program*)))
(define (clear-residual-program!)
  (set! *residual-program* '()))
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

(define (set-union* . sets)
  (if (null? sets)
      '()
      (set-union (car sets) (apply set-union* (cdr sets)))))

(define (set-equal? xs ys)
  (and (and-map (lambda (x) (member x ys)) xs)
       (and-map (lambda (y) (member y xs)) ys)))

(define (and-map p? xs)
  (or (null? xs)
      (and (p? (car xs))
	   (and-map p? (cdr xs)))))

(define (and-map2 p? xs ys)
  (or (null? xs)
      (and (p? (car xs) (car ys))
	   (and-map2 p? (cdr xs) (cdr ys)))))

(define (strict-and-map p? xs)
  (or (null? xs)
      (let ((h (p? (car xs)))
	    (t (strict-and-map p? (cdr xs))))
	(and h t))))

(define (strict-or-map p? xs)
  (and (pair? xs)
       (let ((h (p? (car xs)))
	     (t (strict-or-map p? (cdr xs))))
	 (or h t))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (filter p xs)
  (if (null? xs)
      '()
      (if (p (car xs))
	  (cons (car xs) (filter p (cdr xs)))
	  (filter p (cdr xs))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (any? l)
  (and (pair? l)
       (or (car l) (any? (cdr l)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (take n l)
  (let loop ((n n) (l l) (acc '()))
    (if (or (zero? n) (null? l))
	(reverse acc)
	(loop (- n 1) (cdr l) (cons (car l) acc)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I/O: read a list of Scheme objects
(define (file->list filename)
  (with-input-from-file filename
    (lambda ()
      (let loop ((obj (read)))
	(if (eof-object? obj)
	    '()
	    (cons obj (loop (read))))))))
;;; write a list of Scheme objects
(define (writelpp l filename)
  (with-output-to-file filename
    (lambda ()
      (for-each p l)))) 
