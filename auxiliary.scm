;;; copyright by Peter Thiemann 1998

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; identity
(define (id x) x)			;big-util: identity
;;; successor
(define (succ x) (+ x 1))
(define (pred x) (- x 1))
;;; unit of the continuation monad
(define (result-c v) (lambda (k) (k v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define any->symbol			;big-util: concatenate-symbol
  (lambda args
    (string->symbol
     (apply string-append
	    (map (lambda (arg)
		   (cond
		    ((symbol? arg) (symbol->string arg))
		    ((string? arg) arg)
		    ((number? arg) (number->string arg))))
		 args)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; auxiliary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list of n results of applying thunk

(define (nlist n thunk)
  (let loop ((n n))
    (if (zero? n)
	'()
	(cons (thunk) (loop (- n 1)))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sets rep. by sets without repeated elements; compared w/ eq?

(define (set-include s e)
  (if (null? s)
      (list e)
      (if (memq e s)
	  s
	  (cons e s))))

(define (set-union s1 s2)
  (if (null? s1) s2
      (if (memq (car s1) s2)
	  (set-union (cdr s1) s2)
	  (cons (car s1) (set-union (cdr s1) s2)))))

(define (set-intersection s1 s2)
  (if (null? s1)
      '()
      (if (memq (car s1) s2)
	  (cons (car s1) (set-intersection (cdr s1) s2))
	  (set-intersection (cdr s1) s2))))

(define (set-subtract s e)
  (if (null? s)
      '()
      (if (eq? e (car s))
	  (cdr s)
	  (cons (car s) (set-subtract (cdr s) e)))))

(define (set-difference s1 s2)
  (if (null? s1)
      '()
      (let ((el (car s1))
	    (rest (set-difference (cdr s1) s2)))
	(if (memq el s2)
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

(define (or-map p? xs)
  (and (pair? xs)
       (or (p? (car xs))
	   (or-map p? (cdr xs)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (generic-sort leq x*)
  (let loop ((x* x*) (result '()))
    (if (null? x*)
	result
	(loop (cdr x*) (generic-insert leq (car x*) result)))))

(define (generic-insert leq x x*)
  (let loop ((x* x*))
    (if (null? x*)
	(list x)
	(let ((x1 (car x*)))
	  (if (leq x x1)
	      (cons x x*)
	      (cons x1 (loop (cdr x*))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (filter p xs)			;big-util
  (if (null? xs)
      '()
      (if (p (car xs))
	  (cons (car xs) (filter p (cdr xs)))
	  (filter p (cdr xs))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (remove-duplicates l)		;big-util
  (let loop ((l l) (u '()))
    (if (null? l)
	u
	(let ((e (car l)))
	  (if (member e u)
	      (loop (cdr l) u)
	      (loop (cdr l) (cons e u)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (countq e l)
  (let loop ((l l) (r 0))
    (if (pair? l)
	(if (eq? (car l) e)
	    (loop (cdr l) (+ r 1))
	    (loop (cdr l) r))
	r)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (list-or l)
  (and (pair? l)
       (or (car l) (list-or (cdr l)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (take n l)
  (let loop ((n n) (l l) (acc '()))
    (if (or (zero? n) (null? l))
	(reverse acc)
	(loop (- n 1) (cdr l) (cons (car l) acc)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (thread-map f in xs)
  (if (null? xs)
      in
      (let ((out (f (car xs) in)))
	(thread-map f out (cdr xs)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax load-program
  (syntax-rules ()
    ((_ prg) (eval `(BEGIN ,@prg) (interaction-environment)))))
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

(define (writel l filename)
  (with-output-to-file filename
    (lambda ()
      (for-each write l)))) 

(define (count-cells x)
  (if (pair? x)
      (+ 1 (count-cells (car x)) (count-cells (cdr x)))
      1))
;;;
(define (display-line . objs)
  (for-each display objs)
  (newline))
(define display-return
  (lambda (x) (display-line "returning " x) x))
(define display-list
  (lambda (l) (for-each (lambda (x) (display x) (display " ")) l)))
(define (spaces n)
  (make-string n #\space))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (strip-path-prefix path)
  (let ((l (string-length path)))
    (let loop ((s 0) (i 0))
      (cond
       ((= i l)
	(substring path s l))
       ((eq? (string-ref path i) #\/)
	(loop (+ i 1) (+ i 1)))
       (else
	(loop s (+ i 1)))))))

(define (strip-path-suffix path)
  (let ((l (string-length path)))
    (let loop ((e l) (i 0))
      (cond
       ((= i l)
	(substring path 0 e))
       ((eq? (string-ref path i) #\.)
	(loop i (+ i 1)))
       ((eq? (string-ref path i) #\/)
	(loop l (+ i 1)))
       (else
	(loop e (+ i 1)))))))
