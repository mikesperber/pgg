; -*- Scheme -*-
; from:
; Olivier Danvy, Semantics-Directed Compilation of Nonlinear Patterns, 
; Information Processing Letters 6(37):315-322, 1991

(define (match p d) ;;; Pat x Dat -> Ans
  ((match-hook p d (lambda (h t) (cons h t))) '() '()))

(define (match-hook p d k)   ;;; Pat x Dat x Cont -> Cont
  (case (car p)
    ((Cst) (let ((v (cadr p))) (if (equal? v d) k fail)))
    ((Var) (let ((n (cadr p))) (assoc-c n (test-and-jump d k) (extend n d k))))
    ((Seq) (let ((p* (cdr p))) (match-seq p* d k)))))

(define (match-seq p* d k)   ;;; Pat* x Dat x Cont -> Cont
  (if (null? p*)
      (if (null? d) k fail)
      (let ((p (car p*)) (p* (cdr p*)))
	(if (pair? d)
	    (match-hook p (car d) (match-seq p* (cdr d) k))
	    fail))))

(define (fail ln lv)  ;;; Cont = (List(Nam) x List(Val) -> Ans)
  #f)

(define (test-and-jump d k)  ;;; Val x Cont -> Val -> Cont
  (lambda (v)
    (if (equal? d v)
	k
	fail)))

(define (extend n d k)  ;;; Nam x Val x Cont -> Cont
  (lambda (ln lv)
    (k (cons n ln) (cons d lv))))

(define (assoc-c n s f)  ;;; Nam x (Val -> Cont) x Cont -> Cont
  (lambda (ln lv)
    (let ((offset (index n ln)))
      (if (negative? offset)
	  (f ln lv)
	  ((s (list-ref lv offset)) ln lv)))))

(define (index e l)  ;;; Nam x List(Nam) -> Nat + {-1}
  (let loop ((n 0) (l l))
    (cond
     ((null? l)
      -1)
     ((equal? e (car l))
      n)
     (else
      (loop (+ 1 n) (cdr l))))))
