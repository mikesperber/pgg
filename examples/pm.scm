; -*- Scheme -*-

(define (match p d) ;;; Pat x Dat -> Ans
  ((match-hook p d cons) '() '()))

(define   ;;; Pat x Dat x Cont -> Cont
  (match-hook p d k)
    ((case (car p)
      ((Cst) (let ((v (cadr p))) (if (equal? v d) k fail)))
      ((Var) (let ((n (cadr p))) (assoc-c n (test-and-jump d k) (extend n d k))))
      ((Seq) (let ((p* (cdr p))) (match-seq p* d k))))))

(define   ;;; Pat* x Dat x Cont -> Cont
  (match-seq p* d k)
    ((if (null? p*)
	(if (null? d) k fail)
	(let ((p (car p*)) (p* (cdr p*)))
	  (if (pair? d)
	      (match-hook p (car d) (match-seq p* (cdr d) k))
	      fail)))))

(define  ;;; Cont = (List(Nam) x List(Val) -> Ans)
  (fail ln lv)
    (#f))

(define  ;;; Val x Cont -> Val -> Cont
  (test-and-jump d k)
    ((lambda (v)
      (if (equal? d v)
	  k
	  fail))))

(define  ;;; Nam x Val x Cont -> Cont
  (extend n d k)
    ((lambda (ln lv)
      (k (cons n ln) (cons d lv)))))

(define  ;;; Nam x (Val -> Cont) x Cont -> Cont
  (assoc-c n s f)
    ((lambda (ln lv)
      (let ((offset (index n ln)))
	(if (negative? offset)
	    (f ln lv)
	    ((s (list-ref lv offset)) ln lv))))))

(define  ;;; Nam x List(Nam) -> Nat + {-1}
  (index e l)
    (((let loop ((n 0) (l l)
		 (cond
		  ((null? l)
		   -1)
		  ((equal? e (car l))
		   n)
		  (else
		   (loop (add1 n) (cdr l)))))))))
