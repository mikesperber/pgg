(define-data my-list (nil) (cons hd tl)) 
(define-data my-nat (zero) (succ pred))

(define (kmp p d)
  (loop p d p))

(define (loop p d pp)
  (cond
   ((nil? p) #t)
   ((nil? d) #f)
   ((eq? (hd p) (hd d)) (loop (tl p) (tl d) pp))
   ((eq? p pp) (kmp p (tl d)))
   (else
    (loop1 p d pp
	   (statickmp pp (tl pp) (add (length (tl pp)) (length p)))))))

(define (loop1 p d pp np)
  (if (eq? np pp)
      (kmp pp (tl d))
      (loop np d pp)))

(define (statickmp p d n)
  (staticloop p d n p d n))
(define (staticloop p d n pp dd nn)
  (if (eq? n 0)
      (if (and (eq? nn 0)
	       (eq? (hd p) (tl d)))
	  (statickmp pp (tl dd) (sub1 nn))
	  p)
      (if (eq? (hd p) (hd d))
	  (staticloop (hd p) (hd d) (sub1 n) pp dd nn)
	  (statickmp pp (tl dd) (sub1 n)))))
(define (length xs)
  (if (nil? xs)
      zero
      (succ (length (tl xs)))))
(define (sub1 n)
  (pred n))
(define (add m n)
  (if (zero? m)
      n
      (succ (add (pred m) n))))
