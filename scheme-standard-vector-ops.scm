;;; © 1999 by Peter Thiemann

;;; vector-length, vector?, vector must be built-in
;;; they have special binding times

(define (list->vector l)
  (let* ((n (length l))
	 (v (make-vector n #f)))
    (let loop ((i 0) (l l))
      (if (< i n)
	  (begin
	    (vector-set! v i (car l))
	    (loop (+ i 1) (cdr l)))
	  v))))

(define (vector->list v)
  (let loop ((i (- (vector-length v) 1)) (l '()))
    (if (< i 0)
	l
	(loop (- i 1) (cons (vector-ref v i) l)))))

;;; also built in
;;; (define (vector-fill! v x)
;;;   (let loop ((i (- (vector-length v) 1)))
;;;     (if (>= i 0)
;;; 	(begin
;;; 	  (vector-set! v i x)
;;; 	  (loop (- i 1))))))
