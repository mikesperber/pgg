;; More essential stuff for queues

(define (queue-assoc x q)
  (assoc x (queue-head q)))

(define (queue-any p q)
  (any p (queue-head q)))

(define (dequeue-first! q pred)		;return and remove first entry
  (let ((list (queue-head q)))		;satisfying pred, adapted from BIG
    (cond ((null? list)
	   #f)
	  ((pred (car list))
	   (set-queue-head! q (cdr list))
           (if (null? (cdr list))
               (set-queue-tail! q '()))	; don't retain pointers
	   (car list))
	  ((null? (cdr list))
	   #f)
	  (else
	   (let loop ((list list))
	     (let ((tail (cdr list)))
	       (cond ((null? tail)
		      #f)
		     ((pred (car tail))
		      (set-cdr! list (cdr tail))
		      (if (null? (cdr tail))
			  (set-queue-tail! q list))
		      (car tail))
		     (else
		      (loop tail)))))))))