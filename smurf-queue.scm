;; More essential stuff for queues

(define (queue-assoc x q)
  (assoc x (queue-head q)))

(define (queue-any p q)
  (any p (queue-head q)))

(define (dequeue-first! p q)
  (let loop ((pair (queue-head q))
	     (last-pair '()))
    (cond
     ((null? pair) #f)
     ((p (car pair))
      (let ((value (car pair))
	    (next  (cdr pair)))
	(if (pair? last-pair)
	    (set-cdr! last-pair next)
	    (set-queue-head! q next))
	(if (null? next)
	    (set-queue-tail! q last-pair))
	value))
     (else
      (loop (cdr pair) pair)))))
