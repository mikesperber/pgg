;; More essential stuff for queues

(define (queue-assoc x q)
  (assoc x (queue-head q)))

(define (queue-any p q)
  (any p (queue-head q)))