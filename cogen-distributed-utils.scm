;;; utilities
(define (encap proc)
  (let ((aspace (local-aspace)))
    (lambda args
      (apply remote-apply aspace proc args))))

(define (with-lock lock thunk)
  (obtain-lock lock)
  (let ((value (thunk)))
    (release-lock lock)
    value))

