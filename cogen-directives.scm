(define-syntax define-without-memoization
  (syntax-rules ()
    ((define-without-memoization bla ...)
     (define bla ...))))

(define-syntax define-memo
  (syntax-rules ()
    ((define-memo name level)
     (define (name x) x))))

(define-syntax define-primitive
  (syntax-rules ()
    ((define-primitive o t k)
     (begin
       (display "defined primitive ") (display o)
       (newline)))))
       