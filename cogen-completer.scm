(define-syntax _complete
  (syntax-rules ()
    ((_ body)
     (let ((var (gensym-local 'mlet)))
       (shift k (make-residual-let-trivial var body (k var)))))))

(define-syntax _complete-serious
  (syntax-rules ()
    ((_ body)
     (let ((var (gensym-local 'mlet)))
       (shift k (make-residual-let-serious var body (k var)))))))

