;;; symbol generation
(define gensym (lambda (sym) sym))
(define gensym-local (lambda (sym) sym))
(define gencont (lambda () (gensym 'c)))
