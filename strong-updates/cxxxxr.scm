(define-syntax list
  (syntax-rules ()
    ((list) '())
    ((list x y ...) (cons x (list y ...)))))

(define-syntax caar
  (syntax-rules ()
    ((caar x) (car (car x)))))

(define-syntax cadr
  (syntax-rules ()
    ((caar x) (car (cdr x)))))

(define-syntax cdar
  (syntax-rules ()
    ((caar x) (cdr (car x)))))

(define-syntax cddr
  (syntax-rules ()
    ((caar x) (cdr (cdr x)))))

(define-syntax caaar
  (syntax-rules ()
    ((caar x) (car (car (car x))))))

(define-syntax caadr
  (syntax-rules ()
    ((caar x) (car (car (cdr x))))))

(define-syntax cadar
  (syntax-rules ()
    ((caar x) (car (cdr (car x))))))

(define-syntax caddr
  (syntax-rules ()
    ((caar x) (car (cdr (cdr x))))))

(define-syntax cdaar
  (syntax-rules ()
    ((caar x) (cdr (car (car x))))))

(define-syntax cdadr
  (syntax-rules ()
    ((caar x) (cdr (car (cdr x))))))

(define-syntax cddar
  (syntax-rules ()
    ((caar x) (cdr (cdr (car x))))))

(define-syntax cdddr
  (syntax-rules ()
    ((caar x) (cdr (cdr (cdr x))))))

(define-syntax caaaar
  (syntax-rules ()
    ((caar x) (car (car (car (car x)))))))

(define-syntax caaadr
  (syntax-rules ()
    ((caar x) (car (car (car (cdr x)))))))

(define-syntax caadar
  (syntax-rules ()
    ((caar x) (car (car (cdr (car x)))))))

(define-syntax caaddr
  (syntax-rules ()
    ((caar x) (car (car (cdr (cdr x)))))))

(define-syntax cadaar
  (syntax-rules ()
    ((caar x) (car (cdr (car (car x)))))))

(define-syntax cadadr
  (syntax-rules ()
    ((caar x) (car (cdr (car (cdr x)))))))

(define-syntax caddar
  (syntax-rules ()
    ((caar x) (car (cdr (cdr (car x)))))))

(define-syntax cadddr
  (syntax-rules ()
    ((caar x) (car (cdr (cdr (cdr x)))))))

(define-syntax cdaaar
  (syntax-rules ()
    ((caar x) (cdr (car (car (car x)))))))

(define-syntax cdaadr
  (syntax-rules ()
    ((caar x) (cdr (car (car (cdr x)))))))

(define-syntax cdadar
  (syntax-rules ()
    ((caar x) (cdr (car (cdr (car x)))))))

(define-syntax cdaddr
  (syntax-rules ()
    ((caar x) (cdr (car (cdr (cdr x)))))))

(define-syntax cddaar
  (syntax-rules ()
    ((caar x) (cdr (cdr (car (car x)))))))

(define-syntax cddadr
  (syntax-rules ()
    ((caar x) (cdr (cdr (car (cdr x)))))))

(define-syntax cdddar
  (syntax-rules ()
    ((caar x) (cdr (cdr (cdr (car x)))))))

(define-syntax cddddr
  (syntax-rules ()
    ((caar x) (cdr (cdr (cdr (cdr x)))))))

