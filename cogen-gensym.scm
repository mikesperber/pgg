;;; copyright by Peter Thiemann, 1998

;;; symbol generation
(define *gensym-counter* 0)
(define (gensym-reset!)
  (set! *gensym-counter* 0))
(define gensym
  (lambda (sym)
    (set! *gensym-counter* (+ *gensym-counter* 1))
    (any->symbol sym "-" *gensym-counter*)))
(define gensym-global-trimmed
  (lambda (sym)
    (gensym (trim-symbol sym))))
(define gensym-global-ignore
  (lambda (sym)
    (gensym 'f)))
(define gensym-trimmed gensym-global-trimmed)
(define *gensym-local* (list 0))
(define gensym-local-reset! (lambda () (set! *gensym-local* (list 0))))
(define gensym-local-push! (lambda () (set! *gensym-local* (cons 0 *gensym-local*))))
(define gensym-local-pop! (lambda () (set! *gensym-local* (cdr *gensym-local*))))
(define gensym-local-use-stub
  (lambda (sym)
    (set-car! *gensym-local* (+ (car *gensym-local*) 1))
    (any->symbol sym "-" (car *gensym-local*))))
(define gensym-local-trimmed-use-stub
  (lambda (sym)
    (gensym-local-use-stub (trim-symbol sym))))
(define trim-symbol
  (lambda (sym)
    (let ((s (symbol->string sym)))
      (let loop ((i (- (string-length s) 1)))
	(if (< i 0)
	    sym
	    (let ((c (string-ref s i)))
	      (case (string-ref s i)
		((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\_)
		 (loop (- i 1)))
		(else
		 (substring s 0 (+ i 1))))))))))
(define gensym-local-ignore-stub
  (lambda (sym)
    (set-car! *gensym-local* (+ (car *gensym-local*) 1))
    (any->symbol "x-" (car *gensym-local*))))
(define gensym-ignore-name-stubs!
  (lambda ()
    (set! gensym-trimmed gensym-global-ignore)
    (set! gensym-local gensym-local-ignore-stub)
    (set! gensym-local-trimmed gensym-local-ignore-stub)))
(define gensym-use-name-stubs!
  (lambda ()
    (set! gensym-trimmed gensym-global-trimmed)
    (set! gensym-local gensym-local-use-stub)
    (set! gensym-local-trimmed gensym-local-trimmed-use-stub)))

(define gensym-local gensym-local-use-stub)
(define gensym-local-trimmed gensym-local-trimmed-use-stub)
