;;; cogen-gensym.scm

;;; copyright � 1996-2000 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact


;;; symbol generation
(define *gensym-counter* 0)
(define (gensym-reset! . rest)
  (set! *gensym-counter*
	(if (and (pair? rest) (number? (car rest)))
	    (car rest)
	    0)))
(define gensym
  (lambda (sym)
    (set! *gensym-counter* (+ *gensym-counter* 2))
    (any->symbol sym "-" *gensym-counter*)))
(define gensym-global-trimmed
  (lambda (sym)
    (gensym (trim-symbol sym))))
(define gensym-global-ignore
  (lambda (sym)
    (gensym 'f)))
(define gensym-trimmed gensym-global-trimmed)
(define *gensym-local* '())
(define gensym-local-reset!
  (lambda ()
    (set! *gensym-local* '())))
(define gensym-local-push!
  (lambda ()
    (set! *gensym-local* (cons 1 *gensym-local*))))
(define gensym-local-pop!
  (lambda ()
    (set! *gensym-local* (cdr *gensym-local*))))
(define gensym-local-hold
  (lambda ()
    *gensym-local*))
(define gensym-local-push-old!
  (lambda (old)
    (if (null? old)
	(gensym-local-push!)
	(set! *gensym-local* (cons (car old) *gensym-local*)))))
(define gensym-local-use-stub
  (lambda (sym)
    (if (null? *gensym-local*)
	(gensym sym)
	(begin
	  (set-car! *gensym-local* (+ (car *gensym-local*) 2))
	  (any->symbol sym "-" (car *gensym-local*))))))
(define gensym-local-trimmed-use-stub
  (lambda (sym)
    (gensym-local-use-stub (trim-symbol sym))))
(define gensym-local-ignore-stub
  (lambda (sym)
    (if (null? *gensym-local*)
	(gensym "x")
	(begin
	  (set-car! *gensym-local* (+ (car *gensym-local*) 2))
	  (any->symbol "x-" (car *gensym-local*))))))
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
