(define (nextlevel level args)
  (start-memo (cadr level)
	      (cadr (caddr level))
	      (cadr (cadddr level))
	      args))
;;;
;;; specialize append
;;;
(define ppp (cogen-driver '("examples/app.scm") '(app s d)))
(writelpp ppp "examples/app-d0.scm")
;;; load generating extension
(load "examples/app-d0.scm")
(define level1
  (start-memo 2
	      '$goal
	      '(1 2)
	      (list  'XXX 'YYY)))
(writelpp *residual-program* "examples/app-d1.scm") 
(load "examples/app-d1.scm")
(define level2 (nextlevel level1 (list  '(a b c) 'YYY)))
(writelpp *residual-program* "examples/app-d2.scm") 
;;;
;;; specialize something with partially static stuff
;;;
(define ppp (cogen-driver '("examples/ctors.scm") '(main s d))) 
(writelpp ppp "examples/ctors-d0.scm")
(load "examples/ctors-d0.scm")
(define level1
  (start-memo 2
	      '$goal
	      '(1 2)
	      (list 'XXX 'YYY)))
(writelpp *residual-program* "examples/ctors-d1.scm") 
(load "examples/ctors-d1.scm")
(define level2 (nextlevel level1 (list '(a b c) 'YYY)))
(writelpp *residual-program* "examples/ctors-d2.scm") 
(load "examples/ctors-d2.scm")
;;;
;;; specialize wrt to a PS function
;;;
(define ppp (cogen-driver '("examples/lambda.scm") '(main s d))) 
(writelpp ppp "examples/lambda-d0.scm")
(load "examples/lambda-d0.scm")
(define level1
  (start-memo 2
	      '$goal
	      '(1 2)
	      (list 'XXX 'YYY)))
(writelpp *residual-program* "examples/lambda-d1.scm") 
(load "examples/lambda-d1.scm")
(define level2 (nextlevel level1 (list 42 'YYY)))
(writelpp *residual-program* "examples/lambda-d2.scm") 
(load "examples/lambda-d2.scm")
