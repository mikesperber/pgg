(define (nextlevel level args)
  (start-memo (pred (cadr level))
	      (cadr (list-ref level 2))
	      (cadr (list-ref level 3))
	      args))
;;; specialize append
(define ppp (cogen-driver '("examples/app.scm") '(app s d)))
(writelpp ppp "examples/app-0.scm")
;;; load generating extension
(load "examples/app-0.scm")
(define level1
  (start-memo 2
	       '$goal
	       '(1 2)
	       (list (result-c 'XXX) (result-c 'YYY))))
(writelpp *residual-program* "examples/app-1.scm") 
(load "examples/app-1.scm")
(define level2
  (nextlevel level1 (list (result-c '(a b c)) (result-c 'YYY))))
(writelpp *residual-program* "examples/app-2.scm")
(load "examples/app-2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; specialize something with partially static stuff
(define ppp (cogen-driver '("examples/ctors.scm") '(main s d))) 
(writelpp ppp "examples/ctors-0.scm")
(load "examples/ctors-0.scm")
(define level1
  (start-memo 2
	       '$goal
	       '(1 2)
	       (list (result-c 'XXX) (result-c 'YYY))))
(writelpp *residual-program* "examples/ctors-1.scm") 
(load "examples/ctors-1.scm")
(define level2
  (nextlevel level1 (list (result-c '(a)) (result-c 'YYY))))
(writelpp *residual-program* "examples/ctors-2.scm") 
(load "examples/ctors-2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; an example with partially static input
(define ppp (cogen-driver '("examples/list.dat" "examples/arity.scm")
			  '(sum ((my-nil) (my-cons d *)))))
(writelpp ppp "examples/arity-0.scm")
(load "examples/arity-0.scm")
(define level1
  (start-memo 2
	      '$goal
	      '(1)
	       (list (result-c 'psl))))
(writelpp *residual-program* "examples/arity-1.scm") 
(load "examples/arity-1.scm") 
(define level2
  (nextlevel level1 (list (_ctor_memo 0 '() 'my-nil))))
(p *residual-program*)
(define level2
  (nextlevel level1
	     (list (_ctor_memo 0 '(2 1) 'my-cons (result 'xx2)
			       (_ctor_memo 0 '(2 1) 'my-cons (result
							      'xx1) 
				(_ctor_memo 0 '() 'my-nil))))))
(writelpp *residual-program* "examples/arity-2.scm") 
(load "examples/arity-2.scm") 

