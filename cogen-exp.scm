;;; specialize append
(define ppp (cogen-driver '("examples/app.scm") '(app s d)))
(writelpp ppp "append-gex.scm")
;;; load generating extension
(load "append-gex.scm")
(define level1
  ((start-memo 2
	       '$goal
	       '(1 2)
	       (list (result-c 'XXX) (result-c 'YYY)))
   id))
(writelpp *residual-program* "append-resid1.scm") 
(load "append-resid1.scm")
(define level2
  ((start-memo 1
	       (cadr (list-ref level1 2))
	       (cadr (list-ref level1 3))
	       (list (result-c '(a b c)) (result-c 'YYY)))
   id))
(writelpp *residual-program* "append-resid2.scm") 
;;;
;;; specialize something with partially static stuff
(define ppp (cogen-driver '("examples/ctors.scm") '(main s d))) 
(writelpp ppp "ctors-gex.scm")
(load "ctors-gex.scm")
(define level1
  ((start-memo 2
	       '$goal
	       '(1 2)
	       (list (result-c 'XXX) (result-c 'YYY)))
   id))
(writelpp *residual-program* "ctors-resid1.scm") 
(load "ctors-resid1.scm")
(define level2
  ((start-memo 1
	       (cadr (list-ref level1 2))
	       (cadr (list-ref level1 3))
	       (list (result-c '(a b c)) (result-c 'YYY)))
   id))
