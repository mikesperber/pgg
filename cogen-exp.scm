(set! *residual-program* '()) 
(set! *memolist* '()) 
(define ppp
  (bta-run (scheme->abssyn-d (file->list "examples/app.scm")) '(app s d)))
(writelpp (generate-d ppp) "gex.scm")
;;; load generating extension
(load "gex.scm")
(define level1
  ((multi-memo 2
	       '$goal
	       '(1 2)
	       (list (lambda (k) (k 'XXX)) (lambda (k) (k 'YYY))))
   id))
(writelpp *residual-program* "resid1.scm") 
(set! *residual-program* '()) 
(set! *memolist* '()) 
(load "resid1.scm")
(define level2
  ((multi-memo 1
	       (cadr (list-ref level1 2))
	       (cadr (list-ref level1 3))
	       (list (lambda (k) (k '(a b c))) (lambda (k) (k 'YYY))))
   id))
(writelpp *residual-program* "resid2.scm") 
(set! *residual-program* '()) 
(set! *memolist* '()) 
