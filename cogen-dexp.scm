;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; specialize append
;;;
(define *generator* (cogen-driver '("examples/app.scm") '(app 0 1)))
(writelpp *generator* "examples/app-d0.scm")
;;; load generating extension
(load "examples/app-d0.scm")
(define level1 (start-memo 2 $goal '(1 2) (list  'XXX 'YYY)))
(writelpp *residual-program* "examples/app-d1.scm")
(load "examples/app-d1.scm")
(define level2 (nextlevel level1 (list  '(a b c) 'YYY)))
(writelpp *residual-program* "examples/app-d2.scm")
;;;
;;; now with reversed binding times
;;;
(define *generator* (cogen-driver '("examples/app.scm") '(app 1 0)))
(writelpp *generator* "examples/app-rd0.scm")
;;; load generating extension
(load "examples/app-rd0.scm")
(define level1 (start-memo 2 $goal '(2 1) (list  'XXX 'YYY)))
;;; now the parameters of the goal function are sorted by ascending
;;; binding time
(writelpp *residual-program* "examples/app-rd1.scm")
(load "examples/app-rd1.scm")
(define level2 (nextlevel level1 (list  '(a b c) 'XXX)))
(writelpp *residual-program* "examples/app-rd2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; specialize something with partially static stuff
;;;
(define *generator* (cogen-driver '("examples/ctors.scm") '(main 0 1)))
(writelpp *generator* "examples/ctors-d0.scm")
(load "examples/ctors-d0.scm")
(define level1 (start-memo 2 $goal '(1 2) (list 'XXX 'YYY)))
(writelpp *residual-program* "examples/ctors-d1.scm")
(load "examples/ctors-d1.scm")
(define level2 (nextlevel level1 (list '(a b c) 'YYY)))
(writelpp *residual-program* "examples/ctors-d2.scm")
(load "examples/ctors-d2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; specialize wrt to a PS function
;;;
(define *generator* (cogen-driver '("examples/lambda.scm") '(main 0 1)))
(writelpp *generator* "examples/lambda-d0.scm")
(load "examples/lambda-d0.scm")
(define level1 (start-memo 2 $goal '(1 2) (list 'XXX 'YYY)))
(writelpp *residual-program* "examples/lambda-d1.scm")
(load "examples/lambda-d1.scm")
(define level2 (nextlevel level1 (list 42 'YYY)))
(writelpp *residual-program* "examples/lambda-d2.scm")
(load "examples/lambda-d2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; specialize a continuation-based parser
;;;
(load "examples/direct-lr-support.scm")
(define *generator* (cogen-driver '("examples/cps-lr.scm") '(do-parse 0 0 1)))
(writelpp *generator* "examples/cps-lr-d0.scm")
(load "examples/cps-lr-d0.scm")
(define level1 (start-memo 2 $goal '(1 1 2) (list 'grammar 'k 'input)))
(writelpp *residual-program* "examples/cps-lr-d1.scm")
(load "examples/cps-lr-d1.scm")
(load "../lr-essence/examples/grammars.scm")
(define level2 (nextlevel level1 (list g10-attrib 1 'input)))
(writelpp *residual-program* "examples/cps-lr-d2.scm")
(load "examples/cps-lr-d2.scm")
