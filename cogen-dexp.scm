;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "specialize append") (newline)
;;;
(define *residual-program* (cogen-driver '("examples/app.scm") '(app 0 1)))
(writelpp *residual-program* "examples/app-d0.scm")
;;; load generating extension
(load "examples/app-d0.scm")
(define level1 (start-memo 2 $goal '(1 2) (list  'XXX 'YYY)))
(writelpp *residual-program* "examples/app-d1.scm")
(load "examples/app-d1.scm")
(define level2 (nextlevel level1 (list  '(a b c) 'YYY)))
(writelpp *residual-program* "examples/app-d2.scm")
;;;
(display "now with reversed binding times") (newline)
;;;
(define *residual-program* (cogen-driver '("examples/app.scm") '(app 1 0)))
(writelpp *residual-program* "examples/app-rd0.scm")
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
(display "specialize something with partially static stuff") (newline)
;;;
(define *residual-program* (cogen-driver '("examples/ctors.scm") '(main 0 1)))
(writelpp *residual-program* "examples/ctors-d0.scm")
(load "examples/ctors-d0.scm")
(define level1 (start-memo 2 $goal '(1 2) (list 'XXX 'YYY)))
(writelpp *residual-program* "examples/ctors-d1.scm")
(load "examples/ctors-d1.scm")
(define level2 (nextlevel level1 (list '(a b c) 'YYY)))
(writelpp *residual-program* "examples/ctors-d2.scm")
(load "examples/ctors-d2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "specialize wrt to a PS function") (newline)
;;;
(define *residual-program* (cogen-driver '("examples/lambda.scm") '(main 0 1)))
(writelpp *residual-program* "examples/lambda-d0.scm")
(load "examples/lambda-d0.scm")
(define level1 (start-memo 2 $goal '(1 2) (list 'XXX 'YYY)))
(writelpp *residual-program* "examples/lambda-d1.scm")
(load "examples/lambda-d1.scm")
(define level2 (nextlevel level1 (list 42 'YYY)))
(writelpp *residual-program* "examples/lambda-d2.scm")
(load "examples/lambda-d2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "specialize a continuation-based parser") (newline)
;;;
(load "examples/direct-lr-support.scm")
(load "../lr-essence/examples/grammars.scm")

(define *residual-program*
  (cogen-driver '("examples/cps-lr.scm") '(do-parse 0 0 1)))
(writelpp *residual-program* "examples/cps-lr-d0.scm")
(load "examples/cps-lr-d0.scm")
(define level1 (start-memo 2 $goal '(1 1 2) (list 'grammar 'k 'input)))
(writelpp *residual-program* "examples/cps-lr-d1.scm")
(load "examples/cps-lr-d1.scm")
(define level2 (nextlevel level1 (list g10-attrib 1 'input)))
(writelpp *residual-program* "examples/cps-lr-d2.scm")
(load "examples/cps-lr-d2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "specialize a direct style parser") (newline)
;;;
(load "examples/direct-lr-support.scm")
(load "../lr-essence/examples/grammars.scm")

(define *residual-program*
  (cogen-driver '("examples/direct-lr-pgg.scm") '(direct-parse-main 0 0 1)))
(writelpp *residual-program* "examples/direct-lr-pgg-d0.scm")
(load "examples/direct-lr-pgg-d0.scm")
(define level1 (start-memo 2 $goal '(1 1 2) (list 'grammar 'k 'input)))
(writelpp *residual-program* "examples/direct-lr-pgg-d1.scm")
(load "examples/direct-lr-pgg-d1.scm")
(define level2 (nextlevel level1 (list g10-attrib 1 'input)))
(writelpp *residual-program* "examples/direct-lr-pgg-d2.scm")
(load "examples/direct-lr-pgg-d2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "generate a higher-order online specializer") (newline)
;;; has a binding-time problem
;;;
(load "examples/scheme1-support.scm")
(define *residual-program*
  (cogen-driver '("examples/scheme1-pgg.scm") '(s1-2int-skeleton 0 0 1)))
(writelpp *residual-program* "examples/scheme1-pgg-d0.scm")
(load "examples/scheme1-pgg-d0.scm")
(define level1 (start-memo 2 $goal '(1 1 2) (list 'program 'call-pattern 'dynamic-input)))
(writelpp *residual-program* "examples/scheme1-pgg-d1.scm")
(load "examples/scheme1-pgg-d1.scm")
(define level2 (nextlevel level1 (list '((define (main x y)
					   (+ (call f x) (call f y)))
					 (define (f z)
					   (+ z 1))) '(main 5 ***) 'y)))
(writelpp *residual-program* "examples/scheme1-pgg-d2.scm")
(load "examples/scheme1-pgg-d2.scm")
