;;; TO BE USED WITH THE CPS VERSION OF THE COMBINATORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; specialize append
;;;
(define *residual-program* (cogen-driver '("examples/app.scm") '(app 0 1)))
(writelpp *residual-program* "examples/app-c0.scm")
;;; load generating extension
(load "examples/app-c0.scm")
(define level1 (start-memo 2 $goal '(1 2) (list  'XXX 'YYY)))
(writelpp *residual-program* "examples/app-c1.scm")
(load "examples/app-c1.scm")
(define level2 (nextlevel level1 (list  '(a b c) 'YYY)))
(writelpp *residual-program* "examples/app-c2.scm")
;;;
;;; now with reversed binding times
;;;
(define *residual-program* (cogen-driver '("examples/app.scm") '(app 1 0)))
(writelpp *residual-program* "examples/app-rc0.scm")
;;; load generating extension
(load "examples/app-rc0.scm")
(define level1 (start-memo 2 $goal '(2 1) (list  'XXX 'YYY)))
;;; now the parameters of the goal function are sorted by ascending
;;; binding time
(writelpp *residual-program* "examples/app-rc1.scm")
(load "examples/app-rc1.scm")
(define level2 (nextlevel level1 (list  '(a b c) 'XXX)))
(writelpp *residual-program* "examples/app-rc2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; specialize something with partially static stuff
;;;
(define *residual-program* (cogen-driver '("examples/ctors.scm") '(main 0 1)))
(writelpp *residual-program* "examples/ctors-c0.scm")
(load "examples/ctors-c0.scm")
(define level1 (start-memo 2 $goal '(1 2) (list 'XXX 'YYY)))
(writelpp *residual-program* "examples/ctors-c1.scm")
(load "examples/ctors-c1.scm")
(define level2 (nextlevel level1 (list '(a b c) 'YYY)))
(writelpp *residual-program* "examples/ctors-c2.scm")
(load "examples/ctors-c2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; specialize wrt to a PS function
;;;
(define *residual-program* (cogen-driver '("examples/lambda.scm") '(main 0 1)))
(writelpp *residual-program* "examples/lambda-c0.scm")
(load "examples/lambda-c0.scm")
(define level1 (start-memo 2 $goal '(1 2) (list 'XXX 'YYY)))
(writelpp *residual-program* "examples/lambda-c1.scm")
(load "examples/lambda-c1.scm")
(define level2 (nextlevel level1 (list 42 'YYY)))
(writelpp *residual-program* "examples/lambda-c2.scm")
(load "examples/lambda-c2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; specialize a continuation-based parser
;;;
(load "examples/direct-lr-support.scm")
(load "../lr-essence/examples/grammars.scm")

(define *residual-program*
  (cogen-driver '("examples/cps-lr.scm") '(do-parse 0 0 1)))
(writelpp *residual-program* "examples/cps-lr-c0.scm")
(load "examples/cps-lr-c0.scm")
(define level1 (start-memo 2 $goal '(1 1 2) (list 'grammar 'k 'input)))
(writelpp *residual-program* "examples/cps-lr-c1.scm")
(load "examples/cps-lr-c1.scm")
(define level2 (nextlevel level1 (list g10-attrib 1 'input)))
(writelpp *residual-program* "examples/cps-lr-c2.scm")
(load "examples/cps-lr-c2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; specialize a direct style parser (no support for _eval)
;;;
;(load "examples/direct-lr-support.scm")
;(load "../lr-essence/examples/grammars.scm")
;
;(define *residual-program*
;  (cogen-driver '("examples/direct-lr-pgg.scm") '(direct-parse-main 0 0 1)))
;(writelpp *residual-program* "examples/direct-lr-pgg-c0.scm")
;(load "examples/direct-lr-pgg-c0.scm")
;(define level1 (start-memo 2 $goal '(1 1 2) (list 'grammar 'k 'input)))
;(writelpp *residual-program* "examples/direct-lr-pgg-c1.scm")
;(load "examples/direct-lr-pgg-c1.scm")
;(define level2 (nextlevel level1 (list g10-attrib 1 'input)))
;(writelpp *residual-program* "examples/direct-lr-pgg-c2.scm")
;(load "examples/direct-lr-pgg-c2.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; generate a higher-order online specializer
;;;
(load "examples/scheme1-support.scm")
(define *residual-program*
  (cogen-driver '("examples/scheme1-pgg.scm") '(s1-2int-skeleton 0 0 1)))
(writelpp *residual-program* "examples/scheme1-pgg-c0.scm")
(load "examples/scheme1-pgg-c0.scm")
(define level1 (start-memo 2 $goal '(1 1 2) (list 'program 'call-pattern 'dynamic-input)))
(writelpp *residual-program* "examples/scheme1-pgg-c1.scm")
(load "examples/scheme1-pgg-c1.scm")
(define level2 (nextlevel level1 (list '((define (main x y)
					   (+ (call f x) (call f y)))
					 (define (f z)
					   (+ z 1))) '(main 5 ***) 'y)))
(writelpp *residual-program* "examples/scheme1-pgg-c2.scm")
(load "examples/scheme1-pgg-c2.scm")
