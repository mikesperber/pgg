;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "specialize append") (newline)
;;;
(define *generating-extension* (cogen-driver '("examples/app.scm") '(app 0 1)))
(writelpp *generating-extension* "/tmp/app-d0.scm")
;;; load generating extension
(load "/tmp/app-d0.scm")
(define level1 (start-memo 1 $goal '(0 1) (list  '(a b c) 'YYY)))
(writelpp *residual-program* "/tmp/app-d1.scm")
(load "/tmp/app-d1.scm")
;;;
(display "now with reversed binding times") (newline)
;;;
(define *generating-extension* (cogen-driver '("examples/app.scm") '(app 1 0)))
(writelpp *generating-extension* "/tmp/app-rd0.scm")
;;; load generating extension
(load "/tmp/app-rd0.scm")
(define level1 (start-memo 1 $goal '(1 0) (list  'XXX '(a b c))))
(writelpp *residual-program* "/tmp/app-rd1.scm")
(load "/tmp/app-rd1.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "specialize something with partially static stuff") (newline)
;;;
(define *generating-extension* (cogen-driver '("examples/ctors.scm") '(main 0 1)))
(writelpp *generating-extension* "/tmp/ctors-d0.scm")
(load "/tmp/ctors-d0.scm")
(define level1 (start-memo 1 $goal '(0 1) (list '(a b c) 'YYY)))
(writelpp *residual-program* "/tmp/ctors-d1.scm")
(load "/tmp/ctors-d1.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "specialize wrt to a PS function") (newline)
;;;
(define *generating-extension* (cogen-driver '("examples/lambda.scm") '(main 0 1)))
(writelpp *generating-extension* "/tmp/lambda-d0.scm")
(load "/tmp/lambda-d0.scm")
(define level1 (start-memo 1 $goal '(0 1) (list 42 'YYY)))
(writelpp *residual-program* "/tmp/lambda-d1.scm")
(load "/tmp/lambda-d1.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "specialize a continuation-based parser") (newline)
;;;
(load "examples/direct-lr-support.scm")
(load "../lr-essence/examples/grammars.scm")

(define *generating-extension*
  (cogen-driver '("examples/cps-lr.scm") '(do-parse 0 0 1)))
(writelpp *generating-extension* "/tmp/cps-lr-d0.scm")
(load "/tmp/cps-lr-d0.scm")
(define level1 (start-memo 1 $goal '(0 0 1) (list g10-attrib 1 'input)))
(writelpp *residual-program* "/tmp/cps-lr-d1.scm")
(load "/tmp/cps-lr-d1.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "specialize a direct style parser") (newline)
;;;
(load "examples/direct-lr-support.scm")
(load "../lr-essence/examples/grammars.scm")

(define *generating-extension*
  (cogen-driver '("examples/direct-lr-pgg.scm") '(direct-parse-main 0 0 1)))
(writelpp *generating-extension* "/tmp/direct-lr-pgg-d0.scm")
(load "/tmp/direct-lr-pgg-d0.scm")
(define level1 (start-memo 1 $goal '(0 0 1) (list g10-attrib 1 'input)))
(writelpp *residual-program* "/tmp/direct-lr-pgg-d1.scm")
(load "/tmp/direct-lr-pgg-d1.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(display "generate a higher-order online specializer") (newline)
;;; has a binding-time problem
;;;
(load "examples/scheme1-support.scm")
(define *generating-extension*
  (cogen-driver '("examples/scheme1-pgg.scm") '(s1-2int-skeleton 0 0 1)))
(writelpp *generating-extension* "/tmp/scheme1-pgg-d0.scm")
(load "/tmp/scheme1-pgg-d0.scm")
(define level1 (start-memo 1 $goal '(0 0 1) (list '((define (main x y)
						      (+ (call f x) (call f y)))
						    (define (f z)
						      (+ z 1))) '(main 5 ***) 'y)))
(writelpp *residual-program* "/tmp/scheme1-pgg-d1.scm")
(load "/tmp/scheme1-pgg-d1.scm")

