;; Sample PGG session for separate compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see submitted paper

,open signals
,open pgg-residual
,open pp

(load "examples/modint-examples.scm")

;; naive approach
;(define genext (cogen-driver '("modint-base.scm" "modint.scm") '(main 1 0 0 1)))
;(writelpp genext "regcompiler1.scm")
;(load "regcompiler1.scm")
;(specialize-$goal 'add 3)
;(writelpp *residual-program* "example1_main.scm")
;(continue 'mod1 module1)
;(writelpp *residual-program* "example1_mod1.scm")
;(continue 'mod2 module2)
;(writelpp *residual-program* "example1_mod2.scm")
;(continue 'mod1 module1)
;(writelpp *residual-program* "example1_mod1_1.scm")

;; first improvement 
(define genext (cogen-driver '("examples/modint-base.scm" "examples/modint-mutual.scm") '(main 0 1 0 1)))
(writelpp genext "/tmp/regcompiler2.scm")
(load "/tmp/regcompiler2.scm")
(specialize-$goal exported-labels 3)
(writelpp *residual-program* "/tmp/example2_main.scm")
(continue 'mod1 module1)
(writelpp *residual-program* "/tmp/example2_mod1.scm")
(continue 'mod2 module2)
(writelpp *residual-program* "/tmp/example2_mod2.scm")

;; second improvement 
(define genext (cogen-driver '("examples/modint-base.scm" "examples/modint-dynamic.scm") '(main 1 1 0 1)))
(writelpp genext "/tmp/regcompiler3.scm")
(load "/tmp/regcompiler3.scm")
(specialize-$goal 3)
(writelpp *residual-program* "/tmp/example3_main.scm")
(continue 'mod1 module_1)
(writelpp *residual-program* "/tmp/example3_mod1.scm")
(continue 'mod2 module_2)
(writelpp *residual-program* "/tmp/example3_mod2.scm")
(continue 'end '())
(writelpp *residual-program* "/tmp/example3_mod3.scm")


;;; imperative versions ;;; vector support needs debugging
;; first improvement 
(define genext (cogen-driver '("examples/modint-imp-decls.scm"
			       "examples/modint-imp-base.scm"
			       "examples/modint-mutual.scm")
			     '(main 0 1 0 1)))
(writelpp genext "/tmp/impcompiler2.scm")
(load "/tmp/impcompiler2.scm")
(specialize-$goal exported-labels 3)
(writelpp *residual-program* "/tmp/example-imp2_main.scm")
(continue 'mod1 module1)
(writelpp *residual-program* "/tmp/example-imp2_mod1.scm")
(continue 'mod2 module2)
(writelpp *residual-program* "/tmp/example-imp2_mod2.scm")

;; second improvement 
(define genext (cogen-driver '("examples/modint-imp-decls.scm"
			       "examples/modint-imp-base.scm"
			       "examples/modint-dynamic.scm")
			     '(main 1 1 0 1)))
(writelpp genext "/tmp/impcompiler3.scm")
(load "/tmp/impcompiler3.scm")
(specialize-$goal 3)
(writelpp *residual-program* "/tmp/example-imp3_main.scm")
(continue 'mod1 module_1)
(writelpp *residual-program* "/tmp/example-imp3_mod1.scm")
(continue 'mod2 module_2)
(writelpp *residual-program* "/tmp/example-imp3_mod2.scm")
(continue 'end '())
(writelpp *residual-program* "/tmp/example-imp3_mod3.scm")

,exit
