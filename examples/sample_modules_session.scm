;; Sample PGG session for separate compilation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see ICFP-submitted paper

,open signals
,open pgg-residual
,open pp

;; naive approach
(define genext (cogen-driver '("modint-base.scm" "modint-standard.scm") '(main 1 0 0 1)))
(writelpp genext "regcompiler1.scm")
(load "regcompiler1.scm")
(specialize-$goal 'add 3)
(writelpp *residual-program* "example1_main.scm")
(continue 'mod1 module1)
(writelpp *residual-program* "example1_mod1.scm")
(continue 'mod2 module2)
(writelpp *residual-program* "example1_mod2.scm")
(continue 'mod1 module1)
(writelpp *residual-program* "example1_mod1_1.scm")

;; first improvement 
(define genext (cogen-driver '("modint-base.scm" "modint-mutual.scm") '(main 0 1 0 1)))
(writelpp genext "regcompiler2.scm")
(load "regcompiler2.scm")
(specialize-$goal exported-labels 3)
(writelpp *residual-program* "example2_main.scm")
(continue 'mod1 module1)
(writelpp *residual-program* "example2_mod1.scm")
(continue 'mod2 module2)
(writelpp *residual-program* "example2_mod2.scm")

;; second improvement 
(define genext (cogen-driver '("modint-base.scm" "modint-dynamic.scm") '(main 1 1 0 1)))
(writelpp genext "regcompiler3.scm")
(load "regcompiler3.scm")
(specialize-$goal 3)
(writelpp *residual-program* "example3_main.scm")
(continue 'mod1 module_1)
(writelpp *residual-program* "example3_mod1.scm")
(continue 'mod2 module_2)
(writelpp *residual-program* "example3_mod2.scm")
(continue 'end '())
(writelpp *residual-program* "example3_mod3.scm")

