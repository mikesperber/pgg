;;; ,open signals escapes
(newline)
(display "loading program-generator generator")
(newline)
(load "cogen-record.scm")
(load "pp.scm")
(define p pretty-print)
(load "auxiliary.scm")
(load "cogen-env.scm")
(load "cogen-abssyn.scm") 
(load "cogen-scheme.scm")
(load "cogen-oca.scm")
(load "cogen-skeleton.scm")
(load "cogen-eq-flow.scm")

;;; to run the generating extension
(load "cogen-ctors.scm")
(load "cogen-library.scm")
(load "cogen-residual.scm")
(load "shift-reset.scm")		; from s48 distribution 
;;;(load "cogen-direct.scm")		; in direct style w/ procedures
(load "cogen-direct-syntax.scm")	; in direct style w/ macros

;;; main
(load "cogen-driver.scm") 
