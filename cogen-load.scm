;;; ,open signals escapes
;;; you need to define ***dynamic-context-propagation*** :boolean
(newline)
(display "loading program-generator generator")
(newline)
(load "cogen-record.scm")
(load "auxiliary.scm")
(load "pp.scm")
(define p pretty-print)
(load "cogen-env.scm")
(load "cogen-abssyn.scm") 
(load "cogen-scheme.scm")
(load "cogen-oca.scm")
(load "cogen-skeleton.scm")
(load "cogen-eq-flow.scm")

;;; to run the generating extension
(load "cogen-ctors.scm")
(load "cogen-control.scm")
(load "cogen-library.scm")
(load "cogen-residual.scm")
(load "shift-reset.scm")		; from s48 distribution 
;;;(load "cogen-direct.scm")		; in direct style w/ procedures
(load "cogen-direct-syntax.scm")	; in direct style w/ macros

;;; main
(load "cogen-driver.scm") 
