;;; ,open signals escapes
(newline)
(display "loading program generator generator")
(newline)
(load "cogen-record.scm")
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
;;; (load "cogen-cps.scm")		; in cps
(load "shift-reset.scm")		; necessary for direct style,
					; from s48 distribution 
(load "cogen-direct.scm")		; in direct style
;;; to generate the generating extension
(load "cogen-driver.scm") 
