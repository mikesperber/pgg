;;; ,open signals escapes
(newline)
(display "loading compiler generator")
(newline)
(load "auxiliary.scm")
(load "cogen-abssyn.scm") 
(load "cogen-scheme.scm") 
(load "cogen-skeleton.scm")
(load "cogen-bta.scm") 
;;; to run the generating extension
(load "cogen-ctors.scm")
(load "cogen-library.scm")
;;; (load "cogen-cps.scm")		; in cps
(load "shift-reset.scm")		; necessary for direct style,
					; from s48 distribution 
(load "cogen-direct.scm")		; in direct style
;;; to generate the generating extension
(load "cogen-driver.scm") 
