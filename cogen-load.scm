(newline)
(display "loading compiler generator")
(newline)
(load "auxiliary.scm")
(load "cogen-abssyn.scm") 
(load "cogen-scheme.scm") 
(load "cogen-skeleton.scm")
(load "cogen-bta.scm") 
;;; to run the generating extension
(load "cogen-library.scm")
;;; to generate the generating extension
(load "cogen-driver.scm") 