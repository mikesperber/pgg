;;; cogen-boxops.scm

;;; copyright © 1996, 1997, 1998 by Peter Thiemann
;;; non-commercial use is free as long as the original copright notice
;;; remains intact

(define (make-cell v)
  (list v))
(define (cell-ref r)
  (car r))
(define (cell-set! r v)
  (set-car! r v))
