(define ($goal-2 clone-2 clone-1)
  (app-1-3 clone-2 clone-1))
(define (app-1-3 clone-2 clone-1)
  (if (null? clone-2)
      clone-1
      (let ((var-3 (cdr clone-2)))
        (_op 1 'cons (_lift0 1 (car clone-2)) (app-1-3 var-3 clone-1)))))
