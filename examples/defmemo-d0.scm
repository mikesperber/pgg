(define (app x_1 y_1)
  (_let 1
        x_1
        (lambda (x_1)
          (_let 1
                y_1
                (lambda (y_1)
                  (multi-memo 1 'app-1 '(1 2) (list x_1 y_1)))))))
(define (app-1 x_1 y_1)
  (_if 1
       (_op 1 'null? x_1)
       (lambda ()
         y_1)
       (lambda ()
         (_op 2
              'cons
              (_lift 1 1 (_op 1 'car x_1))
              (app (_op 1 'cdr x_1) y_1)))))
(define ($goal x_1 y_1)
  (app x_1 y_1))
