(define (f0 d) (+ (let ((x d)) 1)
		  (let ((y d)) 2)))

(define-data alist (anil) (acons acar acdr))
(define (f1 d) (acons (let ((x d)) 'head)
		      (let ((y d)) 'tail)))

(define (f2 d) (f1 (f1 d))) 

(define (f3 s d) (acdr (acons s d))) 
(define (f4 s d) (acar (acons s d))) 
