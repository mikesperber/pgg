(define-syntax and
  (syntax-rules ()
    ((and)
     #t)
    ((and e)
     e)
    ((and e1 e2 ...)
     (if e1 (and e2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or)
     #f)
    ((or e)
     e)
    ((or e1 e2 ...)
     (let ((t e1))
       (if t t (or e2 ...))))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
	   (result temp)
	   (cond clause1 clause2 ...))))
    ((cond (test))
     test)
    ((cond (test) clause1 clause2 ...)
     (or test (cond clause1 clause2 ...)))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...) clause1 clause2 ...)
     (if test
	 (begin result1 result2 ...)
	 (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
	 (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...) clause1 clause2 ...)
     (if (memv key '(atoms ...))
	 (begin result1 result2 ...)
	 (case key clause1 clause2 ...)))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body ...)
     (let () body ...))
    ((let* ((x e) rest ...) body ...)
     (let ((x e))
       (let* (rest ...) body ...)))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...) (test exp ...) cmd ...)
     (let loop ((var init) ...)
       (cond
	(test exp ...)
	(else cmd ...
	      (loop (do "step" var step ...) ...)))))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))
