; Balanced parentheses

(define-grammar g00 g00-symbol
  (S)
  (l)
  S
  (((S l) $1)))

(define-grammar g08 g08-symbol
  (S T)
  (l r)
  S
  (((S S T) $1)
   ((S T) $1)
   ((T l S r) $1)
   ((T l r) $1)))

; Constant arithmetic expressions

(define-grammar g10 g10-symbol
  (E T P)
  (+ - * / l r n)
  E
  (((E T) $1)
   ((E T + E) (+ $1 $3))
   ((E T - E) (- $1 $3))
   ((T P) $1)
   ((T P * T) (* $1 $3))
   ((T P / T) (/ $1 $3))
   ((P n) $1)
   ((P l E r) $2)))

(define-grammar g10-error g10-error-symbol
  (E T P)
  (+ - * / l r n)
  E
  (((E T) $1)
   ((E $error) 0)
   ((E T + E) (+ $1 $3))
   ((E T - E) (- $1 $3))
   ((T P) $1)
   ((T P * T) (* $1 $3))
   ((T P / T) (/ $1 $3))
   ((P n) $1)
   ((P l E r) $2)
   ((P l $error r) 0)))

(define-grammar g13 g13-symbol
  (S SLK NESLK SLD NESLD K P N)
  (comma blah dot)
  S
  (((SLK) $1)
   ((SLK NESLK) $1)
   ((NESLK N) $1)
   ((NESLK NESLK K N) $1)
   ((SLD) $1)
   ((SLD NESLD) $1)
   ((NESLD N) $1)
   ((NESLD NESLD P N) $1)
   ((S SLK) $1)
   ((S SLD) $1)
   ((K comma) $1)
   ((P dot) $1)
   ((N blah) $1)))
   
;; javascript example expanded

(define-grammar g14 g14-symbol
  (S E C A OL AL ON AN)
  (c comma colon lcurly rcurly lbracket rbracket)
  S
  (((S E) $1)
   ((E c) $1)
   ((E lcurly OL rcurly) $1)
   ((E lbracket AL rcurly) $1)
   ((C comma) $1)
   ((A c colon E) $1)
   ((OL) $1)
   ((OL ON) $1)
   ((ON A) $1)
   ((ON ON C A) $1)
   ((AL) $1)
   ((AL AN) $1)
   ((AN E) $1)
   ((AN AN C E) $1)))


