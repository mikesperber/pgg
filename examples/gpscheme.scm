; ________________________________________________
; Genetic Programming with Scheme
; version 1.0

; by Jonathan Kleid
;	 jjk@tiac.net

; created 10.26.95
; last modified 7.7.96

; source code based on the algorithms described 
; in the book "Genetic Programming" by John Koza
;
; (I borrowed the random number generator, but
; the rest is adapted from my own Mathematica code.)


; This is a simple Genetic Programming engine with a small 
; example of how to use it. The example performs symbolic
; differentiation using Genetic Programming.
; see <http://tommy.jsc.nasa.gov/~jjf/gp/> for info on GP.

; I have tested it with MacGambit 2.2.1 and Scheme Express 4.2

; One problem you might have is with the eval call in 
; the function get-stand-fitness. eval should use the 
; same environment as the program itself. If it compiles/
; interprets ok and it runs with no errors, then don't
; worry about it.

; _______________________________________________
; Minimal Standard Random Number Generator
; Park & Miller, CACM 31(10), Oct 1988, 32 bit integer version.
; better constants, as proposed by Park.
; By Ozan Yigit
; slightly modified 10.26.95 by Jonathan Kleid

(define (wrapper  numGens popSize complexity fSet fMap tSet)

(let* ((x 0)
       


       (*seed* 1))

(define (srand seed)
  (set! *seed* (round seed))
  *seed*)

(define (rand)
  (let ((A 48271)
        (M 2147483647)
        (Q 44488)
        (R 3399))
    (let* ((hi (quotient *seed* Q))
           (lo (modulo *seed* Q))
           (test (- (* A lo) (* R hi))))
      (if (> test 0)
          (set! *seed* test)
          (set! *seed* (+ test M)))))
  *seed*)

(define (random n)
  (let* ((M 2147483647)
         (slop (modulo M (round n))))
    (let loop ((r (rand)))
      (if (> r slop)
          (modulo r (round n))  
          (loop (rand))))))
; _______________________________________________


; List functions
; created 10.26.95 by Jonathan Kleid

; returns random index to element in tList

(define (get-random-index tList)
  (if (and (list? tList) (not (equal? `() tList)))
      (random (length tList))
      #f)
  )

; returns random element of tList

(define (get-random-elem tList)
  (if (list? tList)
      (list-ref tList (random (length tList)))
      #f)
  )

; safe cdr

(define (scdr tList)
  (if (or (equal? `() tList) (not (list? tList)))
      `()
      (cdr tList))
  )

; returns tList with elem inserted into tList replacing
; what was at position pos

(define (replace-at tList elem pos)
  (if (> pos (length tList))
      (append tList elem)
      (let
        ((newList `()))
        (do ((i pos (- i 1)))
            ((<= i 0) i)
            (set! newList (append newList (list (car tList))))
            (set! tList (cdr tList))           
            )
        (if (not (equal? `() tList))
            (set! tList (cdr tList))
            )
        (set! newList (append newList (list elem)))     
        (do ()
            ((equal? `() tList) newList)
            (set! newList (append newList (list (car tList))))
            (set! tList (cdr tList))
            )
        )
      )
  )

; Tree functions
; created 10.26.95 by Jonathan Kleid

; returns tree with newPart inserted at position led
; to by path. path is a one dimentional list with
; a variable number of elements. The first element
; specifies the branch to take at the highest level,
; the second element (if it exists) specifies the
; next branch to take, and so on until path is empty.
; It then replaces that final branch with newPart.
; This assumes that path does not lead to a depth 
; that is deeper than the tree.

(define (replace-part tree newPart path)
  (if (equal? `() path)
      newPart
      (replace-at tree
                  (replace-part (list-ref tree (car path))
                                newPart
                                (cdr path))
                  (car path))
      )
  )

; returns the path to a random node of tree.
; uses get-random-index

(define (get-random-node tree)
  (define (go-deeper tree path)
    (let
     ((index (get-random-index  (scdr tree))))
     (if index 
         (begin
          (set! index (+ index 1))
          (set! path (append path (list index)))
          (if (= 0 (random 2))
              (set! path (go-deeper (list-ref tree index) path)))
          ))
     path))
  (go-deeper tree `())
  )

; returns a new, random leaf using the terminal set tSet 
; and the functions fSet. fMap is contains the number of 
; arguments for each member of tSet.

(define (construct-random-leaf fSet fMap tSet)
  (let*
   ((fIndex (get-random-index   fSet))
   (leaf (list (list-ref fSet fIndex))))
   (do ((numArgs (list-ref fMap fIndex) (- numArgs 1)))
       ((<= numArgs 0) numArgs)
       (set! leaf (append leaf (list (get-random-elem tSet))))
       )
   leaf
   )
  )


; returns a new, random tree with totalElems elements,
; using the terminal set tSet and the functions fSet.
; fMap is contains the number of arguments for each
; member of tSet.
; modified 10.27.95 by Jonathan Kleid

(define (create-random-tree maxElems fSet fMap tSet)  
  (define (new-node tree elements-remaining fSet fMap tSet)
    (if (> elements-remaining 0)
        (new-node (replace-part tree
                                (construct-random-leaf fSet fMap tSet)
                                (get-random-node tree))
                  (- elements-remaining 1)
                  fSet
                  fMap
                  tSet)
        ; else:
        tree
        )
    )
  (random 10)
  (new-node `() maxElems fSet fMap tSet)
  )

; Genetic Programming population functions
; created 10.29.95 by Jonathan Kleid

; returns a list of the initial generation of randomly 
; created trees. Uses create-random-tree.
;
; popSize := number of trees in generation
; maxElems := maximum number of expressions in a single tree
; fSet := function set used for random tree synthesis
; fMap := the number of arguments for each function of fSet
; tSet := the terminal set for random tree synthesis

(define (init-generation popSize maxElems fSet fMap tSet)
  (define (gen-loop genList popSize maxElems fSet fMap tSet)
    (if (<= popSize 0)
        genList
        (gen-loop
         (append genList (list (create-random-tree maxElems fSet fMap tSet)))
         (- popSize 1)
         maxElems fSet fMap tSet)
        )
    )
  (gen-loop `() popSize maxElems fSet fMap tSet)
  )

; returns the fragment of tree led to by path.

(define (get-frag tree path)
  (if (equal? path `())
      tree
       (get-frag (list-ref tree (car path)) (cdr path))
      )
  )

; returns two a list containing two new trees,
; created from a genetic cross over at a random 
; point of tree1 and tree2.

(define (cross-over tree1 tree2)
  (let
   ((path1 (get-random-node tree1))
   (path2 (get-random-node tree2)))
   (List (replace-part tree1 (get-frag tree2 path2) path1)
         (replace-part tree2 (get-frag tree1 path1) path2))
   )
  )

; returns the standardized fitness of the
; tree (the lower the better)
;
; this function must be specifically defined for 
; each GP application.
; (it is the only function that needs to be written 
;	by a user for a complete run)
;
; This function should (_must_ is too strong a word)
; create random values for each member of the terminal
; set and then call eval on the tree. The result of eval
; could then be compared to the result of the optimal
; function. There may be other ways to write it, depending
; on the specific application.

(define (get-stand-fitness tree)
  ; **stub**
  1
  )

; safe divide function for analyze-standFitness
; (My convention: /x returns x when denom is zero)

(define (/1 numer denom)
  (if (= denom 0)
      1
      (/ numer denom))
  )

; returns a list of the normalized fitness of each member
; of the generation, and append the best score.
; It prints status information for each generation,
; so progress can be followed during a run.
; Uses the safe divide function: /1

(define (analyze-standFitness popSize standFitnessList popList)
  (let
      ((adjFitList `())
       (adjTotal 0)
       (adjFit 0)
       (standFit 0)
       (bestIndex 0)
       (bestFit -1))
    (let loop ((i 0) (standFitnessList standFitnessList))
      (if (< i popSize)
	  (let ((standFit (car standFitnessList)))
	    (if (or (< standFit bestFit) (= bestFit -1))
		(begin
		  (set! bestFit standFit)
		  (set! bestIndex i)))
	    (set! adjFit (/1 1 standFit))
	    (set! adjFitList
		  (append adjFitList
			  (list adjFit)))
	    (set! adjTotal (+ adjTotal adjFit))
	    (loop (+ i 1) (cdr standFitnessList))
	    )))
    (display "best score of generation: ")
    (display  bestFit)
    (newline)
    (display "best tree of generation: ")
    (display (list-ref popList bestIndex))
    (newline)

    (let loop ((i 0) (normFitList '()) (normBest 0))
      (if (< i popSize)
	  (let ((normFit (/1 (list-ref adjFitList i) adjTotal)))
	    (loop (+ i 1)
		  (append normFitList (list normFit))
		  (if (> normFit normBest) normFit normBest)))
	  (list normFitList normBest)))
    )
  )

; returns a random fraction no larger than max.

(define (random-fraction max)
  (/ (random (inexact->exact (* max 1000000))) 1000000)
  )

; returns index to a tree chosen. The likelyhood of any
; given tree being chosen is proportional to its normalized
; fitness, which is a positive fraction <= 1

(define (pick-weighted-random popSize normFitness bestNorm)
  (let
   ((rand-index (random popSize)))
   (if (>= (list-ref normFitness rand-index) (random-fraction bestNorm))
       rand-index
       (pick-weighted-random popSize normFitness bestNorm))
   )
  )


; returns a new generation created from the current generation.
; it uses normalized fitness to choose the trees.


(define (create-gen-from-normFit popSize popList normFitness bestNorm)
  (let
   ((newGen `()))
   (let loop ((i 0))
     (if (< i popSize)
	 (begin
	   (set! newGen
		 (append newGen
			 (cross-over (list-ref popList
					       (pick-weighted-random
						popSize normFitness bestNorm))
				     (list-ref popList
					       (pick-weighted-random
						popSize normFitness bestNorm)))))
	   (loop (+ i 2)))))
   newGen
   )
  )


; creates a new generation of trees based on the following inputs:
;
; popSize := number of trees in the list popList
; popList := list of the current generation
;
; create-newGen calls:
;    get-stand-fitness (user defined)
;    analyze-standFitness
;    create-gen-from-normFit


(define (create-newGen popSize popList)
  (let
      ((standFitList `()) 
       (normFitness `()))
    (let loop ((i 0))
      (if (< i popSize)
	  (begin
	    (set! standFitList
		  (append standFitList
			  (list (get-stand-fitness (list-ref popList i)))))
	    (loop (+ i 1)))))
    (set! normFitness (analyze-standFitness popSize standFitList popList))
    (create-gen-from-normFit
     popSize
     popList
     (car normFitness)
     (car (cdr normFitness)))
    )
  )

; returns the index of the most fit individual of the 
; population stored in popList.


(define (get-best popSize popList)
  (let
      ((bestFit (get-stand-fitness (list-ref popList 0)))
       (bestIndex 0)
       (standFit 0))
    (let loop ((i 1))
      (if (< i popSize)
	  (begin
	    (set! standFit (get-stand-fitness (list-ref popList i)))
	    (if (< standFit bestFit)
		(begin (set! bestFit standFit)
		       (set! bestIndex i)
		       bestIndex)
		)
	    (loop (+ i 1)))))
    (display "best score of generation: ")
    (display  bestFit)
    (newline)
    (display "best tree of generation: ")
    (display (list-ref popList bestIndex))
    (newline)
    bestIndex
    )
  )

; This is the only function that must be called directly for
; a GP run.
;
; numGens := total number of generations to produce (including initial)
; popSize := number of trees in generation
; complexity := maximum number of expressions in a single tree in
;               initial generation
; fSet := function set used for random tree synthesis
; fMap := the number of arguments for each function of fSet
; tSet := the terminal set for random tree synthesis
;
; Make sure (get-stand-fitness tree) is defined !!!!


(define (GP-run numGens popSize complexity fSet fMap tSet)
  (let
    ((popList (init-generation popSize complexity fSet fMap tSet)))
    (let loop ((i 2))
      (if (< i numGens)
	  (begin
	    (set! popList (create-newGen popSize popList))
	    (loop (+ i 1)))))
    (get-best popSize popList)
    (list-ref popList (get-best popSize popList))
    )
  )

; __________________________________________
; GP Application: Symbolic Differentiation
; created 10.29.95 by Jonathan Kleid

; exponent operator, (returns x^y)

(define (^ x y)
  (define (loop x total y)
    (if (> y 0)
        (loop x (* x total) (- y 1))
        total
        )
    )
  (if (>= y 0)
      (loop x 1 y)
      (/ 1 (loop x 1 (- 0 y)))
      )
  )

; The equation we are differentiating.
; Feel free to change it to whatever you want:

(define (equ x) (* 2 (^ x 4)))

; The rest is for the fitness test

(define (mTan x)
  (let
    ((h 0.000000001))
    (/ (- (equ (+ x h)) (equ x))  h)
    )
  )

(define (randPoint)
  (/ (+ 1 (random 10)) (+ 1 (random 10)))
  )

; work around for case when x is entire tree:

;;(define x 0);pjt:moved

; computes fitness of tree. 

(define (get-stand-fitness tree)
  (let
      ((points 20)
       (fitness 0)
       (randMax 10))
    (let loop ((i 0))
      (if (> i points)
	  fitness
	  (begin
	    (set! x (randPoint))
	    (set! fitness
		  (+ fitness (abs (- (mTan x) (eval tree (interaction-environment))))))
	    (loop (+ i 1)))))
    )
  )


; feel free to change these values. size-of-pop and 
; number-generations should be much higher for a successful 
; run, but the computational power needed for high values can 
; be large, although the running time is O(n). 
; Including the ^ operator in function-set seems to slow the 
; run down almost to a halt, so I left it out. If you make equ 
; more complicated and add any members to the terminal set, be sure 
; to add (define name 0) for each new member. This is for the case 
; when an entire tree is composed of only a single variable.

(GP-run numGens popSize complexity fSet fMap tSet)))

;;(define number-generations 40)
;;(define size-of-pop 40)
;;(define tree-complexity 4)
;;(define function-set `(* +))
;;(define function-map `(2 2))
;;(define terminal-set `(x))

;;(gp-run number-generations size-of-pop tree-complexity function-set function-map terminal-set)


