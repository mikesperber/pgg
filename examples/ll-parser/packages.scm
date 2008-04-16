; Interfaces

(define-interface grammar-interface
  (export grammar-productions grammar-nonterminals
	  grammar-start grammar-error
	  grammar-number-of-nonterminals
	  grammar-productions-with-lhs
	  grammar-fetch-property
	  grammar-symbol->name
	  production-lhs production-rhs production-attribution

	  grammar-start-production
	  terminal? nonterminal?
	  (define-grammar :syntax)

	  nonterminal-nullable? sequence-nullable?
	  nonterminal-first sequence-first
	  nonterminal-follow))

(define-interface scc-union-interface
  (export complete-subsets!))

(define-interface parser-interface
  (export parse))

(define-interface toy-grammars-interface
  (export g10 (g10-symbol :syntax)))

; Structures

(define-structure grammar grammar-interface
  (open scheme big-util define-record-types enumerated scc-union)
  (files grammar))

(define-structure scc-union scc-union-interface
  (open scheme)
  (files scc-union))

(define-structure toy-grammars toy-grammars-interface
  (open scheme enumerated grammar)
  (files toy-grammars))
