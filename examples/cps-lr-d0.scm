(define (shift-terminal grammar_1
                        k_1
                        first-map_1
                        continuations_1
                        closure_1
                        terminal_1
                        input_2
                        fail_1)
  (_let 1
        'grammar_1
        grammar_1
        (lambda (grammar_1)
          (_let 1
                'k_1
                k_1
                (lambda (k_1)
                  (_let 1
                        'first-map_1
                        first-map_1
                        (lambda (first-map_1)
                          (_let 2
                                'continuations_1
                                continuations_1
                                (lambda (continuations_1)
                                  (_let 2
                                        'closure_1
                                        closure_1
                                        (lambda (closure_1)
                                          (_let 1
                                                'terminal_1
                                                terminal_1
                                                (lambda (terminal_1)
                                                  (_let 1
                                                        'input_2
                                                        input_2
                                                        (lambda (input_2)
                                                          (_let 1
                                                                'fail_1
                                                                fail_1
                                                                (lambda (fail_1)
                                                                  (the-trick terminal_1
                                                                             (_op 2
                                                                                  next-terminals
                                                                                  closure_1
                                                                                  (_lift 1
                                                                                         1
                                                                                         grammar_1))
                                                                             (multi-memo 2
                                                                                         'shift-terminal-340
                                                                                         '(1
                                                                                           1
                                                                                           1
                                                                                           2
                                                                                           2
                                                                                           2)
                                                                                         (list grammar_1
                                                                                               k_1
                                                                                               first-map_1
                                                                                               closure_1
                                                                                               continuations_1
                                                                                               input_2))
                                                                             fail_1))))))))))))))))))
(define (shift-terminal-340 grammar_1
                            k_1
                            first-map_1
                            closure_1
                            continuations_1
                            input_2)
  (_lambda_memo '2
                '(symbol_1)
                '8
                (list grammar_1
                      k_1
                      first-map_1
                      closure_1
                      continuations_1
                      input_2)
                '(1 1 1 2 2 2)
                (lambda (grammar_1 k_1
                                   first-map_1
                                   closure_1
                                   continuations_1
                                   input_2)
                  (lambda (symbol_1)
                    (_let 1
                          'symbol_1
                          symbol_1
                          (lambda (symbol_1)
                            (_let 2
                                  'next-state_1
                                  (_op 2 goto closure_1 symbol_1)
                                  (lambda (next-state_1)
                                    (parse grammar_1
                                           k_1
                                           first-map_1
                                           next-state_1
                                           (_op 2
                                                cons
                                                (multi-memo 2
                                                            'shift-terminal-341
                                                            '(1 1 1 2 2)
                                                            (list grammar_1
                                                                  k_1
                                                                  first-map_1
                                                                  continuations_1
                                                                  closure_1))
                                                (take (_op 2
                                                           -
                                                           (_op 2
                                                                active
                                                                next-state_1)
                                                           (_lift 1
                                                                  1
                                                                  (_lift0 1
                                                                          '1)))
                                                      continuations_1))
                                           input_2)))))))))
(define (shift-terminal-341 grammar_1 k_1 first-map_1 continuations_1 closure_1)
  (_lambda_memo '2
                '(nonterminal input)
                '9
                (list grammar_1 k_1 first-map_1 continuations_1 closure_1)
                '(1 1 1 2 2)
                (lambda (grammar_1 k_1 first-map_1 continuations_1 closure_1)
                  (lambda (nonterminal input)
                    (_let 1
                          'nonterminal
                          nonterminal
                          (lambda (nonterminal)
                            (_let 1
                                  'input
                                  input
                                  (lambda (input)
                                    (shift-nonterminal grammar_1
                                                       k_1
                                                       first-map_1
                                                       continuations_1
                                                       closure_1
                                                       nonterminal
                                                       input)))))))))
(define (shift-nonterminal grammar_1
                           k_1
                           first-map_1
                           continuations_1
                           closure_1
                           nonterminal_1
                           input_3)
  (_let 1
        'grammar_1
        grammar_1
        (lambda (grammar_1)
          (_let 1
                'k_1
                k_1
                (lambda (k_1)
                  (_let 1
                        'first-map_1
                        first-map_1
                        (lambda (first-map_1)
                          (_let 2
                                'continuations_1
                                continuations_1
                                (lambda (continuations_1)
                                  (_let 2
                                        'closure_1
                                        closure_1
                                        (lambda (closure_1)
                                          (_let 1
                                                'nonterminal_1
                                                nonterminal_1
                                                (lambda (nonterminal_1)
                                                  (_let 1
                                                        'input_3
                                                        input_3
                                                        (lambda (input_3)
                                                          (the-trick nonterminal_1
                                                                     (_op 2
                                                                          next-nonterminals
                                                                          closure_1
                                                                          (_lift 1
                                                                                 1
                                                                                 grammar_1))
                                                                     (multi-memo 2
                                                                                 'shift-nonterminal-338
                                                                                 '(1
                                                                                   1
                                                                                   1
                                                                                   2
                                                                                   2
                                                                                   2)
                                                                                 (list grammar_1
                                                                                       k_1
                                                                                       first-map_1
                                                                                       closure_1
                                                                                       continuations_1
                                                                                       input_3))
                                                                     (_lift 1
                                                                            1
                                                                            (_lambda_memo '1
                                                                                          '()
                                                                                          '7
                                                                                          (list)
                                                                                          '()
                                                                                          (lambda ()
                                                                                            (lambda ()
                                                                                              (_lift0 1
                                                                                                      'you-cannot-see-me)))))))))))))))))))))
(define (shift-nonterminal-338 grammar_1
                               k_1
                               first-map_1
                               closure_1
                               continuations_1
                               input_3)
  (_lambda_memo '2
                '(symbol_2)
                '5
                (list grammar_1
                      k_1
                      first-map_1
                      closure_1
                      continuations_1
                      input_3)
                '(1 1 1 2 2 2)
                (lambda (grammar_1 k_1
                                   first-map_1
                                   closure_1
                                   continuations_1
                                   input_3)
                  (lambda (symbol_2)
                    (_let 1
                          'symbol_2
                          symbol_2
                          (lambda (symbol_2)
                            (_let 2
                                  'next-state_2
                                  (_op 2 goto closure_1 symbol_2)
                                  (lambda (next-state_2)
                                    (parse grammar_1
                                           k_1
                                           first-map_1
                                           next-state_2
                                           (_op 2
                                                cons
                                                (multi-memo 2
                                                            'shift-nonterminal-339
                                                            '(1 1 1 2 2)
                                                            (list grammar_1
                                                                  k_1
                                                                  first-map_1
                                                                  continuations_1
                                                                  closure_1))
                                                (take (_op 2
                                                           -
                                                           (_op 2
                                                                active
                                                                next-state_2)
                                                           (_lift 1
                                                                  1
                                                                  (_lift0 1
                                                                          '1)))
                                                      continuations_1))
                                           input_3)))))))))
(define (shift-nonterminal-339 grammar_1
                               k_1
                               first-map_1
                               continuations_1
                               closure_1)
  (_lambda_memo '2
                '(nonterminal input)
                '6
                (list grammar_1 k_1 first-map_1 continuations_1 closure_1)
                '(1 1 1 2 2)
                (lambda (grammar_1 k_1 first-map_1 continuations_1 closure_1)
                  (lambda (nonterminal input)
                    (_let 1
                          'nonterminal
                          nonterminal
                          (lambda (nonterminal)
                            (_let 1
                                  'input
                                  input
                                  (lambda (input)
                                    (shift-nonterminal grammar_1
                                                       k_1
                                                       first-map_1
                                                       continuations_1
                                                       closure_1
                                                       nonterminal
                                                       input)))))))))
(define (loop fail_2 element_1 cont_1 set_2)
  (_let 0
        'fail_2
        fail_2
        (lambda (fail_2)
          (_let 1
                'element_1
                element_1
                (lambda (element_1)
                  (_let 0
                        'cont_1
                        cont_1
                        (lambda (cont_1)
                          (_let 1
                                'set_2
                                set_2
                                (lambda (set_2)
                                  (_lift 1
                                         1
                                         (multi-memo 1
                                                     'loop-336
                                                     '(1 1 1 1)
                                                     (list fail_2
                                                           element_1
                                                           cont_1
                                                           set_2))))))))))))
(define (loop-336 fail_2 element_1 cont_1 set_2)
  (_if 1
       (_op 1 null? set_2)
       (_lift 1 1 (_app_memo 1 fail_2))
       (_lift 1
              1
              (multi-memo 1
                          'loop-337
                          '(1 1 1 1)
                          (list fail_2 element_1 cont_1 set_2)))))
(define (loop-337 fail_2 element_1 cont_1 set_2)
  (_if 1
       (_op 1 equal? element_1 (_op 1 car set_2))
       (_lift 1 1 (_app_memo 1 cont_1 (_op 1 car set_2)))
       (loop (_lift 1 1 fail_2)
             (_lift 1 1 element_1)
             (_lift 1 1 cont_1)
             (_lift 1 1 (_op 1 cdr set_2)))))
(define (loop fail_3 input-front_1 cont_2 item-set_2)
  (_let 1
        'fail_3
        fail_3
        (lambda (fail_3)
          (_let 2
                'input-front_1
                input-front_1
                (lambda (input-front_1)
                  (_let 1
                        'cont_2
                        cont_2
                        (lambda (cont_2)
                          (_let 2
                                'item-set_2
                                item-set_2
                                (lambda (item-set_2)
                                  (multi-memo 2
                                              'loop-334
                                              '(2 2 2 2)
                                              (list fail_3
                                                    input-front_1
                                                    cont_2
                                                    item-set_2)))))))))))
(define (loop-334 fail_3 input-front_1 cont_2 item-set_2)
  (_if 2
       (_op 2 null? item-set_2)
       (_app_memo 2 fail_3)
       (_let 2
             'item_2
             (_op 2 car item-set_2)
             (lambda (item_2)
               (multi-memo 2
                           'loop-335
                           '(2 2 2 2 2)
                           (list item_2
                                 fail_3
                                 input-front_1
                                 cont_2
                                 item-set_2))))))
(define (loop-335 item_2 fail_3 input-front_1 cont_2 item-set_2)
  (_if 2
       (_op 2 equal? input-front_1 (_op 2 item-lookahead item_2))
       (_app_memo 2 cont_2 item_2)
       (loop fail_3 input-front_1 cont_2 (_op 2 cdr item-set_2))))
(define (take n_1 l_1)
  (_let 2
        'n_1
        n_1
        (lambda (n_1)
          (_let 2
                'l_1
                l_1
                (lambda (l_1)
                  (multi-memo 2 'take-333 '(2 2) (list n_1 l_1)))))))
(define (take-333 n_1 l_1)
  (_if 2
       (_op 2 zero? n_1)
       (_lift 1 1 (_lift0 1 '()))
       (_op 2
            cons
            (_op 2 car l_1)
            (take (_op 2 - n_1 (_lift 1 1 (_lift0 1 '1))) (_op 2 cdr l_1)))))
(define (select-lookahead-item-the-trick item-set_1 k_3 input_5 cont_2 fail_3)
  (_let 1
        'item-set_1
        item-set_1
        (lambda (item-set_1)
          (_let 0
                'k_3
                k_3
                (lambda (k_3)
                  (_let 1
                        'input_5
                        input_5
                        (lambda (input_5)
                          (_let 1
                                'cont_2
                                cont_2
                                (lambda (cont_2)
                                  (_let 0
                                        'fail_3
                                        fail_3
                                        (lambda (fail_3)
                                          (_let 1
                                                'input-front_1
                                                (take (_lift 1 1 k_3) input_5)
                                                (lambda (input-front_1)
                                                  (loop (_lift 1 1 fail_3)
                                                        input-front_1
                                                        cont_2
                                                        item-set_1))))))))))))))
(define (the-trick element_1 set_1 cont_1 fail_2)
  (_let 1
        'element_1
        element_1
        (lambda (element_1)
          (_let 1
                'set_1
                set_1
                (lambda (set_1)
                  (_let 1
                        'cont_1
                        cont_1
                        (lambda (cont_1)
                          (_let 1
                                'fail_2
                                fail_2
                                (lambda (fail_2)
                                  (loop fail_2 element_1 cont_1 set_1))))))))))
(define (do-parse source-grammar_1 k_2 input_4)
  (_let 0
        'source-grammar_1
        source-grammar_1
        (lambda (source-grammar_1)
          (_let 1
                'k_2
                k_2
                (lambda (k_2)
                  (_let 1
                        'input_4
                        input_4
                        (lambda (input_4)
                          (_let 1
                                'grammar_2
                                (_op 1
                                     source-grammar->grammar
                                     source-grammar_1
                                     k_2)
                                (lambda (grammar_2)
                                  (_let 1
                                        'start-production_1
                                        (_op 1
                                             car
                                             (_op 1
                                                  grammar-productions
                                                  grammar_2))
                                        (lambda (start-production_1)
                                          (parse grammar_2
                                                 k_2
                                                 (_op 1
                                                      compute-first
                                                      grammar_2
                                                      k_2)
                                                 (_lift 1
                                                        1
                                                        (_op 1
                                                             list
                                                             (_op 1
                                                                  make-item
                                                                  start-production_1
                                                                  (_lift0 1 '0)
                                                                  (_op 1
                                                                       cdr
                                                                       (_op 1
                                                                            production-rhs
                                                                            start-production_1)))))
                                                 (_lift 1
                                                        1
                                                        (_lift0 1
                                                                '()))
                                                 (_op 2
                                                      append
                                                      input_4
                                                      (_lift 1
                                                             1
                                                             (_op 1
                                                                  make-$
                                                                  k_2)))))))))))))))
(define (parse grammar_1 k_1 first-map_1 state_1 continuations_1 input_1)
  (_let 1
        'grammar_1
        grammar_1
        (lambda (grammar_1)
          (_let 1
                'k_1
                k_1
                (lambda (k_1)
                  (_let 1
                        'first-map_1
                        first-map_1
                        (lambda (first-map_1)
                          (_let 2
                                'state_1
                                state_1
                                (lambda (state_1)
                                  (_let 2
                                        'continuations_1
                                        continuations_1
                                        (lambda (continuations_1)
                                          (_let 2
                                                'input_1
                                                input_1
                                                (lambda (input_1)
                                                  (multi-memo 2
                                                              'parse-329
                                                              '(2 1 1 1 2 2)
                                                              (list state_1
                                                                    grammar_1
                                                                    k_1
                                                                    first-map_1
                                                                    continuations_1
                                                                    input_1)))))))))))))))
(define (parse-329 state_1 grammar_1 k_1 first-map_1 continuations_1 input_1)
  (_if 2
       (_if 2
            (_op 2 final? state_1 (_lift 1 1 grammar_1))
            (_op 2 equal? (_lift 1 1 (_lift0 1 '$)) (_op 2 car input_1))
            (_lift 1 1 (_lift0 1 '#f)))
       (_lift 1 1 (_lift0 1 'accept))
       (_let 2
             'closure_1
             (_op 2
                  compute-closure
                  state_1
                  (_lift 1 1 grammar_1)
                  (_lift 1 1 k_1)
                  (_lift 1 1 first-map_1))
             (lambda (closure_1)
               (_let 1
                     'accept-items_1
                     (_op 2 accept closure_1)
                     (lambda (accept-items_1)
                       (shift-terminal grammar_1
                                       k_1
                                       first-map_1
                                       continuations_1
                                       closure_1
                                       (_op 2 car input_1)
                                       (_op 2 cdr input_1)
                                       (multi-memo 2
                                                   'parse-330
                                                   '(2 1 1 1 2 2 2)
                                                   (list accept-items_1
                                                         grammar_1
                                                         k_1
                                                         first-map_1
                                                         closure_1
                                                         continuations_1
                                                         input_1)))))))))
(define (parse-330 accept-items_1
                   grammar_1
                   k_1
                   first-map_1
                   closure_1
                   continuations_1
                   input_1)
  (_lambda_memo '2
                '()
                '1
                (list accept-items_1
                      grammar_1
                      k_1
                      first-map_1
                      closure_1
                      continuations_1
                      input_1)
                '(2 1 1 1 2 2 2)
                (lambda (accept-items_1 grammar_1
                                        k_1
                                        first-map_1
                                        closure_1
                                        continuations_1
                                        input_1)
                  (lambda ()
                    (select-lookahead-item-the-trick accept-items_1
                                                     k_1
                                                     input_1
                                                     (multi-memo 2
                                                                 'parse-331
                                                                 '(1 1 1 2 2 2)
                                                                 (list grammar_1
                                                                       k_1
                                                                       first-map_1
                                                                       closure_1
                                                                       continuations_1
                                                                       input_1))
                                                     (_lambda_memo '1
                                                                   '()
                                                                   '4
                                                                   (list)
                                                                   '()
                                                                   (lambda ()
                                                                     (lambda ()
                                                                       (_lift0 1
                                                                               'error)))))))))
(define (parse-331 grammar_1 k_1 first-map_1 closure_1 continuations_1 input_1)
  (_lambda_memo '2
                '(item_1)
                '2
                (list grammar_1
                      k_1
                      first-map_1
                      closure_1
                      continuations_1
                      input_1)
                '(1 1 1 2 2 2)
                (lambda (grammar_1 k_1
                                   first-map_1
                                   closure_1
                                   continuations_1
                                   input_1)
                  (lambda (item_1)
                    (_let 2
                          'item_1
                          item_1
                          (lambda (item_1)
                            (_app_memo 2
                                       (_op 2
                                            list-ref
                                            (_op 2
                                                 cons
                                                 (multi-memo 2
                                                             'parse-332
                                                             '(1 1 1 2 2)
                                                             (list grammar_1
                                                                   k_1
                                                                   first-map_1
                                                                   continuations_1
                                                                   closure_1))
                                                 continuations_1)
                                            (_op 2
                                                 length
                                                 (_op 2 item-rhs item_1)))
                                       (_op 2 item-lhs item_1)
                                       input_1)))))))
(define (parse-332 grammar_1 k_1 first-map_1 continuations_1 closure_1)
  (_lambda_memo '2
                '(nonterminal input)
                '3
                (list grammar_1 k_1 first-map_1 continuations_1 closure_1)
                '(1 1 1 2 2)
                (lambda (grammar_1 k_1 first-map_1 continuations_1 closure_1)
                  (lambda (nonterminal input)
                    (_let 1
                          'nonterminal
                          nonterminal
                          (lambda (nonterminal)
                            (_let 1
                                  'input
                                  input
                                  (lambda (input)
                                    (shift-nonterminal grammar_1
                                                       k_1
                                                       first-map_1
                                                       continuations_1
                                                       closure_1
                                                       nonterminal
                                                       input)))))))))
(define ($goal source-grammar_1 k_2 input_4)
  (do-parse source-grammar_1 k_2 input_4))
