(define (shift-terminal_1 grammar_1
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
                                  (_let 1
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
                                                          (_let 0
                                                                'fail_1
                                                                fail_1
                                                                (lambda (fail_1)
                                                                  (the-trick terminal_1
                                                                             (_op 1
                                                                                  next-terminals
                                                                                  closure_1
                                                                                  grammar_1)
                                                                             (_lambda_memo '1
                                                                                           '(symbol_1)
                                                                                           '8
                                                                                           (list grammar_1
                                                                                                 k_1
                                                                                                 first-map_1
                                                                                                 closure_1
                                                                                                 continuations_1
                                                                                                 input_2)
                                                                                           '(1
                                                                                             1
                                                                                             1
                                                                                             1
                                                                                             2
                                                                                             2)
                                                                                           (lambda (grammar_1 k_1
                                                                                                              first-map_1
                                                                                                              closure_1
                                                                                                              continuations_1
                                                                                                              input_2)
                                                                                             (lambda (symbol_1)
                                                                                               (_let 0
                                                                                                     'symbol_1
                                                                                                     symbol_1
                                                                                                     (lambda (symbol_1)
                                                                                                       (_let 1
                                                                                                             'next-state_1
                                                                                                             (_op 1
                                                                                                                  goto
                                                                                                                  closure_1
                                                                                                                  symbol_1)
                                                                                                             (lambda (next-state_1)
                                                                                                               (parse grammar_1
                                                                                                                      k_1
                                                                                                                      first-map_1
                                                                                                                      next-state_1
                                                                                                                      (_op 2
                                                                                                                           cons
                                                                                                                           (multi-memo 2
                                                                                                                                       'shift-terminal_1-15
                                                                                                                                       shift-terminal_1-15
                                                                                                                                       '(1
                                                                                                                                         1
                                                                                                                                         1
                                                                                                                                         2
                                                                                                                                         1)
                                                                                                                                       (list grammar_1
                                                                                                                                             k_1
                                                                                                                                             first-map_1
                                                                                                                                             continuations_1
                                                                                                                                             closure_1))
                                                                                                                           (chop (_op 1
                                                                                                                                      -
                                                                                                                                      (_op 1
                                                                                                                                           active
                                                                                                                                           next-state_1)
                                                                                                                                      (_lift0 1
                                                                                                                                              '1))
                                                                                                                                 continuations_1))
                                                                                                                      input_2))))))))
                                                                             fail_1))))))))))))))))))
(define (shift-terminal_1-15 grammar_1
                             k_1
                             first-map_1
                             continuations_1
                             closure_1)
  (_lambda '2
           '(nonterminal input)
           (lambda (nonterminal input)
             (_let 1
                   'nonterminal
                   nonterminal
                   (lambda (nonterminal)
                     (_let 1
                           'input
                           input
                           (lambda (input)
                             (shift-nonterminal_1 grammar_1
                                                  k_1
                                                  first-map_1
                                                  continuations_1
                                                  closure_1
                                                  nonterminal
                                                  input))))))))
(define (shift-nonterminal_1 grammar_1
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
                                  (_let 1
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
                                                                     (_op 1
                                                                          next-nonterminals
                                                                          closure_1
                                                                          grammar_1)
                                                                     (_lambda_memo '1
                                                                                   '(symbol_2)
                                                                                   '5
                                                                                   (list grammar_1
                                                                                         k_1
                                                                                         first-map_1
                                                                                         closure_1
                                                                                         continuations_1
                                                                                         input_3)
                                                                                   '(1
                                                                                     1
                                                                                     1
                                                                                     1
                                                                                     2
                                                                                     2)
                                                                                   (lambda (grammar_1 k_1
                                                                                                      first-map_1
                                                                                                      closure_1
                                                                                                      continuations_1
                                                                                                      input_3)
                                                                                     (lambda (symbol_2)
                                                                                       (_let 0
                                                                                             'symbol_2
                                                                                             symbol_2
                                                                                             (lambda (symbol_2)
                                                                                               (_let 1
                                                                                                     'next-state_2
                                                                                                     (_op 1
                                                                                                          goto
                                                                                                          closure_1
                                                                                                          symbol_2)
                                                                                                     (lambda (next-state_2)
                                                                                                       (parse grammar_1
                                                                                                              k_1
                                                                                                              first-map_1
                                                                                                              next-state_2
                                                                                                              (_op 2
                                                                                                                   cons
                                                                                                                   (multi-memo 2
                                                                                                                               'shift-nonterminal_1-14
                                                                                                                               shift-nonterminal_1-14
                                                                                                                               '(1
                                                                                                                                 1
                                                                                                                                 1
                                                                                                                                 2
                                                                                                                                 1)
                                                                                                                               (list grammar_1
                                                                                                                                     k_1
                                                                                                                                     first-map_1
                                                                                                                                     continuations_1
                                                                                                                                     closure_1))
                                                                                                                   (chop (_op 1
                                                                                                                              -
                                                                                                                              (_op 1
                                                                                                                                   active
                                                                                                                                   next-state_2)
                                                                                                                              (_lift0 1
                                                                                                                                      '1))
                                                                                                                         continuations_1))
                                                                                                              input_3))))))))
                                                                     (_lambda_memo '1
                                                                                   '()
                                                                                   '7
                                                                                   (list)
                                                                                   '()
                                                                                   (lambda ()
                                                                                     (lambda ()
                                                                                       (_lift 1
                                                                                              1
                                                                                              (_lift0 1
                                                                                                      'you-cannot-see-me)))))))))))))))))))))
(define (shift-nonterminal_1-14 grammar_1
                                k_1
                                first-map_1
                                continuations_1
                                closure_1)
  (_lambda '2
           '(nonterminal input)
           (lambda (nonterminal input)
             (_let 1
                   'nonterminal
                   nonterminal
                   (lambda (nonterminal)
                     (_let 1
                           'input
                           input
                           (lambda (input)
                             (shift-nonterminal_1 grammar_1
                                                  k_1
                                                  first-map_1
                                                  continuations_1
                                                  closure_1
                                                  nonterminal
                                                  input))))))))
(define (loop_1 fail_2 element_1 cont_1 set_2)
  (_let 0
        'fail_2
        fail_2
        (lambda (fail_2)
          (_let 2
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
                                  (multi-memo 1
                                              'loop_1-12
                                              loop_1-12
                                              '(1 2 1 1)
                                              (list fail_2
                                                    element_1
                                                    cont_1
                                                    set_2)))))))))))
(define (loop_1-12 fail_2 element_1 cont_1 set_2)
  (_if 1
       (_op 1 null? set_2)
       (_app_memo 1 fail_2)
       (multi-memo 2
                   'loop_1-13
                   loop_1-13
                   '(1 2 1 1)
                   (list fail_2 element_1 cont_1 set_2))))
(define (loop_1-13 fail_2 element_1 cont_1 set_2)
  (_if 2
       (_op 2 equal? element_1 (_lift 1 1 (_op 1 car set_2)))
       (_app_memo 1 cont_1 (_op 1 car set_2))
       (loop_1 fail_2 element_1 cont_1 (_op 1 cdr set_2))))
(define (loop_2 fail_3 input-front_1 cont_2 item-set_2)
  (_let 0
        'fail_3
        fail_3
        (lambda (fail_3)
          (_let 2
                'input-front_1
                input-front_1
                (lambda (input-front_1)
                  (_let 0
                        'cont_2
                        cont_2
                        (lambda (cont_2)
                          (_let 1
                                'item-set_2
                                item-set_2
                                (lambda (item-set_2)
                                  (multi-memo 1
                                              'loop_2-10
                                              loop_2-10
                                              '(1 2 1 1)
                                              (list fail_3
                                                    input-front_1
                                                    cont_2
                                                    item-set_2)))))))))))
(define (loop_2-10 fail_3 input-front_1 cont_2 item-set_2)
  (_if 1
       (_op 1 null? item-set_2)
       (_lift 1 1 (_app_memo 1 fail_3))
       (_let 1
             'item_2
             (_op 1 car item-set_2)
             (lambda (item_2)
               (multi-memo 2
                           'loop_2-11
                           loop_2-11
                           '(1 1 2 1 1)
                           (list item_2
                                 fail_3
                                 input-front_1
                                 cont_2
                                 item-set_2))))))
(define (loop_2-11 item_2 fail_3 input-front_1 cont_2 item-set_2)
  (_if 2
       (_op 2 equal? input-front_1 (_lift 1 1 (_op 1 item-lookahead item_2)))
       (_app_memo 1 cont_2 item_2)
       (loop_2 fail_3 input-front_1 cont_2 (_op 1 cdr item-set_2))))
(define (chop n_1 l_1)
  (_let 1
        'n_1
        n_1
        (lambda (n_1)
          (_let 2
                'l_1
                l_1
                (lambda (l_1)
                  (multi-memo 1 'chop-9 chop-9 '(1 2) (list n_1 l_1)))))))
(define (chop-9 n_1 l_1)
  (_if 1
       (_op 1 zero? n_1)
       (_lift 1 1 (_lift0 1 '()))
       (_op 2
            cons
            (_op 2 car l_1)
            (chop (_op 1 - n_1 (_lift0 1 '1)) (_op 2 cdr l_1)))))
(define (select-lookahead-item-the-trick item-set_1 k_3 input_5 cont_2 fail_3)
  (_let 0
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
                          (_let 0
                                'cont_2
                                cont_2
                                (lambda (cont_2)
                                  (_let 0
                                        'fail_3
                                        fail_3
                                        (lambda (fail_3)
                                          (_let 1
                                                'input-front_1
                                                (chop k_3 input_5)
                                                (lambda (input-front_1)
                                                  (loop_2 fail_3
                                                          input-front_1
                                                          cont_2
                                                          item-set_1))))))))))))))
(define (the-trick element_1 set_1 cont_1 fail_2)
  (_let 1
        'element_1
        element_1
        (lambda (element_1)
          (_let 0
                'set_1
                set_1
                (lambda (set_1)
                  (_let 0
                        'cont_1
                        cont_1
                        (lambda (cont_1)
                          (_let 0
                                'fail_2
                                fail_2
                                (lambda (fail_2)
                                  (loop_1 fail_2
                                          element_1
                                          cont_1
                                          set_1))))))))))
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
                                                                     start-production_1))))
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
                          (_let 1
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
                                                              'parse-7
                                                              parse-7
                                                              '(1 1 1 1 2 2)
                                                              (list state_1
                                                                    grammar_1
                                                                    k_1
                                                                    first-map_1
                                                                    continuations_1
                                                                    input_1)))))))))))))))
(define (parse-7 state_1 grammar_1 k_1 first-map_1 continuations_1 input_1)
  (_if 2
       (_if 1
            (_op 1 final? state_1 grammar_1)
            (_op 2 equal? (_lift 1 1 (_lift0 1 '$)) (_op 2 car input_1))
            (_lift 1 1 (_lift0 1 '#f)))
       (_lift 1 1 (_lift0 1 'accept))
       (_let 1
             'closure_1
             (_op 1 compute-closure state_1 grammar_1 k_1 first-map_1)
             (lambda (closure_1)
               (_let 0
                     'accept-items_1
                     (_op 1 accept closure_1)
                     (lambda (accept-items_1)
                       (shift-terminal_1 grammar_1
                                         k_1
                                         first-map_1
                                         continuations_1
                                         closure_1
                                         (_op 2 car input_1)
                                         (_op 2 cdr input_1)
                                         (_lambda_memo '1
                                                       '()
                                                       '1
                                                       (list accept-items_1
                                                             grammar_1
                                                             k_1
                                                             first-map_1
                                                             closure_1
                                                             continuations_1
                                                             input_1)
                                                       '(1 1 1 1 1 2 2)
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
                                                                                            (_lambda_memo '1
                                                                                                          '(item_1)
                                                                                                          '2
                                                                                                          (list grammar_1
                                                                                                                k_1
                                                                                                                first-map_1
                                                                                                                closure_1
                                                                                                                continuations_1
                                                                                                                input_1)
                                                                                                          '(1
                                                                                                            1
                                                                                                            1
                                                                                                            1
                                                                                                            2
                                                                                                            2)
                                                                                                          (lambda (grammar_1 k_1
                                                                                                                             first-map_1
                                                                                                                             closure_1
                                                                                                                             continuations_1
                                                                                                                             input_1)
                                                                                                            (lambda (item_1)
                                                                                                              (_let 1
                                                                                                                    'item_1
                                                                                                                    item_1
                                                                                                                    (lambda (item_1)
                                                                                                                      (_app 2
                                                                                                                            (_op 2
                                                                                                                                 list-ref
                                                                                                                                 (_op 2
                                                                                                                                      cons
                                                                                                                                      (multi-memo 2
                                                                                                                                                  'parse-8
                                                                                                                                                  parse-8
                                                                                                                                                  '(1
                                                                                                                                                    1
                                                                                                                                                    1
                                                                                                                                                    2
                                                                                                                                                    1)
                                                                                                                                                  (list grammar_1
                                                                                                                                                        k_1
                                                                                                                                                        first-map_1
                                                                                                                                                        continuations_1
                                                                                                                                                        closure_1))
                                                                                                                                      continuations_1)
                                                                                                                                 (_lift 1
                                                                                                                                        1
                                                                                                                                        (_op 1
                                                                                                                                             length
                                                                                                                                             (_op 1
                                                                                                                                                  item-rhs
                                                                                                                                                  item_1))))
                                                                                                                            (_lift 1
                                                                                                                                   1
                                                                                                                                   (_op 1
                                                                                                                                        item-lhs
                                                                                                                                        item_1))
                                                                                                                            input_1))))))
                                                                                            (_lambda_memo '1
                                                                                                          '()
                                                                                                          '4
                                                                                                          (list)
                                                                                                          '()
                                                                                                          (lambda ()
                                                                                                            (lambda ()
                                                                                                              (_lift0 1
                                                                                                                      'error)))))))))))))))
(define (parse-8 grammar_1 k_1 first-map_1 continuations_1 closure_1)
  (_lambda '2
           '(nonterminal input)
           (lambda (nonterminal input)
             (_let 1
                   'nonterminal
                   nonterminal
                   (lambda (nonterminal)
                     (_let 1
                           'input
                           input
                           (lambda (input)
                             (shift-nonterminal_1 grammar_1
                                                  k_1
                                                  first-map_1
                                                  continuations_1
                                                  closure_1
                                                  nonterminal
                                                  input))))))))
(define ($goal source-grammar_1 k_2 input_4)
  (do-parse source-grammar_1 k_2 input_4))
