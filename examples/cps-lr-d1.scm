(define ($goal-342 clone-3 clone-2 clone-1)
  (let ((grammar_2-7 (source-grammar->grammar clone-3 clone-2)))
    (let ((start-production_1-8 (car (grammar-productions grammar_2-7))))
      (let ((first-map_1-11 (compute-first grammar_2-7 clone-2)))
        (_let 1
              'state_1
              (_lift0 1
                      (list (make-item start-production_1-8
                                       0
                                       (cdr (production-rhs start-production_1-8)))))
              (lambda (state_1-12)
                (_let 1
                      'continuations_1
                      (_lift0 1 '())
                      (lambda (continuations_1-13)
                        (_let 1
                              'input_1
                              (_op 1 append clone-1 (_lift0 1 (make-$ clone-2)))
                              (lambda (input_1-14)
                                (multi-memo 1
                                            'parse-329-343
                                            '(0 0 0 1 1 1)
                                            (list grammar_2-7
                                                  clone-2
                                                  first-map_1-11
                                                  state_1-12
                                                  continuations_1-13
                                                  input_1-14))))))))))))
(define (parse-329-343 clone-5 clone-4 clone-3 clone-6 clone-2 clone-1)
  (_if 1
       (_if 1
            (_op 1 final? clone-6 (_lift0 1 clone-5))
            (_op 1 equal? (_lift0 1 '$) (_op 1 car clone-1))
            (_lift0 1 #f))
       (_lift0 1 'accept)
       (_let 1
             'closure_1
             (_op 1
                  compute-closure
                  clone-6
                  (_lift0 1 clone-5)
                  (_lift0 1 clone-4)
                  (_lift0 1 clone-3))
             (lambda (closure_1-7)
               (let ((accept-items_1-8 (_op 1 accept closure_1-7)))
                 (_let 1
                       'continuations_1
                       clone-2
                       (lambda (continuations_1-12)
                         (_let 1
                               'closure_1
                               closure_1-7
                               (lambda (closure_1-13)
                                 (let ((terminal_1-14 (_op 1 car clone-1)))
                                   (let ((input_2-15 (_op 1 cdr clone-1)))
                                     (let ((fail_1-16 (multi-memo 1
                                                                  'parse-330-344
                                                                  '(0
                                                                    0
                                                                    0
                                                                    1
                                                                    1
                                                                    1
                                                                    1)
                                                                  (list clone-5
                                                                        clone-4
                                                                        clone-3
                                                                        accept-items_1-8
                                                                        closure_1-7
                                                                        clone-2
                                                                        clone-1))))
                                       (let ((set_1-18 (_op 1
                                                            next-terminals
                                                            closure_1-13
                                                            (_lift0 1
                                                                    clone-5))))
                                         (let ((cont_1-19 (multi-memo 1
                                                                      'shift-terminal-340-357
                                                                      '(0
                                                                        0
                                                                        0
                                                                        1
                                                                        1
                                                                        1)
                                                                      (list clone-5
                                                                            clone-4
                                                                            clone-3
                                                                            closure_1-13
                                                                            continuations_1-12
                                                                            input_2-15))))
                                           (_let 1
                                                 'input-front_1
                                                 terminal_1-14
                                                 (lambda (input-front_1-22)
                                                   (_let 1
                                                         'item-set_2
                                                         set_1-18
                                                         (lambda (item-set_2-24)
                                                           (multi-memo 1
                                                                       'loop-334-354
                                                                       '(1
                                                                         1
                                                                         1
                                                                         1)
                                                                       (list fail_1-16
                                                                             input-front_1-22
                                                                             cont_1-19
                                                                             item-set_2-24))))))))))))))))))))
(define (shift-terminal-340-357 clone-6 clone-5 clone-4 clone-3 clone-2 clone-1)
  (_lambda_memo 1
                '(symbol_1)
                'cls-358
                (list clone-6 clone-5 clone-4 clone-3 clone-2 clone-1)
                '(0 0 0 1 1 1)
                (lambda (var-8 var-9 var-10 var-11 var-12 var-13)
                  (lambda (symbol_1-7)
                    (_let 1
                          'next-state_1
                          (_op 1 goto clone-3 symbol_1-7)
                          (lambda (next-state_1-15)
                            (_let 1
                                  'state_1
                                  next-state_1-15
                                  (lambda (state_1-21)
                                    (_let 1
                                          'continuations_1
                                          (_op 1
                                               cons
                                               (multi-memo 1
                                                           'shift-terminal-341-359
                                                           '(0 0 0 1 1)
                                                           (list clone-6
                                                                 clone-5
                                                                 clone-4
                                                                 clone-2
                                                                 clone-3))
                                               (_let 1
                                                     'n_1
                                                     (_op 1
                                                          -
                                                          (_op 1
                                                               active
                                                               next-state_1-15)
                                                          (_lift0 1 1))
                                                     (lambda (n_1-16)
                                                       (_let 1
                                                             'l_1
                                                             clone-2
                                                             (lambda (l_1-17)
                                                               (multi-memo 1
                                                                           'take-333-356
                                                                           '(1
                                                                             1)
                                                                           (list n_1-16
                                                                                 l_1-17)))))))
                                          (lambda (continuations_1-22)
                                            (_let 1
                                                  'input_1
                                                  clone-1
                                                  (lambda (input_1-23)
                                                    (multi-memo 1
                                                                'parse-329-343
                                                                '(0 0 0 1 1 1)
                                                                (list clone-6
                                                                      clone-5
                                                                      clone-4
                                                                      state_1-21
                                                                      continuations_1-22
                                                                      input_1-23))))))))))))))
(define (shift-terminal-341-359 clone-5 clone-4 clone-3 clone-2 clone-1)
  (_lambda_memo 1
                '(nonterminal input)
                'cls-360
                (list clone-5 clone-4 clone-3 clone-2 clone-1)
                '(0 0 0 1 1)
                (lambda (var-8 var-9 var-10 var-11 var-12)
                  (lambda (nonterminal-6 input-7)
                    (_let 1
                          'continuations_1
                          clone-2
                          (lambda (continuations_1-18)
                            (_let 1
                                  'closure_1
                                  clone-1
                                  (lambda (closure_1-19)
                                    (let ((set_1-23 (_op 1
                                                         next-nonterminals
                                                         closure_1-19
                                                         (_lift0 1 clone-5))))
                                      (let ((cont_1-24 (multi-memo 1
                                                                   'shift-nonterminal-338-350
                                                                   '(0
                                                                     0
                                                                     0
                                                                     1
                                                                     1
                                                                     1)
                                                                   (list clone-5
                                                                         clone-4
                                                                         clone-3
                                                                         closure_1-19
                                                                         continuations_1-18
                                                                         input-7))))
                                        (let ((fail_2-25 (_lift0 1
                                                                 (static-constructor '7
                                                                                     (lambda ()
                                                                                       (lambda ()
                                                                                         'you-cannot-see-me))
                                                                                     (list)
                                                                                     '()))))
                                          (_let 1
                                                'input-front_1
                                                nonterminal-6
                                                (lambda (input-front_1-27)
                                                  (_let 1
                                                        'item-set_2
                                                        set_1-23
                                                        (lambda (item-set_2-29)
                                                          (multi-memo 1
                                                                      'loop-334-354
                                                                      '(1 1 1 1)
                                                                      (list fail_2-25
                                                                            input-front_1-27
                                                                            cont_1-24
                                                                            item-set_2-29)))))))))))))))))
(define (parse-330-344 clone-6 clone-5 clone-4 clone-7 clone-3 clone-2 clone-1)
  (_lambda_memo 1
                '()
                'cls-345
                (list clone-6 clone-5 clone-4 clone-7 clone-3 clone-2 clone-1)
                '(0 0 0 1 1 1 1)
                (lambda (var-8 var-9 var-10 var-11 var-12 var-13 var-14)
                  (lambda ()
                    (let ((cont_2-18 (multi-memo 1
                                                 'parse-331-346
                                                 '(0 0 0 1 1 1)
                                                 (list clone-6
                                                       clone-5
                                                       clone-4
                                                       clone-3
                                                       clone-2
                                                       clone-1))))
                      (_let 1
                            'n_1
                            (_lift0 1 clone-5)
                            (lambda (n_1-20)
                              (_let 1
                                    'l_1
                                    clone-1
                                    (lambda (l_1-21)
                                      (let ((input-front_1-22 (multi-memo 1
                                                                          'take-333-356
                                                                          '(1 1)
                                                                          (list n_1-20
                                                                                l_1-21))))
                                        (let ((fail_3-23 (_lift0 1
                                                                 (static-constructor '4
                                                                                     (lambda ()
                                                                                       (lambda ()
                                                                                         'error))
                                                                                     (list)
                                                                                     '()))))
                                          (_let 1
                                                'input-front_1
                                                input-front_1-22
                                                (lambda (input-front_1-24)
                                                  (_let 1
                                                        'item-set_2
                                                        clone-7
                                                        (lambda (item-set_2-26)
                                                          (multi-memo 1
                                                                      'loop-334-354
                                                                      '(1 1 1 1)
                                                                      (list fail_3-23
                                                                            input-front_1-24
                                                                            cont_2-18
                                                                            item-set_2-26)))))))))))))))))
(define (parse-331-346 clone-6 clone-5 clone-4 clone-3 clone-2 clone-1)
  (_lambda_memo 1
                '(item_1)
                'cls-347
                (list clone-6 clone-5 clone-4 clone-3 clone-2 clone-1)
                '(0 0 0 1 1 1)
                (lambda (var-8 var-9 var-10 var-11 var-12 var-13)
                  (lambda (item_1-7)
                    (_let 1
                          'item_1
                          item_1-7
                          (lambda (item_1-14)
                            (_app_memo 1
                                       (_op 1
                                            list-ref
                                            (_op 1
                                                 cons
                                                 (multi-memo 1
                                                             'parse-332-348
                                                             '(0 0 0 1 1)
                                                             (list clone-6
                                                                   clone-5
                                                                   clone-4
                                                                   clone-2
                                                                   clone-3))
                                                 clone-2)
                                            (_op 1
                                                 length
                                                 (_op 1 item-rhs item_1-14)))
                                       (_op 1 item-lhs item_1-14)
                                       clone-1)))))))
(define (parse-332-348 clone-5 clone-4 clone-3 clone-2 clone-1)
  (_lambda_memo 1
                '(nonterminal input)
                'cls-349
                (list clone-5 clone-4 clone-3 clone-2 clone-1)
                '(0 0 0 1 1)
                (lambda (var-8 var-9 var-10 var-11 var-12)
                  (lambda (nonterminal-6 input-7)
                    (_let 1
                          'continuations_1
                          clone-2
                          (lambda (continuations_1-18)
                            (_let 1
                                  'closure_1
                                  clone-1
                                  (lambda (closure_1-19)
                                    (let ((set_1-23 (_op 1
                                                         next-nonterminals
                                                         closure_1-19
                                                         (_lift0 1 clone-5))))
                                      (let ((cont_1-24 (multi-memo 1
                                                                   'shift-nonterminal-338-350
                                                                   '(0
                                                                     0
                                                                     0
                                                                     1
                                                                     1
                                                                     1)
                                                                   (list clone-5
                                                                         clone-4
                                                                         clone-3
                                                                         closure_1-19
                                                                         continuations_1-18
                                                                         input-7))))
                                        (let ((fail_2-25 (_lift0 1
                                                                 (static-constructor '7
                                                                                     (lambda ()
                                                                                       (lambda ()
                                                                                         'you-cannot-see-me))
                                                                                     (list)
                                                                                     '()))))
                                          (_let 1
                                                'input-front_1
                                                nonterminal-6
                                                (lambda (input-front_1-27)
                                                  (_let 1
                                                        'item-set_2
                                                        set_1-23
                                                        (lambda (item-set_2-29)
                                                          (multi-memo 1
                                                                      'loop-334-354
                                                                      '(1 1 1 1)
                                                                      (list fail_2-25
                                                                            input-front_1-27
                                                                            cont_1-24
                                                                            item-set_2-29)))))))))))))))))
(define (shift-nonterminal-338-350 clone-6
                                   clone-5
                                   clone-4
                                   clone-3
                                   clone-2
                                   clone-1)
  (_lambda_memo 1
                '(symbol_2)
                'cls-351
                (list clone-6 clone-5 clone-4 clone-3 clone-2 clone-1)
                '(0 0 0 1 1 1)
                (lambda (var-8 var-9 var-10 var-11 var-12 var-13)
                  (lambda (symbol_2-7)
                    (_let 1
                          'next-state_2
                          (_op 1 goto clone-3 symbol_2-7)
                          (lambda (next-state_2-15)
                            (_let 1
                                  'state_1
                                  next-state_2-15
                                  (lambda (state_1-21)
                                    (_let 1
                                          'continuations_1
                                          (_op 1
                                               cons
                                               (multi-memo 1
                                                           'shift-nonterminal-339-352
                                                           '(0 0 0 1 1)
                                                           (list clone-6
                                                                 clone-5
                                                                 clone-4
                                                                 clone-2
                                                                 clone-3))
                                               (_let 1
                                                     'n_1
                                                     (_op 1
                                                          -
                                                          (_op 1
                                                               active
                                                               next-state_2-15)
                                                          (_lift0 1 1))
                                                     (lambda (n_1-16)
                                                       (_let 1
                                                             'l_1
                                                             clone-2
                                                             (lambda (l_1-17)
                                                               (multi-memo 1
                                                                           'take-333-356
                                                                           '(1
                                                                             1)
                                                                           (list n_1-16
                                                                                 l_1-17)))))))
                                          (lambda (continuations_1-22)
                                            (_let 1
                                                  'input_1
                                                  clone-1
                                                  (lambda (input_1-23)
                                                    (multi-memo 1
                                                                'parse-329-343
                                                                '(0 0 0 1 1 1)
                                                                (list clone-6
                                                                      clone-5
                                                                      clone-4
                                                                      state_1-21
                                                                      continuations_1-22
                                                                      input_1-23))))))))))))))
(define (take-333-356 clone-2 clone-1)
  (_if 1
       (_op 1 zero? clone-2)
       (_lift0 1 '())
       (_op 1
            cons
            (_op 1 car clone-1)
            (_let 1
                  'n_1
                  (_op 1 - clone-2 (_lift0 1 1))
                  (lambda (n_1-3)
                    (_let 1
                          'l_1
                          (_op 1 cdr clone-1)
                          (lambda (l_1-4)
                            (multi-memo 1
                                        'take-333-356
                                        '(1 1)
                                        (list n_1-3 l_1-4)))))))))
(define (shift-nonterminal-339-352 clone-5 clone-4 clone-3 clone-2 clone-1)
  (_lambda_memo 1
                '(nonterminal input)
                'cls-353
                (list clone-5 clone-4 clone-3 clone-2 clone-1)
                '(0 0 0 1 1)
                (lambda (var-8 var-9 var-10 var-11 var-12)
                  (lambda (nonterminal-6 input-7)
                    (_let 1
                          'continuations_1
                          clone-2
                          (lambda (continuations_1-18)
                            (_let 1
                                  'closure_1
                                  clone-1
                                  (lambda (closure_1-19)
                                    (let ((set_1-23 (_op 1
                                                         next-nonterminals
                                                         closure_1-19
                                                         (_lift0 1 clone-5))))
                                      (let ((cont_1-24 (multi-memo 1
                                                                   'shift-nonterminal-338-350
                                                                   '(0
                                                                     0
                                                                     0
                                                                     1
                                                                     1
                                                                     1)
                                                                   (list clone-5
                                                                         clone-4
                                                                         clone-3
                                                                         closure_1-19
                                                                         continuations_1-18
                                                                         input-7))))
                                        (let ((fail_2-25 (_lift0 1
                                                                 (static-constructor '7
                                                                                     (lambda ()
                                                                                       (lambda ()
                                                                                         'you-cannot-see-me))
                                                                                     (list)
                                                                                     '()))))
                                          (_let 1
                                                'input-front_1
                                                nonterminal-6
                                                (lambda (input-front_1-27)
                                                  (_let 1
                                                        'item-set_2
                                                        set_1-23
                                                        (lambda (item-set_2-29)
                                                          (multi-memo 1
                                                                      'loop-334-354
                                                                      '(1 1 1 1)
                                                                      (list fail_2-25
                                                                            input-front_1-27
                                                                            cont_1-24
                                                                            item-set_2-29)))))))))))))))))
(define (loop-334-354 clone-4 clone-3 clone-2 clone-1)
  (_if 1
       (_op 1 null? clone-1)
       (_app_memo 1 clone-4)
       (_let 1
             'item_2
             (_op 1 car clone-1)
             (lambda (item_2-5)
               (multi-memo 1
                           'loop-335-355
                           '(1 1 1 1 1)
                           (list item_2-5 clone-4 clone-3 clone-2 clone-1))))))
(define (loop-335-355 clone-5 clone-4 clone-3 clone-2 clone-1)
  (_if 1
       (_op 1 equal? clone-3 (_op 1 item-lookahead clone-5))
       (_app_memo 1 clone-2 clone-5)
       (_let 1
             'input-front_1
             clone-3
             (lambda (input-front_1-7)
               (_let 1
                     'item-set_2
                     (_op 1 cdr clone-1)
                     (lambda (item-set_2-9)
                       (multi-memo 1
                                   'loop-334-354
                                   '(1 1 1 1)
                                   (list clone-4
                                         input-front_1-7
                                         clone-2
                                         item-set_2-9))))))))
