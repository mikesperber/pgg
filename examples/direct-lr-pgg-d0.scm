(defdata mylist (mynil) (mycons mycar mycdr))
(define (loop_1 xs_2 acc_1)
  (_let 1
        xs_2
        (lambda (xs_2)
          (_let 0
                acc_1
                (lambda (acc_1)
                  (multi-memo 1 'loop_1-29 '(1 1) (list xs_2 acc_1)))))))
(define (loop_1-29 xs_2 acc_1)
  (_if 1
       (_test_memo 1 'mynil? xs_2)
       (lambda ()
         acc_1)
       (lambda ()
         (loop_1 (_sel_memo 1 'mycdr xs_2)
                 (_ctor_memo 1
                             '(2 1)
                             'mycons
                             (_sel_memo 1 'mycar xs_2)
                             acc_1)))))
(define (loop_2 ch_1
                grammar_1
                k_1
                first-map_1
                closure_1
                p_3
                attribute-stack_1
                input_1
                ts_3)
  (_let 2
        ch_1
        (lambda (ch_1)
          (_let 0
                grammar_1
                (lambda (grammar_1)
                  (_let 0
                        k_1
                        (lambda (k_1)
                          (_let 0
                                first-map_1
                                (lambda (first-map_1)
                                  (_let 0
                                        closure_1
                                        (lambda (closure_1)
                                          (_let 1
                                                p_3
                                                (lambda (p_3)
                                                  (_let 1
                                                        attribute-stack_1
                                                        (lambda (attribute-stack_1)
                                                          (_let 1
                                                                input_1
                                                                (lambda (input_1)
                                                                  (_let 1
                                                                        ts_3
                                                                        (lambda (ts_3)
                                                                          (multi-memo 1
                                                                                      'loop_2-27
                                                                                      '(2
                                                                                        1
                                                                                        1
                                                                                        1
                                                                                        1
                                                                                        2
                                                                                        2
                                                                                        2
                                                                                        1)
                                                                                      (list ch_1
                                                                                            grammar_1
                                                                                            k_1
                                                                                            first-map_1
                                                                                            closure_1
                                                                                            p_3
                                                                                            attribute-stack_1
                                                                                            input_1
                                                                                            ts_3)))))))))))))))))))))
(define (loop_2-27 ch_1
                   grammar_1
                   k_1
                   first-map_1
                   closure_1
                   p_3
                   attribute-stack_1
                   input_1
                   ts_3)
  (_if 1
       (_op 1 'null? ts_3)
       (lambda ()
         (_op 2
              '_sim-error
              (_lift 1 1 (_lift0 1 'direct-parse))
              (_lift 1 1 (_lift0 1 '"can't shift on"))
              ch_1))
       (lambda ()
         (multi-memo 2
                     'loop_2-28
                     '(2 1 1 1 1 2 2 2 1)
                     (list ch_1
                           grammar_1
                           k_1
                           first-map_1
                           closure_1
                           p_3
                           attribute-stack_1
                           input_1
                           ts_3)))))
(define (loop_2-28 ch_1
                   grammar_1
                   k_1
                   first-map_1
                   closure_1
                   p_3
                   attribute-stack_1
                   input_1
                   ts_3)
  (_if 2
       (_op 2 'equal? (_lift 1 1 (_op 1 'car ts_3)) ch_1)
       (lambda ()
         (parse-bar grammar_1
                    k_1
                    first-map_1
                    closure_1
                    (_op 1 'car ts_3)
                    (input-attr p_3)
                    attribute-stack_1
                    (_op 2 'stream-cdr input_1)))
       (lambda ()
         (loop_2 ch_1
                 grammar_1
                 k_1
                 first-map_1
                 closure_1
                 p_3
                 attribute-stack_1
                 input_1
                 (_op 1 'cdr ts_3)))))
(define (loop_4 ch_2 la-set_1)
  (_let 2
        ch_2
        (lambda (ch_2)
          (_let 1
                la-set_1
                (lambda (la-set_1)
                  (_let 2
                        (_op 2 'equal? ch_2 (_lift 1 1 (_op 1 'car la-set_1)))
                        (lambda (xxx-1)
                          (multi-memo 2
                                      'loop_4-25
                                      '(2 2 1)
                                      (list xxx-1 ch_2 la-set_1)))))))))
(define (loop_4-25 xxx-1 ch_2 la-set_1)
  (_if 2
       xxx-1
       (lambda ()
         xxx-1)
       (lambda ()
         (multi-memo 1 'loop_4-26 '(2 1) (list ch_2 la-set_1)))))
(define (loop_4-26 ch_2 la-set_1)
  (_if 1
       (_op 1 'null? (_op 1 'cdr la-set_1))
       (lambda ()
         (_lift 1 1 (_lift0 1 '#f)))
       (lambda ()
         (loop_4 ch_2 (_op 1 'cdr la-set_1)))))
(define (inner-loop_1 ch_2
                      ts_2
                      grammar_1
                      k_1
                      first-map_1
                      closure_1
                      input_1
                      attribute-stack_1
                      pos_1
                      k_2
                      input_3
                      trie_2)
  (_let 2
        ch_2
        (lambda (ch_2)
          (_let 0
                ts_2
                (lambda (ts_2)
                  (_let 0
                        grammar_1
                        (lambda (grammar_1)
                          (_let 0
                                k_1
                                (lambda (k_1)
                                  (_let 0
                                        first-map_1
                                        (lambda (first-map_1)
                                          (_let 0
                                                closure_1
                                                (lambda (closure_1)
                                                  (_let 1
                                                        input_1
                                                        (lambda (input_1)
                                                          (_let 1
                                                                attribute-stack_1
                                                                (lambda (attribute-stack_1)
                                                                  (_let 1
                                                                        pos_1
                                                                        (lambda (pos_1)
                                                                          (_let 1
                                                                                k_2
                                                                                (lambda (k_2)
                                                                                  (_let 1
                                                                                        input_3
                                                                                        (lambda (input_3)
                                                                                          (_let 1
                                                                                                trie_2
                                                                                                (lambda (trie_2)
                                                                                                  (multi-memo 1
                                                                                                              'inner-loop_1-22
                                                                                                              '(2
                                                                                                                1
                                                                                                                1
                                                                                                                1
                                                                                                                1
                                                                                                                1
                                                                                                                2
                                                                                                                2
                                                                                                                1
                                                                                                                1
                                                                                                                2
                                                                                                                1)
                                                                                                              (list ch_2
                                                                                                                    ts_2
                                                                                                                    grammar_1
                                                                                                                    k_1
                                                                                                                    first-map_1
                                                                                                                    closure_1
                                                                                                                    input_1
                                                                                                                    attribute-stack_1
                                                                                                                    pos_1
                                                                                                                    k_2
                                                                                                                    input_3
                                                                                                                    trie_2)))))))))))))))))))))))))))
(define (inner-loop_1-22 ch_2
                         ts_2
                         grammar_1
                         k_1
                         first-map_1
                         closure_1
                         input_1
                         attribute-stack_1
                         pos_1
                         k_2
                         input_3
                         trie_2)
  (_if 1
       (_op 1 'null? trie_2)
       (lambda ()
         (continue_1 ts_2
                     grammar_1
                     k_1
                     first-map_1
                     closure_1
                     input_1
                     attribute-stack_1
                     (_lift0 1 '#f)))
       (lambda ()
         (multi-memo 2
                     'inner-loop_1-23
                     '(2 1 1 1 1 1 2 2 1 1 2 1)
                     (list ch_2
                           ts_2
                           grammar_1
                           k_1
                           first-map_1
                           closure_1
                           input_1
                           attribute-stack_1
                           pos_1
                           k_2
                           input_3
                           trie_2)))))
(define (inner-loop_1-23 ch_2
                         ts_2
                         grammar_1
                         k_1
                         first-map_1
                         closure_1
                         input_1
                         attribute-stack_1
                         pos_1
                         k_2
                         input_3
                         trie_2)
  (_if 2
       (loop_4 ch_2 (_op 1 'caar trie_2))
       (lambda ()
         (loop_3 ts_2
                 grammar_1
                 k_1
                 first-map_1
                 closure_1
                 input_1
                 attribute-stack_1
                 k_2
                 (_op 1 'cdar trie_2)
                 (_op 1 '+ pos_1 (_lift0 1 '1))
                 (multi-memo 1
                             'inner-loop_1-24
                             '(1 1 2)
                             (list pos_1 k_2 input_3))))
       (lambda ()
         (inner-loop_1 ch_2
                       ts_2
                       grammar_1
                       k_1
                       first-map_1
                       closure_1
                       input_1
                       attribute-stack_1
                       pos_1
                       k_2
                       input_3
                       (_op 1 'cdr trie_2)))))
(define (inner-loop_1-24 pos_1 k_2 input_3)
  (_if 1
       (_op 1 '= (_op 1 '+ pos_1 (_lift0 1 '1)) k_2)
       (lambda ()
         (_lift 1 1 (_lift0 1 '())))
       (lambda ()
         (_op 2 'stream-cdr input_3))))
(define (loop_3 ts_2
                grammar_1
                k_1
                first-map_1
                closure_1
                input_1
                attribute-stack_1
                k_2
                trie_1
                pos_1
                input_3)
  (_let 0
        ts_2
        (lambda (ts_2)
          (_let 0
                grammar_1
                (lambda (grammar_1)
                  (_let 0
                        k_1
                        (lambda (k_1)
                          (_let 0
                                first-map_1
                                (lambda (first-map_1)
                                  (_let 0
                                        closure_1
                                        (lambda (closure_1)
                                          (_let 1
                                                input_1
                                                (lambda (input_1)
                                                  (_let 1
                                                        attribute-stack_1
                                                        (lambda (attribute-stack_1)
                                                          (_let 1
                                                                k_2
                                                                (lambda (k_2)
                                                                  (_let 1
                                                                        trie_1
                                                                        (lambda (trie_1)
                                                                          (_let 1
                                                                                pos_1
                                                                                (lambda (pos_1)
                                                                                  (_let 2
                                                                                        input_3
                                                                                        (lambda (input_3)
                                                                                          (multi-memo 1
                                                                                                      'loop_3-20
                                                                                                      '(1
                                                                                                        1
                                                                                                        1
                                                                                                        1
                                                                                                        1
                                                                                                        2
                                                                                                        2
                                                                                                        1
                                                                                                        1
                                                                                                        2
                                                                                                        1)
                                                                                                      (list ts_2
                                                                                                            grammar_1
                                                                                                            k_1
                                                                                                            first-map_1
                                                                                                            closure_1
                                                                                                            input_1
                                                                                                            attribute-stack_1
                                                                                                            pos_1
                                                                                                            k_2
                                                                                                            input_3
                                                                                                            trie_1)))))))))))))))))))))))))
(define (loop_3-20 ts_2
                   grammar_1
                   k_1
                   first-map_1
                   closure_1
                   input_1
                   attribute-stack_1
                   pos_1
                   k_2
                   input_3
                   trie_1)
  (_if 1
       (_op 1 'null? trie_1)
       (lambda ()
         (continue_1 ts_2
                     grammar_1
                     k_1
                     first-map_1
                     closure_1
                     input_1
                     attribute-stack_1
                     (_lift0 1 '#f)))
       (lambda ()
         (multi-memo 1
                     'loop_3-21
                     '(1 1 1 1 1 2 2 1 1 2 1)
                     (list ts_2
                           grammar_1
                           k_1
                           first-map_1
                           closure_1
                           input_1
                           attribute-stack_1
                           pos_1
                           k_2
                           input_3
                           trie_1)))))
(define (loop_3-21 ts_2
                   grammar_1
                   k_1
                   first-map_1
                   closure_1
                   input_1
                   attribute-stack_1
                   pos_1
                   k_2
                   input_3
                   trie_1)
  (_if 1
       (_op 1 '= pos_1 k_2)
       (lambda ()
         (continue_1 ts_2
                     grammar_1
                     k_1
                     first-map_1
                     closure_1
                     input_1
                     attribute-stack_1
                     trie_1))
       (lambda ()
         (_let 1
               (_op 2 'stream-car input_3)
               (lambda (ch_2)
                 (inner-loop_1 ch_2
                               ts_2
                               grammar_1
                               k_1
                               first-map_1
                               closure_1
                               input_1
                               attribute-stack_1
                               pos_1
                               k_2
                               input_3
                               trie_1))))))
(define (continue_1 ts_2
                    grammar_1
                    k_1
                    first-map_1
                    closure_1
                    input_1
                    attribute-stack_1
                    production_1)
  (_let 0
        ts_2
        (lambda (ts_2)
          (_let 0
                grammar_1
                (lambda (grammar_1)
                  (_let 0
                        k_1
                        (lambda (k_1)
                          (_let 0
                                first-map_1
                                (lambda (first-map_1)
                                  (_let 0
                                        closure_1
                                        (lambda (closure_1)
                                          (_let 2
                                                input_1
                                                (lambda (input_1)
                                                  (_let 2
                                                        attribute-stack_1
                                                        (lambda (attribute-stack_1)
                                                          (_let 1
                                                                production_1
                                                                (lambda (production_1)
                                                                  (multi-memo 1
                                                                              'continue_1-18
                                                                              '(1
                                                                                1
                                                                                1
                                                                                1
                                                                                1
                                                                                1
                                                                                2
                                                                                2)
                                                                              (list ts_2
                                                                                    production_1
                                                                                    grammar_1
                                                                                    k_1
                                                                                    first-map_1
                                                                                    closure_1
                                                                                    attribute-stack_1
                                                                                    input_1)))))))))))))))))))
(define (continue_1-18 ts_2
                       production_1
                       grammar_1
                       k_1
                       first-map_1
                       closure_1
                       attribute-stack_1
                       input_1)
  (_if 1
       (_op 1 'not production_1)
       (lambda ()
         (_let 2
               (_op 2 'stream-car input_1)
               (lambda (p_3)
                 (_let 1
                       (input-char p_3)
                       (lambda (ch_1)
                         (loop_2 ch_1
                                 grammar_1
                                 k_1
                                 first-map_1
                                 closure_1
                                 p_3
                                 attribute-stack_1
                                 input_1
                                 ts_2))))))
       (lambda ()
         (_let 1
               (_op 1 'length (_op 1 'production-rhs production_1))
               (lambda (rhs-length_1)
                 (_let 0
                       (_op 1 'production-lhs production_1)
                       (lambda (lhs_1)
                         (_let 0
                               (_op 1 'production-attribution production_1)
                               (lambda (attribution_1)
                                 (_let 1
                                       (_op 2
                                            'apply
                                            (_eval 1 1 attribution_1)
                                            (my-list->list (reverse-mylist (top->mylist rhs-length_1
                                                                                        attribute-stack_1))))
                                       (lambda (attribute-value_1)
                                         (multi-memo 1
                                                     'continue_1-19
                                                     '(1 1 1 1 2 1 1 2 2)
                                                     (list grammar_1
                                                           k_1
                                                           first-map_1
                                                           closure_1
                                                           attribute-stack_1
                                                           lhs_1
                                                           rhs-length_1
                                                           attribute-value_1
                                                           input_1)))))))))))))
(define (continue_1-19 grammar_1
                       k_1
                       first-map_1
                       closure_1
                       attribute-stack_1
                       lhs_1
                       rhs-length_1
                       attribute-value_1
                       input_1)
  (_if 1
       (_op 1 'zero? rhs-length_1)
       (lambda ()
         (parse-bar grammar_1
                    k_1
                    first-map_1
                    closure_1
                    lhs_1
                    attribute-value_1
                    attribute-stack_1
                    input_1))
       (lambda ()
         (_op 2
              'parse-result
              (_lift 1 1 lhs_1)
              (_lift 1 1 rhs-length_1)
              attribute-value_1
              input_1))))
(define (select-lookahead-item_1 ts_2
                                 grammar_1
                                 k_1
                                 first-map_1
                                 closure_1
                                 input_1
                                 attribute-stack_1
                                 item-set_1
                                 k_2
                                 input_2)
  (_let 0
        ts_2
        (lambda (ts_2)
          (_let 0
                grammar_1
                (lambda (grammar_1)
                  (_let 0
                        k_1
                        (lambda (k_1)
                          (_let 0
                                first-map_1
                                (lambda (first-map_1)
                                  (_let 0
                                        closure_1
                                        (lambda (closure_1)
                                          (_let 1
                                                input_1
                                                (lambda (input_1)
                                                  (_let 1
                                                        attribute-stack_1
                                                        (lambda (attribute-stack_1)
                                                          (_let 0
                                                                item-set_1
                                                                (lambda (item-set_1)
                                                                  (_let 1
                                                                        k_2
                                                                        (lambda (k_2)
                                                                          (_let 1
                                                                                input_2
                                                                                (lambda (input_2)
                                                                                  (loop_3 ts_2
                                                                                          grammar_1
                                                                                          k_1
                                                                                          first-map_1
                                                                                          closure_1
                                                                                          input_1
                                                                                          attribute-stack_1
                                                                                          k_2
                                                                                          (_op 1
                                                                                               'items->trie
                                                                                               item-set_1
                                                                                               k_2)
                                                                                          (_lift0 1
                                                                                                  '0)
                                                                                          input_2))))))))))))))))))))))
(define (loop_5 ch_3
                grammar_1
                k_1
                first-map_1
                closure_1
                p_4
                attribute-stack_1
                input_1
                ts_4)
  (_let 2
        ch_3
        (lambda (ch_3)
          (_let 0
                grammar_1
                (lambda (grammar_1)
                  (_let 0
                        k_1
                        (lambda (k_1)
                          (_let 0
                                first-map_1
                                (lambda (first-map_1)
                                  (_let 0
                                        closure_1
                                        (lambda (closure_1)
                                          (_let 1
                                                p_4
                                                (lambda (p_4)
                                                  (_let 1
                                                        attribute-stack_1
                                                        (lambda (attribute-stack_1)
                                                          (_let 1
                                                                input_1
                                                                (lambda (input_1)
                                                                  (_let 1
                                                                        ts_4
                                                                        (lambda (ts_4)
                                                                          (multi-memo 1
                                                                                      'loop_5-16
                                                                                      '(2
                                                                                        1
                                                                                        1
                                                                                        1
                                                                                        1
                                                                                        2
                                                                                        2
                                                                                        2
                                                                                        1)
                                                                                      (list ch_3
                                                                                            grammar_1
                                                                                            k_1
                                                                                            first-map_1
                                                                                            closure_1
                                                                                            p_4
                                                                                            attribute-stack_1
                                                                                            input_1
                                                                                            ts_4)))))))))))))))))))))
(define (loop_5-16 ch_3
                   grammar_1
                   k_1
                   first-map_1
                   closure_1
                   p_4
                   attribute-stack_1
                   input_1
                   ts_4)
  (_if 1
       (_op 1 'null? ts_4)
       (lambda ()
         (_op 2
              '_sim-error
              (_lift 1 1 (_lift0 1 'direct-parse))
              (_lift 1 1 (_lift0 1 '"can't shift on"))
              ch_3))
       (lambda ()
         (multi-memo 2
                     'loop_5-17
                     '(2 1 1 1 1 2 2 2 1)
                     (list ch_3
                           grammar_1
                           k_1
                           first-map_1
                           closure_1
                           p_4
                           attribute-stack_1
                           input_1
                           ts_4)))))
(define (loop_5-17 ch_3
                   grammar_1
                   k_1
                   first-map_1
                   closure_1
                   p_4
                   attribute-stack_1
                   input_1
                   ts_4)
  (_if 2
       (_op 2 'equal? (_lift 1 1 (_op 1 'car ts_4)) ch_3)
       (lambda ()
         (parse-bar grammar_1
                    k_1
                    first-map_1
                    closure_1
                    (_op 1 'car ts_4)
                    (input-attr p_4)
                    attribute-stack_1
                    (_op 2 'stream-cdr input_1)))
       (lambda ()
         (loop_5 ch_3
                 grammar_1
                 k_1
                 first-map_1
                 closure_1
                 p_4
                 attribute-stack_1
                 input_1
                 (_op 1 'cdr ts_4)))))
(define (loop_6 the-lhs_1
                grammar_2
                k_3
                first-map_2
                closure_2
                the-att_1
                attribute-stack_2
                the-inp_1
                nts_2)
  (_let 2
        the-lhs_1
        (lambda (the-lhs_1)
          (_let 0
                grammar_2
                (lambda (grammar_2)
                  (_let 0
                        k_3
                        (lambda (k_3)
                          (_let 0
                                first-map_2
                                (lambda (first-map_2)
                                  (_let 0
                                        closure_2
                                        (lambda (closure_2)
                                          (_let 1
                                                the-att_1
                                                (lambda (the-att_1)
                                                  (_let 1
                                                        attribute-stack_2
                                                        (lambda (attribute-stack_2)
                                                          (_let 1
                                                                the-inp_1
                                                                (lambda (the-inp_1)
                                                                  (_let 1
                                                                        nts_2
                                                                        (lambda (nts_2)
                                                                          (multi-memo 1
                                                                                      'loop_6-14
                                                                                      '(2
                                                                                        1
                                                                                        1
                                                                                        1
                                                                                        1
                                                                                        2
                                                                                        2
                                                                                        2
                                                                                        1)
                                                                                      (list the-lhs_1
                                                                                            grammar_2
                                                                                            k_3
                                                                                            first-map_2
                                                                                            closure_2
                                                                                            the-att_1
                                                                                            attribute-stack_2
                                                                                            the-inp_1
                                                                                            nts_2)))))))))))))))))))))
(define (loop_6-14 the-lhs_1
                   grammar_2
                   k_3
                   first-map_2
                   closure_2
                   the-att_1
                   attribute-stack_2
                   the-inp_1
                   nts_2)
  (_if 1
       (_op 1 'null? (_op 1 'cdr nts_2))
       (lambda ()
         (parse-bar grammar_2
                    k_3
                    first-map_2
                    closure_2
                    (_op 1 'car nts_2)
                    the-att_1
                    attribute-stack_2
                    the-inp_1))
       (lambda ()
         (multi-memo 2
                     'loop_6-15
                     '(2 1 1 1 1 2 2 2 1)
                     (list the-lhs_1
                           grammar_2
                           k_3
                           first-map_2
                           closure_2
                           the-att_1
                           attribute-stack_2
                           the-inp_1
                           nts_2)))))
(define (loop_6-15 the-lhs_1
                   grammar_2
                   k_3
                   first-map_2
                   closure_2
                   the-att_1
                   attribute-stack_2
                   the-inp_1
                   nts_2)
  (_if 2
       (_op 2 'equal? (_lift 1 1 (_op 1 'car nts_2)) the-lhs_1)
       (lambda ()
         (parse-bar grammar_2
                    k_3
                    first-map_2
                    closure_2
                    (_op 1 'car nts_2)
                    the-att_1
                    attribute-stack_2
                    the-inp_1))
       (lambda ()
         (loop_6 the-lhs_1
                 grammar_2
                 k_3
                 first-map_2
                 closure_2
                 the-att_1
                 attribute-stack_2
                 the-inp_1
                 (_op 1 'cdr nts_2)))))
(define (direct-parse-main source-grammar_1 k_4 input_5)
  (_let 0
        source-grammar_1
        (lambda (source-grammar_1)
          (_let 1
                k_4
                (lambda (k_4)
                  (_let 1
                        input_5
                        (lambda (input_5)
                          (_let 1
                                (_op 1
                                     'source-grammar->grammar
                                     source-grammar_1
                                     k_4)
                                (lambda (grammar_3)
                                  (_let 1
                                        (_op 1
                                             'car
                                             (_op 1
                                                  'grammar-productions
                                                  grammar_3))
                                        (lambda (start-production_1)
                                          (direct-parse grammar_3
                                                        k_4
                                                        (_op 1
                                                             'compute-first
                                                             grammar_3
                                                             k_4)
                                                        (_op 1
                                                             'list
                                                             (_op 1
                                                                  'make-item
                                                                  start-production_1
                                                                  (_lift0 1 '0)
                                                                  (_op 1
                                                                       'cdr
                                                                       (_op 1
                                                                            'production-rhs
                                                                            start-production_1))))
                                                        (_lift 1
                                                               1
                                                               (_lift0 1
                                                                       '()))
                                                        input_5))))))))))))
(define (parse-bar grammar_2
                   k_3
                   first-map_2
                   closure_2
                   sym_1
                   attribute-value_2
                   attribute-stack_2
                   input_4)
  (_let 1
        grammar_2
        (lambda (grammar_2)
          (_let 1
                k_3
                (lambda (k_3)
                  (_let 1
                        first-map_2
                        (lambda (first-map_2)
                          (_let 1
                                closure_2
                                (lambda (closure_2)
                                  (_let 0
                                        sym_1
                                        (lambda (sym_1)
                                          (_let 1
                                                attribute-value_2
                                                (lambda (attribute-value_2)
                                                  (_let 2
                                                        attribute-stack_2
                                                        (lambda (attribute-stack_2)
                                                          (_let 1
                                                                input_4
                                                                (lambda (input_4)
                                                                  (_let 1
                                                                        (_op 1
                                                                             'goto
                                                                             closure_2
                                                                             sym_1)
                                                                        (lambda (next-state_1)
                                                                          (_let 2
                                                                                (direct-parse grammar_2
                                                                                              k_3
                                                                                              first-map_2
                                                                                              next-state_1
                                                                                              (push attribute-value_2
                                                                                                    attribute-stack_2)
                                                                                              input_4)
                                                                                (lambda (result_2)
                                                                                  (multi-memo 1
                                                                                              'parse-bar-11
                                                                                              '(1
                                                                                                2
                                                                                                1
                                                                                                1
                                                                                                1
                                                                                                1
                                                                                                2)
                                                                                              (list next-state_1
                                                                                                    result_2
                                                                                                    grammar_2
                                                                                                    k_3
                                                                                                    first-map_2
                                                                                                    closure_2
                                                                                                    attribute-stack_2)))))))))))))))))))))))
(define (parse-bar-11 next-state_1
                      result_2
                      grammar_2
                      k_3
                      first-map_2
                      closure_2
                      attribute-stack_2)
  (_if 1
       (_op 1 'final? next-state_1 grammar_2)
       (lambda ()
         result_2)
       (lambda ()
         (_let 1
               (_op 2 'result-lhs result_2)
               (lambda (the-lhs_1)
                 (_let 2
                       (_op 2 'result-dot result_2)
                       (lambda (the-dot_1)
                         (_let 1
                               (_op 2 'result-att result_2)
                               (lambda (the-att_1)
                                 (_let 1
                                       (_op 2 'result-inp result_2)
                                       (lambda (the-inp_1)
                                         (_let 1
                                               (_op 1
                                                    'next-nonterminals
                                                    closure_2
                                                    grammar_2)
                                               (lambda (nts_1)
                                                 (multi-memo 1
                                                             'parse-bar-12
                                                             '(2
                                                               2
                                                               1
                                                               1
                                                               1
                                                               1
                                                               2
                                                               2
                                                               2
                                                               1)
                                                             (list the-dot_1
                                                                   the-lhs_1
                                                                   grammar_2
                                                                   k_3
                                                                   first-map_2
                                                                   closure_2
                                                                   the-att_1
                                                                   attribute-stack_2
                                                                   the-inp_1
                                                                   nts_1)))))))))))))))
(define (parse-bar-12 the-dot_1
                      the-lhs_1
                      grammar_2
                      k_3
                      first-map_2
                      closure_2
                      the-att_1
                      attribute-stack_2
                      the-inp_1
                      nts_1)
  (_if 1
       (_op 1 'null? nts_1)
       (lambda ()
         (_op 2
              'parse-result
              the-lhs_1
              (_op 2 '- the-dot_1 (_lift 1 1 (_lift0 1 '1)))
              the-att_1
              the-inp_1))
       (lambda ()
         (multi-memo 2
                     'parse-bar-13
                     '(2 2 1 1 1 1 2 2 2 1)
                     (list the-dot_1
                           the-lhs_1
                           grammar_2
                           k_3
                           first-map_2
                           closure_2
                           the-att_1
                           attribute-stack_2
                           the-inp_1
                           nts_1)))))
(define (parse-bar-13 the-dot_1
                      the-lhs_1
                      grammar_2
                      k_3
                      first-map_2
                      closure_2
                      the-att_1
                      attribute-stack_2
                      the-inp_1
                      nts_1)
  (_if 2
       (_op 2 '< (_lift 1 1 (_lift0 1 '1)) the-dot_1)
       (lambda ()
         (_op 2
              'parse-result
              the-lhs_1
              (_op 2 '- the-dot_1 (_lift 1 1 (_lift0 1 '1)))
              the-att_1
              the-inp_1))
       (lambda ()
         (loop_6 the-lhs_1
                 grammar_2
                 k_3
                 first-map_2
                 closure_2
                 the-att_1
                 attribute-stack_2
                 the-inp_1
                 nts_1))))
(define (direct-parse grammar_1
                      k_1
                      first-map_1
                      state_1
                      attribute-stack_1
                      input_1)
  (_let 1
        grammar_1
        (lambda (grammar_1)
          (_let 1
                k_1
                (lambda (k_1)
                  (_let 1
                        first-map_1
                        (lambda (first-map_1)
                          (_let 1
                                state_1
                                (lambda (state_1)
                                  (_let 1
                                        attribute-stack_1
                                        (lambda (attribute-stack_1)
                                          (_let 2
                                                input_1
                                                (lambda (input_1)
                                                  (multi-memo 1
                                                              'direct-parse-7
                                                              '(1 1 1 2 1 2)
                                                              (list state_1
                                                                    grammar_1
                                                                    first-map_1
                                                                    attribute-stack_1
                                                                    k_1
                                                                    input_1)))))))))))))))
(define (direct-parse-7 state_1
                        grammar_1
                        first-map_1
                        attribute-stack_1
                        k_1
                        input_1)
  (_if 1
       (_op 1 'final? state_1 grammar_1)
       (lambda ()
         (multi-memo 2 'direct-parse-8 '(2 2) (list attribute-stack_1 input_1)))
       (lambda ()
         (_let 1
               (_op 1 'compute-closure state_1 grammar_1 k_1 first-map_1)
               (lambda (closure_1)
                 (_let 0
                       (_op 1 'next-terminals closure_1 grammar_1)
                       (lambda (ts_1)
                         (_let 0
                               (multi-memo 1
                                           'direct-parse-9
                                           '(1 1 1)
                                           (list state_1 grammar_1 ts_1))
                               (lambda (ts_2)
                                 (_let 1
                                       (_op 1 'accept closure_1)
                                       (lambda (accept-items_1)
                                         (multi-memo 1
                                                     'direct-parse-10
                                                     '(1 1 1 1 2 1 1 2)
                                                     (list ts_2
                                                           grammar_1
                                                           first-map_1
                                                           closure_1
                                                           attribute-stack_1
                                                           accept-items_1
                                                           k_1
                                                           input_1)))))))))))))
(define (direct-parse-10 ts_2
                         grammar_1
                         first-map_1
                         closure_1
                         attribute-stack_1
                         accept-items_1
                         k_1
                         input_1)
  (_if 1
       (_op 1 'null? accept-items_1)
       (lambda ()
         (_let 2
               (_op 2 'stream-car input_1)
               (lambda (p_4)
                 (_let 1
                       (input-char p_4)
                       (lambda (ch_3)
                         (loop_5 ch_3
                                 grammar_1
                                 k_1
                                 first-map_1
                                 closure_1
                                 p_4
                                 attribute-stack_1
                                 input_1
                                 ts_2))))))
       (lambda ()
         (select-lookahead-item_1 ts_2
                                  grammar_1
                                  k_1
                                  first-map_1
                                  closure_1
                                  input_1
                                  attribute-stack_1
                                  accept-items_1
                                  k_1
                                  input_1))))
(define (direct-parse-9 state_1 grammar_1 ts_1)
  (_if 1
       (_op 1 'final? state_1 grammar_1)
       (lambda ()
         (filter-2 (_lambda_memo '1
                                 '(t_1)
                                 '1
                                 (list)
                                 '()
                                 (lambda ()
                                   (lambda (t_1)
                                     (_let 0
                                           t_1
                                           (lambda (t_1)
                                             (_op 1
                                                  'not
                                                  (_op 1
                                                       'equal?
                                                       t_1
                                                       (_lift0 1 '$))))))))
                   ts_1))
       (lambda ()
         ts_1)))
(define (direct-parse-8 attribute-stack_1 input_1)
  (_if 2
       (_op 2
            'equal?
            (_lift 1 1 (_lift0 1 '$))
            (input-char (_op 2 'stream-car input_1)))
       (lambda ()
         (_op 2 'car attribute-stack_1))
       (lambda ()
         (_op 2
              '_sim-error
              (_lift 1 1 (_lift0 1 'direct-parse))
              (_lift 1 1 (_lift0 1 '"expecting eof"))
              (input-char (_op 2 'stream-car input_1))))))
(define (filter-20 pred?_2 rest_1 result_1)
  (_let 1
        pred?_2
        (lambda (pred?_2)
          (_let 1
                rest_1
                (lambda (rest_1)
                  (_let 0
                        result_1
                        (lambda (result_1)
                          (multi-memo 1
                                      'filter-20-5
                                      '(1 1 1)
                                      (list pred?_2 rest_1 result_1)))))))))
(define (filter-20-5 pred?_2 rest_1 result_1)
  (_if 1
       (_op 1 'null? rest_1)
       (lambda ()
         (_op 1 'reverse result_1))
       (lambda ()
         (filter-20 pred?_2
                    (_op 1 'cdr rest_1)
                    (multi-memo 1
                                'filter-20-6
                                '(1 1 1)
                                (list pred?_2 rest_1 result_1))))))
(define (filter-20-6 pred?_2 rest_1 result_1)
  (_if 1
       (_app_memo 1 pred?_2 (_op 1 'car rest_1))
       (lambda ()
         (_op 1 'cons (_op 1 'car rest_1) result_1))
       (lambda ()
         result_1)))
(define (filter-2 pred?_1 list_1)
  (_let 0
        pred?_1
        (lambda (pred?_1)
          (_let 0
                list_1
                (lambda (list_1)
                  (filter-20 pred?_1
                             list_1
                             (_lift0 1 '())))))))
(define (input-attr p_2)
  (_let 1
        p_2
        (lambda (p_2)
          (_op 2 'cdr p_2))))
(define (input-char p_1)
  (_let 1
        p_1
        (lambda (p_1)
          (_op 2 'car p_1))))
(define (my-list->list l_1)
  (_let 1
        l_1
        (lambda (l_1)
          (multi-memo 1 'my-list->list-4 '(1) (list l_1)))))
(define (my-list->list-4 l_1)
  (_if 1
       (_test_memo 1 'mynil? l_1)
       (lambda ()
         (_lift 1 1 (_lift0 1 '())))
       (lambda ()
         (_op 2
              'cons
              (_sel_memo 1 'mycar l_1)
              (my-list->list (_sel_memo 1 'mycdr l_1))))))
(define (reverse-mylist xs_1)
  (_let 0
        xs_1
        (lambda (xs_1)
          (loop_1 xs_1 (_ctor_memo 1 '() 'mynil)))))
(define (top->mylist n_1 stack_2)
  (_let 1
        n_1
        (lambda (n_1)
          (_let 2
                stack_2
                (lambda (stack_2)
                  (multi-memo 1 'top->mylist-2 '(1 2) (list n_1 stack_2)))))))
(define (top->mylist-2 n_1 stack_2)
  (_if 1
       (_op 1 'zero? n_1)
       (lambda ()
         (_ctor_memo 1 '() 'mynil))
       (lambda ()
         (multi-memo 1 'top->mylist-3 '(1 2) (list n_1 stack_2)))))
(define (top->mylist-3 n_1 stack_2)
  (_if 1
       (_op 1 '= n_1 (_lift0 1 '1))
       (lambda ()
         (_ctor_memo 1
                     '(2 1)
                     'mycons
                     (_op 2 'car stack_2)
                     (_ctor_memo 1 '() 'mynil)))
       (lambda ()
         (_ctor_memo 1
                     '(2 1)
                     'mycons
                     (_op 2 'car stack_2)
                     (top->mylist (_op 1 '- n_1 (_lift0 1 '1))
                                  (_op 2 'cdr stack_2))))))
(define (push elem_1 stack_1)
  (_let 1
        elem_1
        (lambda (elem_1)
          (_let 1
                stack_1
                (lambda (stack_1)
                  (_op 2 'cons elem_1 stack_1))))))
(define (empty-stack)
  (_lift0 1 '()))
(define ($goal source-grammar_1 k_4 input_5)
  (direct-parse-main source-grammar_1 k_4 input_5))
