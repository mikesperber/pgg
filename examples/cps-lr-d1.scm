(define ($goal-16 clone-3 clone-2 clone-1)
  (define (parse-7-17 clone-6 clone-5 clone-4 clone-3 clone-2 clone-1)
    (_if 0
         (final? clone-6 clone-5)
         (_if 1
              (_op 1 equal? (_lift0 1 '$) (_op 1 car clone-1))
              (_lift0 1 'accept)
              (let ((closure_1-7 (compute-closure clone-6
                                                  clone-5
                                                  clone-4
                                                  clone-3)))
                (_let 1
                      'continuations_1
                      clone-2
                      (lambda (continuations_1-39)
                        (let* ((terminal_1-41 (_op 1 car clone-1))
                               (input_2-42 (_op 1 cdr clone-1)))
                          (_let 1
                                'element_1
                                terminal_1-41
                                (lambda (element_1-66)
                                  (let ((set_2-68 (next-terminals closure_1-7
                                                                  clone-5)))
                                    (loop_1-12-21 (static-constructor '1
                                                                      (lambda (var-9 var-10
                                                                                     var-11
                                                                                     var-12
                                                                                     var-13
                                                                                     var-14
                                                                                     var-15)
                                                                        (lambda ()
                                                                          (_let 1
                                                                                'l_1
                                                                                clone-1
                                                                                (lambda (l_1-30)
                                                                                  (let ((input-front_1-31 (chop-9-20 clone-4
                                                                                                                     l_1-30)))
                                                                                    (_let 1
                                                                                          'input-front_1
                                                                                          input-front_1-31
                                                                                          (lambda (input-front_1-33)
                                                                                            (let ((item-set_2-35 (accept closure_1-7)))
                                                                                              (loop_2-10-23 (static-constructor '4
                                                                                                                                (lambda ()
                                                                                                                                  (lambda ()
                                                                                                                                    'error))
                                                                                                                                (list)
                                                                                                                                '())
                                                                                                            (static-constructor '2
                                                                                                                                (lambda (var-17 var-18
                                                                                                                                                var-19
                                                                                                                                                var-20
                                                                                                                                                var-21
                                                                                                                                                var-22)
                                                                                                                                  (lambda (item_1-16)
                                                                                                                                    (_app 1
                                                                                                                                          (_op 1
                                                                                                                                               list-ref
                                                                                                                                               (_op 1
                                                                                                                                                    cons
                                                                                                                                                    (multi-memo 1
                                                                                                                                                                'parse-8-18
                                                                                                                                                                parse-8-18
                                                                                                                                                                '(0
                                                                                                                                                                  0
                                                                                                                                                                  0
                                                                                                                                                                  0
                                                                                                                                                                  1)
                                                                                                                                                                (list clone-5
                                                                                                                                                                      clone-4
                                                                                                                                                                      clone-3
                                                                                                                                                                      closure_1-7
                                                                                                                                                                      clone-2))
                                                                                                                                                    clone-2)
                                                                                                                                               (_lift0 1
                                                                                                                                                       (length (item-rhs item_1-16))))
                                                                                                                                          (_lift0 1
                                                                                                                                                  (item-lhs item_1-16))
                                                                                                                                          clone-1)))
                                                                                                                                (list clone-5
                                                                                                                                      clone-4
                                                                                                                                      clone-3
                                                                                                                                      closure_1-7
                                                                                                                                      clone-2
                                                                                                                                      clone-1)
                                                                                                                                '(0
                                                                                                                                  0
                                                                                                                                  0
                                                                                                                                  0
                                                                                                                                  1
                                                                                                                                  1))
                                                                                                            item-set_2-35
                                                                                                            input-front_1-33)))))))))
                                                                      (list (accept closure_1-7)
                                                                            clone-5
                                                                            clone-4
                                                                            clone-3
                                                                            closure_1-7
                                                                            clone-2
                                                                            clone-1)
                                                                      '(0
                                                                        0
                                                                        0
                                                                        0
                                                                        0
                                                                        1
                                                                        1))
                                                  (static-constructor '8
                                                                      (lambda (var-45 var-46
                                                                                      var-47
                                                                                      var-48
                                                                                      var-49
                                                                                      var-50)
                                                                        (lambda (symbol_1-44)
                                                                          (let* ((next-state_1-52 (goto closure_1-7
                                                                                                        symbol_1-44))
                                                                                 (n_1-53 (- (active next-state_1-52)
                                                                                            1)))
                                                                            (_let 1
                                                                                  'l_1
                                                                                  continuations_1-39
                                                                                  (lambda (l_1-54)
                                                                                    (_let 1
                                                                                          'continuations_1
                                                                                          (_op 1
                                                                                               cons
                                                                                               (multi-memo 1
                                                                                                           'shift-terminal_1-15-25
                                                                                                           shift-terminal_1-15-25
                                                                                                           '(0
                                                                                                             0
                                                                                                             0
                                                                                                             0
                                                                                                             1)
                                                                                                           (list clone-5
                                                                                                                 clone-4
                                                                                                                 clone-3
                                                                                                                 closure_1-7
                                                                                                                 continuations_1-39))
                                                                                               (chop-9-20 n_1-53
                                                                                                          l_1-54))
                                                                                          (lambda (continuations_1-59)
                                                                                            (_let 1
                                                                                                  'input_1
                                                                                                  input_2-42
                                                                                                  (lambda (input_1-60)
                                                                                                    (multi-memo 1
                                                                                                                'parse-7-17
                                                                                                                parse-7-17
                                                                                                                '(0
                                                                                                                  0
                                                                                                                  0
                                                                                                                  0
                                                                                                                  1
                                                                                                                  1)
                                                                                                                (list next-state_1-52
                                                                                                                      clone-5
                                                                                                                      clone-4
                                                                                                                      clone-3
                                                                                                                      continuations_1-59
                                                                                                                      input_1-60)))))))))))
                                                                      (list clone-5
                                                                            clone-4
                                                                            clone-3
                                                                            closure_1-7
                                                                            continuations_1-39
                                                                            input_2-42)
                                                                      '(0
                                                                        0
                                                                        0
                                                                        0
                                                                        1
                                                                        1))
                                                  set_2-68
                                                  element_1-66)))))))))
         (_if 1
              (_lift0 1 #f)
              (_lift0 1 'accept)
              (let ((closure_1-69 (compute-closure clone-6
                                                   clone-5
                                                   clone-4
                                                   clone-3)))
                (_let 1
                      'continuations_1
                      clone-2
                      (lambda (continuations_1-101)
                        (let* ((terminal_1-103 (_op 1 car clone-1))
                               (input_2-104 (_op 1 cdr clone-1)))
                          (_let 1
                                'element_1
                                terminal_1-103
                                (lambda (element_1-128)
                                  (let ((set_2-130 (next-terminals closure_1-69
                                                                   clone-5)))
                                    (loop_1-12-21 (static-constructor '1
                                                                      (lambda (var-71 var-72
                                                                                      var-73
                                                                                      var-74
                                                                                      var-75
                                                                                      var-76
                                                                                      var-77)
                                                                        (lambda ()
                                                                          (_let 1
                                                                                'l_1
                                                                                clone-1
                                                                                (lambda (l_1-92)
                                                                                  (let ((input-front_1-93 (chop-9-20 clone-4
                                                                                                                     l_1-92)))
                                                                                    (_let 1
                                                                                          'input-front_1
                                                                                          input-front_1-93
                                                                                          (lambda (input-front_1-95)
                                                                                            (let ((item-set_2-97 (accept closure_1-69)))
                                                                                              (loop_2-10-23 (static-constructor '4
                                                                                                                                (lambda ()
                                                                                                                                  (lambda ()
                                                                                                                                    'error))
                                                                                                                                (list)
                                                                                                                                '())
                                                                                                            (static-constructor '2
                                                                                                                                (lambda (var-79 var-80
                                                                                                                                                var-81
                                                                                                                                                var-82
                                                                                                                                                var-83
                                                                                                                                                var-84)
                                                                                                                                  (lambda (item_1-78)
                                                                                                                                    (_app 1
                                                                                                                                          (_op 1
                                                                                                                                               list-ref
                                                                                                                                               (_op 1
                                                                                                                                                    cons
                                                                                                                                                    (multi-memo 1
                                                                                                                                                                'parse-8-18
                                                                                                                                                                parse-8-18
                                                                                                                                                                '(0
                                                                                                                                                                  0
                                                                                                                                                                  0
                                                                                                                                                                  0
                                                                                                                                                                  1)
                                                                                                                                                                (list clone-5
                                                                                                                                                                      clone-4
                                                                                                                                                                      clone-3
                                                                                                                                                                      closure_1-69
                                                                                                                                                                      clone-2))
                                                                                                                                                    clone-2)
                                                                                                                                               (_lift0 1
                                                                                                                                                       (length (item-rhs item_1-78))))
                                                                                                                                          (_lift0 1
                                                                                                                                                  (item-lhs item_1-78))
                                                                                                                                          clone-1)))
                                                                                                                                (list clone-5
                                                                                                                                      clone-4
                                                                                                                                      clone-3
                                                                                                                                      closure_1-69
                                                                                                                                      clone-2
                                                                                                                                      clone-1)
                                                                                                                                '(0
                                                                                                                                  0
                                                                                                                                  0
                                                                                                                                  0
                                                                                                                                  1
                                                                                                                                  1))
                                                                                                            item-set_2-97
                                                                                                            input-front_1-95)))))))))
                                                                      (list (accept closure_1-69)
                                                                            clone-5
                                                                            clone-4
                                                                            clone-3
                                                                            closure_1-69
                                                                            clone-2
                                                                            clone-1)
                                                                      '(0
                                                                        0
                                                                        0
                                                                        0
                                                                        0
                                                                        1
                                                                        1))
                                                  (static-constructor '8
                                                                      (lambda (var-107 var-108
                                                                                       var-109
                                                                                       var-110
                                                                                       var-111
                                                                                       var-112)
                                                                        (lambda (symbol_1-106)
                                                                          (let* ((next-state_1-114 (goto closure_1-69
                                                                                                         symbol_1-106))
                                                                                 (n_1-115 (- (active next-state_1-114)
                                                                                             1)))
                                                                            (_let 1
                                                                                  'l_1
                                                                                  continuations_1-101
                                                                                  (lambda (l_1-116)
                                                                                    (_let 1
                                                                                          'continuations_1
                                                                                          (_op 1
                                                                                               cons
                                                                                               (multi-memo 1
                                                                                                           'shift-terminal_1-15-25
                                                                                                           shift-terminal_1-15-25
                                                                                                           '(0
                                                                                                             0
                                                                                                             0
                                                                                                             0
                                                                                                             1)
                                                                                                           (list clone-5
                                                                                                                 clone-4
                                                                                                                 clone-3
                                                                                                                 closure_1-69
                                                                                                                 continuations_1-101))
                                                                                               (chop-9-20 n_1-115
                                                                                                          l_1-116))
                                                                                          (lambda (continuations_1-121)
                                                                                            (_let 1
                                                                                                  'input_1
                                                                                                  input_2-104
                                                                                                  (lambda (input_1-122)
                                                                                                    (multi-memo 1
                                                                                                                'parse-7-17
                                                                                                                parse-7-17
                                                                                                                '(0
                                                                                                                  0
                                                                                                                  0
                                                                                                                  0
                                                                                                                  1
                                                                                                                  1)
                                                                                                                (list next-state_1-114
                                                                                                                      clone-5
                                                                                                                      clone-4
                                                                                                                      clone-3
                                                                                                                      continuations_1-121
                                                                                                                      input_1-122)))))))))))
                                                                      (list clone-5
                                                                            clone-4
                                                                            clone-3
                                                                            closure_1-69
                                                                            continuations_1-101
                                                                            input_2-104)
                                                                      '(0
                                                                        0
                                                                        0
                                                                        0
                                                                        1
                                                                        1))
                                                  set_2-130
                                                  element_1-128)))))))))))
  (define (shift-terminal_1-15-25 clone-5 clone-4 clone-3 clone-1 clone-2)
    (_lambda 1
             '(nonterminal input)
             (lambda (nonterminal-6 input-7)
               (_let 1
                     'continuations_1
                     clone-2
                     (lambda (continuations_1-13)
                       (_let 1
                             'element_1
                             nonterminal-6
                             (lambda (element_1-39)
                               (let ((set_2-41 (next-nonterminals clone-1
                                                                  clone-5)))
                                 (loop_1-12-21 (static-constructor '7
                                                                   (lambda ()
                                                                     (lambda ()
                                                                       (_lift0 1
                                                                               'you-cannot-see-me)))
                                                                   (list)
                                                                   '())
                                               (static-constructor '5
                                                                   (lambda (var-18 var-19
                                                                                   var-20
                                                                                   var-21
                                                                                   var-22
                                                                                   var-23)
                                                                     (lambda (symbol_2-17)
                                                                       (let* ((next-state_2-25 (goto clone-1
                                                                                                     symbol_2-17))
                                                                              (n_1-26 (- (active next-state_2-25)
                                                                                         1)))
                                                                         (_let 1
                                                                               'l_1
                                                                               continuations_1-13
                                                                               (lambda (l_1-27)
                                                                                 (_let 1
                                                                                       'continuations_1
                                                                                       (_op 1
                                                                                            cons
                                                                                            (multi-memo 1
                                                                                                        'shift-nonterminal_1-14-19
                                                                                                        shift-nonterminal_1-14-19
                                                                                                        '(0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          1)
                                                                                                        (list clone-5
                                                                                                              clone-4
                                                                                                              clone-3
                                                                                                              clone-1
                                                                                                              continuations_1-13))
                                                                                            (chop-9-20 n_1-26
                                                                                                       l_1-27))
                                                                                       (lambda (continuations_1-32)
                                                                                         (_let 1
                                                                                               'input_1
                                                                                               input-7
                                                                                               (lambda (input_1-33)
                                                                                                 (multi-memo 1
                                                                                                             'parse-7-17
                                                                                                             parse-7-17
                                                                                                             '(0
                                                                                                               0
                                                                                                               0
                                                                                                               0
                                                                                                               1
                                                                                                               1)
                                                                                                             (list next-state_2-25
                                                                                                                   clone-5
                                                                                                                   clone-4
                                                                                                                   clone-3
                                                                                                                   continuations_1-32
                                                                                                                   input_1-33)))))))))))
                                                                   (list clone-5
                                                                         clone-4
                                                                         clone-3
                                                                         clone-1
                                                                         continuations_1-13
                                                                         input-7)
                                                                   '(0
                                                                     0
                                                                     0
                                                                     0
                                                                     1
                                                                     1))
                                               set_2-41
                                               element_1-39)))))))))
  (define (loop_2-10-23 clone-4 clone-2 clone-1 clone-3)
    (_if 0
         (null? clone-1)
         (_lift0 1 (_app_memo 0 clone-4))
         (let ((item_2-5 (car clone-1)))
           (multi-memo 1
                       'loop_2-11-24
                       loop_2-11-24
                       '(0 0 0 0 1)
                       (list item_2-5 clone-4 clone-2 clone-1 clone-3)))))
  (define (loop_2-11-24 clone-5 clone-4 clone-2 clone-1 clone-3)
    (_if 1
         (_op 1 equal? clone-3 (_lift0 1 (item-lookahead clone-5)))
         (_app_memo 0 clone-2 clone-5)
         (_let 1
               'input-front_1
               clone-3
               (lambda (input-front_1-7)
                 (let ((item-set_2-9 (cdr clone-1)))
                   (loop_2-10-23 clone-4
                                 clone-2
                                 item-set_2-9
                                 input-front_1-7))))))
  (define (parse-8-18 clone-5 clone-4 clone-3 clone-1 clone-2)
    (_lambda 1
             '(nonterminal input)
             (lambda (nonterminal-6 input-7)
               (_let 1
                     'continuations_1
                     clone-2
                     (lambda (continuations_1-13)
                       (_let 1
                             'element_1
                             nonterminal-6
                             (lambda (element_1-39)
                               (let ((set_2-41 (next-nonterminals clone-1
                                                                  clone-5)))
                                 (loop_1-12-21 (static-constructor '7
                                                                   (lambda ()
                                                                     (lambda ()
                                                                       (_lift0 1
                                                                               'you-cannot-see-me)))
                                                                   (list)
                                                                   '())
                                               (static-constructor '5
                                                                   (lambda (var-18 var-19
                                                                                   var-20
                                                                                   var-21
                                                                                   var-22
                                                                                   var-23)
                                                                     (lambda (symbol_2-17)
                                                                       (let* ((next-state_2-25 (goto clone-1
                                                                                                     symbol_2-17))
                                                                              (n_1-26 (- (active next-state_2-25)
                                                                                         1)))
                                                                         (_let 1
                                                                               'l_1
                                                                               continuations_1-13
                                                                               (lambda (l_1-27)
                                                                                 (_let 1
                                                                                       'continuations_1
                                                                                       (_op 1
                                                                                            cons
                                                                                            (multi-memo 1
                                                                                                        'shift-nonterminal_1-14-19
                                                                                                        shift-nonterminal_1-14-19
                                                                                                        '(0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          1)
                                                                                                        (list clone-5
                                                                                                              clone-4
                                                                                                              clone-3
                                                                                                              clone-1
                                                                                                              continuations_1-13))
                                                                                            (chop-9-20 n_1-26
                                                                                                       l_1-27))
                                                                                       (lambda (continuations_1-32)
                                                                                         (_let 1
                                                                                               'input_1
                                                                                               input-7
                                                                                               (lambda (input_1-33)
                                                                                                 (multi-memo 1
                                                                                                             'parse-7-17
                                                                                                             parse-7-17
                                                                                                             '(0
                                                                                                               0
                                                                                                               0
                                                                                                               0
                                                                                                               1
                                                                                                               1)
                                                                                                             (list next-state_2-25
                                                                                                                   clone-5
                                                                                                                   clone-4
                                                                                                                   clone-3
                                                                                                                   continuations_1-32
                                                                                                                   input_1-33)))))))))))
                                                                   (list clone-5
                                                                         clone-4
                                                                         clone-3
                                                                         clone-1
                                                                         continuations_1-13
                                                                         input-7)
                                                                   '(0
                                                                     0
                                                                     0
                                                                     0
                                                                     1
                                                                     1))
                                               set_2-41
                                               element_1-39)))))))))
  (define (shift-nonterminal_1-14-19 clone-5 clone-4 clone-3 clone-1 clone-2)
    (_lambda 1
             '(nonterminal input)
             (lambda (nonterminal-6 input-7)
               (_let 1
                     'continuations_1
                     clone-2
                     (lambda (continuations_1-13)
                       (_let 1
                             'element_1
                             nonterminal-6
                             (lambda (element_1-39)
                               (let ((set_2-41 (next-nonterminals clone-1
                                                                  clone-5)))
                                 (loop_1-12-21 (static-constructor '7
                                                                   (lambda ()
                                                                     (lambda ()
                                                                       (_lift0 1
                                                                               'you-cannot-see-me)))
                                                                   (list)
                                                                   '())
                                               (static-constructor '5
                                                                   (lambda (var-18 var-19
                                                                                   var-20
                                                                                   var-21
                                                                                   var-22
                                                                                   var-23)
                                                                     (lambda (symbol_2-17)
                                                                       (let* ((next-state_2-25 (goto clone-1
                                                                                                     symbol_2-17))
                                                                              (n_1-26 (- (active next-state_2-25)
                                                                                         1)))
                                                                         (_let 1
                                                                               'l_1
                                                                               continuations_1-13
                                                                               (lambda (l_1-27)
                                                                                 (_let 1
                                                                                       'continuations_1
                                                                                       (_op 1
                                                                                            cons
                                                                                            (multi-memo 1
                                                                                                        'shift-nonterminal_1-14-19
                                                                                                        shift-nonterminal_1-14-19
                                                                                                        '(0
                                                                                                          0
                                                                                                          0
                                                                                                          0
                                                                                                          1)
                                                                                                        (list clone-5
                                                                                                              clone-4
                                                                                                              clone-3
                                                                                                              clone-1
                                                                                                              continuations_1-13))
                                                                                            (chop-9-20 n_1-26
                                                                                                       l_1-27))
                                                                                       (lambda (continuations_1-32)
                                                                                         (_let 1
                                                                                               'input_1
                                                                                               input-7
                                                                                               (lambda (input_1-33)
                                                                                                 (multi-memo 1
                                                                                                             'parse-7-17
                                                                                                             parse-7-17
                                                                                                             '(0
                                                                                                               0
                                                                                                               0
                                                                                                               0
                                                                                                               1
                                                                                                               1)
                                                                                                             (list next-state_2-25
                                                                                                                   clone-5
                                                                                                                   clone-4
                                                                                                                   clone-3
                                                                                                                   continuations_1-32
                                                                                                                   input_1-33)))))))))))
                                                                   (list clone-5
                                                                         clone-4
                                                                         clone-3
                                                                         clone-1
                                                                         continuations_1-13
                                                                         input-7)
                                                                   '(0
                                                                     0
                                                                     0
                                                                     0
                                                                     1
                                                                     1))
                                               set_2-41
                                               element_1-39)))))))))
  (define (loop_1-12-21 clone-4 clone-2 clone-1 clone-3)
    (_if 0
         (null? clone-1)
         (_app_memo 0 clone-4)
         (multi-memo 1
                     'loop_1-13-22
                     loop_1-13-22
                     '(0 0 0 1)
                     (list clone-4 clone-2 clone-1 clone-3))))
  (define (loop_1-13-22 clone-4 clone-2 clone-1 clone-3)
    (_if 1
         (_op 1 equal? clone-3 (_lift0 1 (car clone-1)))
         (_app_memo 0 clone-2 (car clone-1))
         (_let 1
               'element_1
               clone-3
               (lambda (element_1-6)
                 (let ((set_2-8 (cdr clone-1)))
                   (loop_1-12-21 clone-4 clone-2 set_2-8 element_1-6))))))
  (define (chop-9-20 clone-2 clone-1)
    (_if 0
         (zero? clone-2)
         (_lift0 1 '())
         (let ((n_1-3 (- clone-2 1)))
           (_let 1
                 'l_1
                 (_op 1 cdr clone-1)
                 (lambda (l_1-4)
                   (_op 1 cons (_op 1 car clone-1) (chop-9-20 n_1-3 l_1-4)))))))
  (let* ((grammar_2-7 (source-grammar->grammar clone-3 clone-2))
         (start-production_1-8 (car (grammar-productions grammar_2-7)))
         (first-map_1-11 (compute-first grammar_2-7 clone-2))
         (state_1-12 (list (make-item start-production_1-8
                                      0
                                      (cdr (production-rhs start-production_1-8))))))
    (_let 1
          'continuations_1
          (_lift0 1 '())
          (lambda (continuations_1-13)
            (_let 1
                  'input_1
                  (_op 1 append clone-1 (_lift0 1 (make-$ clone-2)))
                  (lambda (input_1-14)
                    (multi-memo 1
                                'parse-7-17
                                parse-7-17
                                '(0 0 0 0 1 1)
                                (list state_1-12
                                      grammar_2-7
                                      clone-2
                                      first-map_1-11
                                      continuations_1-13
                                      input_1-14))))))))
