(define ($goal-88 clone-3 clone-2 clone-1)
  (let ((var-7 (source-grammar->grammar clone-3 clone-2)))
    (let ((var-8 (car (grammar-productions var-7))))
      (let ((var-11 (compute-first var-7 clone-2)))
        (let ((var-12 (list (make-item var-8 0 (cdr (production-rhs var-8))))))
          (let ((var-13 (_lift0 1 '())))
            (_let 1
                  clone-1
                  (lambda (var-14)
                    (direct-parse-7-89 var-12
                                       var-7
                                       var-11
                                       clone-2
                                       var-13
                                       var-14)))))))))
(define (direct-parse-7-89 clone-6 clone-5 clone-4 clone-2 clone-3 clone-1)
  (if (final? clone-6 clone-5)
      (multi-memo 1 'direct-parse-8-90 '(1 1) (list clone-3 clone-1))
      (let ((var-7 (compute-closure clone-6 clone-5 clone-2 clone-4)))
        (let ((var-10 (accept var-7)))
          (direct-parse-10-94 (direct-parse-9-91 clone-6
                                                 clone-5
                                                 (next-terminals var-7 clone-5))
                              clone-5
                              clone-4
                              var-7
                              var-10
                              clone-2
                              clone-3
                              clone-1)))))
(define (direct-parse-10-94 clone-8
                            clone-7
                            clone-6
                            clone-5
                            clone-3
                            clone-2
                            clone-4
                            clone-1)
  (if (null? clone-3)
      (_let 1
            (_op 1 'stream-car clone-1)
            (lambda (var-9)
              (let ((var-11 (_op 1 'car var-9)))
                (_let 1
                      var-11
                      (lambda (var-12)
                        (loop_5-16-95 clone-7
                                      clone-2
                                      clone-6
                                      clone-5
                                      clone-8
                                      var-12
                                      var-9
                                      clone-4
                                      clone-1))))))
      (let ((var-39 (items->trie clone-3 clone-2)))
        (_let 1
              clone-1
              (lambda (var-41)
                (loop_3-20-102 clone-8
                               clone-7
                               clone-2
                               clone-6
                               clone-5
                               0
                               clone-2
                               var-39
                               clone-1
                               clone-4
                               var-41))))))
(define (loop_3-20-102 clone-11
                       clone-10
                       clone-9
                       clone-8
                       clone-7
                       clone-4
                       clone-3
                       clone-1
                       clone-6
                       clone-5
                       clone-2)
  (if (null? clone-1)
      (_let 1
            clone-6
            (lambda (var-17)
              (_let 1
                    clone-5
                    (lambda (var-18)
                      (continue_1-18-103 clone-11
                                         #f
                                         clone-10
                                         clone-9
                                         clone-8
                                         clone-7
                                         var-18
                                         var-17)))))
      (loop_3-21-111 clone-11
                     clone-10
                     clone-9
                     clone-8
                     clone-7
                     clone-4
                     clone-3
                     clone-1
                     clone-6
                     clone-5
                     clone-2)))
(define (loop_3-21-111 clone-11
                       clone-10
                       clone-9
                       clone-8
                       clone-7
                       clone-4
                       clone-3
                       clone-1
                       clone-6
                       clone-5
                       clone-2)
  (if (= clone-4 clone-3)
      (_let 1
            clone-6
            (lambda (var-17)
              (_let 1
                    clone-5
                    (lambda (var-18)
                      (continue_1-18-103 clone-11
                                         clone-1
                                         clone-10
                                         clone-9
                                         clone-8
                                         clone-7
                                         var-18
                                         var-17)))))
      (let ((var-20 (_op 1 'stream-car clone-2)))
        (_let 1
              var-20
              (lambda (var-21)
                (inner-loop_1-22-112 clone-11
                                     clone-10
                                     clone-9
                                     clone-8
                                     clone-7
                                     clone-4
                                     clone-3
                                     clone-1
                                     var-21
                                     clone-6
                                     clone-5
                                     clone-2))))))
(define (inner-loop_1-22-112 clone-11
                             clone-10
                             clone-9
                             clone-8
                             clone-7
                             clone-4
                             clone-3
                             clone-1
                             clone-12
                             clone-6
                             clone-5
                             clone-2)
  (if (null? clone-1)
      (_let 1
            clone-6
            (lambda (var-18)
              (_let 1
                    clone-5
                    (lambda (var-19)
                      (continue_1-18-103 clone-11
                                         #f
                                         clone-10
                                         clone-9
                                         clone-8
                                         clone-7
                                         var-19
                                         var-18)))))
      (multi-memo 1
                  'inner-loop_1-23-113
                  '(0 0 0 0 0 0 0 0 1 1 1 1)
                  (list clone-11
                        clone-10
                        clone-9
                        clone-8
                        clone-7
                        clone-4
                        clone-3
                        clone-1
                        clone-12
                        clone-6
                        clone-5
                        clone-2))))
(define (inner-loop_1-23-113 clone-11
                             clone-10
                             clone-9
                             clone-8
                             clone-7
                             clone-4
                             clone-3
                             clone-1
                             clone-12
                             clone-6
                             clone-5
                             clone-2)
  (_let 1
        clone-12
        (lambda (var-13)
          (let ((var-14 (caar clone-1)))
            (_let 1
                  (_op 1 'equal? var-13 (_lift0 1 (car var-14)))
                  (lambda (var-15)
                    (_if 1
                         (multi-memo 1
                                     'loop_4-25-114
                                     '(0 1 1)
                                     (list var-14 var-15 var-13))
                         (lambda ()
                           (let ((var-24 (cdar clone-1)))
                             (let ((var-25 (+ clone-4 1)))
                               (_let 1
                                     (inner-loop_1-24-116 clone-4
                                                          clone-3
                                                          clone-2)
                                     (lambda (var-26)
                                       (loop_3-20-102 clone-11
                                                      clone-10
                                                      clone-9
                                                      clone-8
                                                      clone-7
                                                      var-25
                                                      clone-3
                                                      var-24
                                                      clone-6
                                                      clone-5
                                                      var-26))))))
                         (lambda ()
                           (_let 1
                                 clone-12
                                 (lambda (var-27)
                                   (let ((var-38 (cdr clone-1)))
                                     (inner-loop_1-22-112 clone-11
                                                          clone-10
                                                          clone-9
                                                          clone-8
                                                          clone-7
                                                          clone-4
                                                          clone-3
                                                          var-38
                                                          var-27
                                                          clone-6
                                                          clone-5
                                                          clone-2))))))))))))
(define (inner-loop_1-24-116 clone-3 clone-2 clone-1)
  (if (= (+ clone-3 1) clone-2)
      (_lift0 1 '())
      (_op 1 'stream-cdr clone-1)))
(define (loop_4-25-114 clone-1 clone-3 clone-2)
  (_if 1
       clone-3
       (lambda ()
         clone-3)
       (lambda ()
         (loop_4-26-115 clone-1 clone-2))))
(define (loop_4-26-115 clone-1 clone-2)
  (if (null? (cdr clone-1))
      (_lift0 1 #f)
      (_let 1
            clone-2
            (lambda (var-3)
              (let ((var-4 (cdr clone-1)))
                (_let 1
                      (_op 1 'equal? var-3 (_lift0 1 (car var-4)))
                      (lambda (var-5)
                        (multi-memo 1
                                    'loop_4-25-114
                                    '(0 1 1)
                                    (list var-4 var-5 var-3)))))))))
(define (continue_1-18-103 clone-8
                           clone-7
                           clone-6
                           clone-5
                           clone-4
                           clone-3
                           clone-2
                           clone-1)
  (if (not clone-7)
      (_let 1
            (_op 1 'stream-car clone-1)
            (lambda (var-9)
              (let ((var-11 (_op 1 'car var-9)))
                (_let 1
                      var-11
                      (lambda (var-12)
                        (loop_2-27-104 clone-6
                                       clone-5
                                       clone-4
                                       clone-3
                                       clone-8
                                       var-12
                                       var-9
                                       clone-2
                                       clone-1))))))
      (let ((var-21 (length (production-rhs clone-7))))
        (_let 1
              clone-2
              (lambda (var-25)
                (let ((var-27 (top->mylist-2-106 var-21 var-25)))
                  (let ((var-29 (loop_1-29-108 var-27
                                               (static-constructor 'mynil
                                                                   mynil
                                                                   (list)
                                                                   '()))))
                    (let ((var-30 (_op 1
                                       'apply
                                       (production-attribution clone-7)
                                       (my-list->list-4-109 var-29))))
                      (continue_1-19-110 clone-6
                                         clone-5
                                         clone-4
                                         clone-3
                                         (production-lhs clone-7)
                                         var-21
                                         clone-2
                                         var-30
                                         clone-1)))))))))
(define (continue_1-19-110 clone-9
                           clone-8
                           clone-7
                           clone-6
                           clone-4
                           clone-3
                           clone-5
                           clone-2
                           clone-1)
  (if (zero? clone-3)
      (_let 1
            clone-5
            (lambda (var-16)
              (let ((var-18 (goto clone-6 clone-4)))
                (let ((var-25 (_op 1 'cons clone-2 var-16)))
                  (_let 1
                        clone-1
                        (lambda (var-26)
                          (_let 1
                                (direct-parse-7-89 var-18
                                                   clone-9
                                                   clone-7
                                                   clone-8
                                                   var-25
                                                   var-26)
                                (lambda (var-27)
                                  (parse-bar-11-97 var-18
                                                   clone-9
                                                   clone-8
                                                   clone-7
                                                   clone-6
                                                   var-27
                                                   var-16)))))))))
      (_op 1
           'parse-result
           (_lift0 1 clone-4)
           (_lift0 1 clone-3)
           clone-2
           clone-1)))
(define (my-list->list-4-109 clone-1)
  (if (mynil? (clone-1 'value))
      (_lift0 1 '())
      (let ((var-2 (mycdr (clone-1 'value))))
        (_op 1 'cons (mycar (clone-1 'value)) (my-list->list-4-109 var-2)))))
(define (loop_1-29-108 clone-2 clone-1)
  (if (mynil? (clone-2 'value))
      clone-1
      (let ((var-3 (mycdr (clone-2 'value))))
        (loop_1-29-108 var-3
                       (static-constructor 'mycons
                                           mycons
                                           (list (mycar (clone-2 'value))
                                                 clone-1)
                                           '(1 0))))))
(define (top->mylist-2-106 clone-2 clone-1)
  (if (zero? clone-2)
      (static-constructor 'mynil mynil (list) '())
      (top->mylist-3-107 clone-2 clone-1)))
(define (top->mylist-3-107 clone-2 clone-1)
  (if (= clone-2 1)
      (static-constructor 'mycons
                          mycons
                          (list (_op 1 'car clone-1)
                                (static-constructor 'mynil
                                                    mynil
                                                    (list)
                                                    '()))
                          '(1 0))
      (let ((var-3 (- clone-2 1)))
        (_let 1
              (_op 1 'cdr clone-1)
              (lambda (var-4)
                (static-constructor 'mycons
                                    mycons
                                    (list (_op 1 'car clone-1)
                                          (top->mylist-2-106 var-3 var-4))
                                    '(1 0)))))))
(define (loop_2-27-104 clone-8
                       clone-7
                       clone-6
                       clone-5
                       clone-1
                       clone-9
                       clone-4
                       clone-3
                       clone-2)
  (if (null? clone-1)
      (_op 1
           '_sim-error
           (_lift0 1 'direct-parse)
           (_lift0 1 "can't shift on")
           clone-9)
      (multi-memo 1
                  'loop_2-28-105
                  '(0 0 0 0 0 1 1 1 1)
                  (list clone-8
                        clone-7
                        clone-6
                        clone-5
                        clone-1
                        clone-9
                        clone-4
                        clone-3
                        clone-2))))
(define (loop_2-28-105 clone-8
                       clone-7
                       clone-6
                       clone-5
                       clone-1
                       clone-9
                       clone-4
                       clone-3
                       clone-2)
  (_if 1
       (_op 1 'equal? (_lift0 1 (car clone-1)) clone-9)
       (lambda ()
         (let ((var-16 (_op 1 'cdr clone-4)))
           (_let 1
                 clone-3
                 (lambda (var-17)
                   (let ((var-18 (_op 1 'stream-cdr clone-2)))
                     (let ((var-19 (goto clone-5 (car clone-1))))
                       (let ((var-26 (_op 1 'cons var-16 var-17)))
                         (_let 1
                               var-18
                               (lambda (var-27)
                                 (_let 1
                                       (direct-parse-7-89 var-19
                                                          clone-8
                                                          clone-6
                                                          clone-7
                                                          var-26
                                                          var-27)
                                       (lambda (var-28)
                                         (parse-bar-11-97 var-19
                                                          clone-8
                                                          clone-7
                                                          clone-6
                                                          clone-5
                                                          var-28
                                                          var-17))))))))))))
       (lambda ()
         (_let 1
               clone-9
               (lambda (var-29)
                 (let ((var-37 (cdr clone-1)))
                   (loop_2-27-104 clone-8
                                  clone-7
                                  clone-6
                                  clone-5
                                  var-37
                                  var-29
                                  clone-4
                                  clone-3
                                  clone-2)))))))
(define (loop_5-16-95 clone-8
                      clone-7
                      clone-6
                      clone-5
                      clone-1
                      clone-9
                      clone-4
                      clone-3
                      clone-2)
  (if (null? clone-1)
      (_op 1
           '_sim-error
           (_lift0 1 'direct-parse)
           (_lift0 1 "can't shift on")
           clone-9)
      (multi-memo 1
                  'loop_5-17-96
                  '(0 0 0 0 0 1 1 1 1)
                  (list clone-8
                        clone-7
                        clone-6
                        clone-5
                        clone-1
                        clone-9
                        clone-4
                        clone-3
                        clone-2))))
(define (loop_5-17-96 clone-8
                      clone-7
                      clone-6
                      clone-5
                      clone-1
                      clone-9
                      clone-4
                      clone-3
                      clone-2)
  (_if 1
       (_op 1 'equal? (_lift0 1 (car clone-1)) clone-9)
       (lambda ()
         (let ((var-16 (_op 1 'cdr clone-4)))
           (_let 1
                 clone-3
                 (lambda (var-17)
                   (let ((var-18 (_op 1 'stream-cdr clone-2)))
                     (let ((var-19 (goto clone-5 (car clone-1))))
                       (let ((var-26 (_op 1 'cons var-16 var-17)))
                         (_let 1
                               var-18
                               (lambda (var-27)
                                 (_let 1
                                       (direct-parse-7-89 var-19
                                                          clone-8
                                                          clone-6
                                                          clone-7
                                                          var-26
                                                          var-27)
                                       (lambda (var-28)
                                         (parse-bar-11-97 var-19
                                                          clone-8
                                                          clone-7
                                                          clone-6
                                                          clone-5
                                                          var-28
                                                          var-17))))))))))))
       (lambda ()
         (_let 1
               clone-9
               (lambda (var-29)
                 (let ((var-37 (cdr clone-1)))
                   (loop_5-16-95 clone-8
                                 clone-7
                                 clone-6
                                 clone-5
                                 var-37
                                 var-29
                                 clone-4
                                 clone-3
                                 clone-2)))))))
(define (parse-bar-11-97 clone-7
                         clone-5
                         clone-4
                         clone-3
                         clone-2
                         clone-6
                         clone-1)
  (if (final? clone-7 clone-5)
      clone-6
      (let ((var-8 (_op 1 'result-lhs clone-6)))
        (_let 1
              (_op 1 'result-dot clone-6)
              (lambda (var-9)
                (let ((var-10 (_op 1 'result-att clone-6)))
                  (let ((var-11 (_op 1 'result-inp clone-6)))
                    (let ((var-12 (next-nonterminals clone-2 clone-5)))
                      (parse-bar-12-98 clone-5
                                       clone-4
                                       clone-3
                                       clone-2
                                       var-12
                                       var-9
                                       var-8
                                       var-10
                                       clone-1
                                       var-11)))))))))
(define (parse-bar-12-98 clone-8
                         clone-7
                         clone-6
                         clone-5
                         clone-1
                         clone-10
                         clone-9
                         clone-4
                         clone-3
                         clone-2)
  (if (null? clone-1)
      (_op 1
           'parse-result
           clone-9
           (_op 1 '- clone-10 (_lift0 1 1))
           clone-4
           clone-2)
      (multi-memo 1
                  'parse-bar-13-99
                  '(0 0 0 0 0 1 1 1 1 1)
                  (list clone-8
                        clone-7
                        clone-6
                        clone-5
                        clone-1
                        clone-10
                        clone-9
                        clone-4
                        clone-3
                        clone-2))))
(define (parse-bar-13-99 clone-8
                         clone-7
                         clone-6
                         clone-5
                         clone-1
                         clone-10
                         clone-9
                         clone-4
                         clone-3
                         clone-2)
  (_if 1
       (_op 1 '< (_lift0 1 1) clone-10)
       (lambda ()
         (_op 1
              'parse-result
              clone-9
              (_op 1 '- clone-10 (_lift0 1 1))
              clone-4
              clone-2))
       (lambda ()
         (_let 1
               clone-9
               (lambda (var-11)
                 (loop_6-14-100 clone-8
                                clone-7
                                clone-6
                                clone-5
                                clone-1
                                var-11
                                clone-4
                                clone-3
                                clone-2))))))
(define (loop_6-14-100 clone-8
                       clone-7
                       clone-6
                       clone-5
                       clone-1
                       clone-9
                       clone-4
                       clone-3
                       clone-2)
  (if (null? (cdr clone-1))
      (_let 1
            clone-3
            (lambda (var-16)
              (let ((var-18 (goto clone-5 (car clone-1))))
                (let ((var-25 (_op 1 'cons clone-4 var-16)))
                  (_let 1
                        clone-2
                        (lambda (var-26)
                          (_let 1
                                (direct-parse-7-89 var-18
                                                   clone-8
                                                   clone-6
                                                   clone-7
                                                   var-25
                                                   var-26)
                                (lambda (var-27)
                                  (parse-bar-11-97 var-18
                                                   clone-8
                                                   clone-7
                                                   clone-6
                                                   clone-5
                                                   var-27
                                                   var-16)))))))))
      (multi-memo 1
                  'loop_6-15-101
                  '(0 0 0 0 0 1 1 1 1)
                  (list clone-8
                        clone-7
                        clone-6
                        clone-5
                        clone-1
                        clone-9
                        clone-4
                        clone-3
                        clone-2))))
(define (loop_6-15-101 clone-8
                       clone-7
                       clone-6
                       clone-5
                       clone-1
                       clone-9
                       clone-4
                       clone-3
                       clone-2)
  (_if 1
       (_op 1 'equal? (_lift0 1 (car clone-1)) clone-9)
       (lambda ()
         (_let 1
               clone-3
               (lambda (var-16)
                 (let ((var-18 (goto clone-5 (car clone-1))))
                   (let ((var-25 (_op 1 'cons clone-4 var-16)))
                     (_let 1
                           clone-2
                           (lambda (var-26)
                             (_let 1
                                   (direct-parse-7-89 var-18
                                                      clone-8
                                                      clone-6
                                                      clone-7
                                                      var-25
                                                      var-26)
                                   (lambda (var-27)
                                     (parse-bar-11-97 var-18
                                                      clone-8
                                                      clone-7
                                                      clone-6
                                                      clone-5
                                                      var-27
                                                      var-16))))))))))
       (lambda ()
         (_let 1
               clone-9
               (lambda (var-28)
                 (let ((var-36 (cdr clone-1)))
                   (loop_6-14-100 clone-8
                                  clone-7
                                  clone-6
                                  clone-5
                                  var-36
                                  var-28
                                  clone-4
                                  clone-3
                                  clone-2)))))))
(define (direct-parse-9-91 clone-3 clone-2 clone-1)
  (if (final? clone-3 clone-2)
      (let ((var-8 (static-constructor '1
                                       (lambda ()
                                         (lambda (t_1-4)
                                           (not (equal? t_1-4 '$))))
                                       (list)
                                       '())))
        (filter-20-5-92 var-8 clone-1 '()))
      clone-1))
(define (filter-20-5-92 clone-3 clone-2 clone-1)
  (if (null? clone-2)
      (reverse clone-1)
      (let ((var-5 (cdr clone-2)))
        (filter-20-5-92 clone-3
                        var-5
                        (filter-20-6-93 clone-3 clone-2 clone-1)))))
(define (filter-20-6-93 clone-3 clone-2 clone-1)
  (if ((clone-3 'value) (car clone-2))
      (cons (car clone-2) clone-1)
      clone-1))
(define (direct-parse-8-90 clone-2 clone-1)
  (let ((var-3 (_op 1 'stream-car clone-1)))
    (_if 1
         (_op 1 'equal? (_lift0 1 '$) (_op 1 'car var-3))
         (lambda ()
           (_op 1 'car clone-2))
         (lambda ()
           (let ((var-4 (_op 1 'stream-car clone-1)))
             (_op 1
                  '_sim-error
                  (_lift0 1 'direct-parse)
                  (_lift0 1 "expecting eof")
                  (_op 1 'car var-4)))))))
