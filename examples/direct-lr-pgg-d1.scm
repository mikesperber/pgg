(define ($goal-35 clone-38 clone-37 clone-36)
  (let ((var-42 (source-grammar->grammar clone-38 clone-37)))
    (let ((var-43 (car (grammar-productions var-42))))
      (let ((var-46 (compute-first var-42 clone-37)))
        (let ((var-47 (list (make-item var-43
                                       0
                                       (cdr (production-rhs var-43))))))
          (let ((var-48 (_lift0 1 '())))
            (_let 1
                  clone-36
                  (lambda (var-49)
                    (direct-parse-12-50 var-47
                                        var-42
                                        var-46
                                        clone-37
                                        var-48
                                        var-49)))))))))
(define (direct-parse-12-50 clone-56
                            clone-55
                            clone-54
                            clone-52
                            clone-53
                            clone-51)
  (if (final? clone-56 clone-55)
      (multi-memo 1 'direct-parse-13-57 '(1 1) (list clone-53 clone-51))
      (let ((var-62 (compute-closure clone-56 clone-55 clone-52 clone-54)))
        (let ((var-87 (accept var-62)))
          (direct-parse-15-88 (direct-parse-14-64 clone-56
                                                  clone-55
                                                  (next-terminals var-62
                                                                  clone-55))
                              clone-55
                              clone-54
                              var-62
                              var-87
                              clone-52
                              clone-53
                              clone-51)))))
(define (direct-parse-15-88 clone-96
                            clone-95
                            clone-94
                            clone-93
                            clone-91
                            clone-90
                            clone-92
                            clone-89)
  (if (null? clone-91)
      (_let 1
            (_op 1 'stream-car clone-89)
            (lambda (var-97)
              (let ((var-99 (_op 1 'car var-97)))
                (_let 1
                      var-99
                      (lambda (var-100)
                        (loop_5-21-109 clone-95
                                       clone-90
                                       clone-94
                                       clone-93
                                       clone-96
                                       var-100
                                       var-97
                                       clone-92
                                       clone-89))))))
      (let ((var-284 (items->trie clone-91 clone-90)))
        (_let 1
              clone-89
              (lambda (var-286)
                (loop_3-25-287 clone-96
                               clone-95
                               clone-90
                               clone-94
                               clone-93
                               0
                               clone-90
                               var-284
                               clone-89
                               clone-92
                               var-286))))))
(define (loop_3-25-287 clone-298
                       clone-297
                       clone-296
                       clone-295
                       clone-294
                       clone-291
                       clone-290
                       clone-288
                       clone-293
                       clone-292
                       clone-289)
  (if (null? clone-288)
      (_let 1
            clone-293
            (lambda (var-304)
              (_let 1
                    clone-292
                    (lambda (var-305)
                      (continue_1-23-307 clone-298
                                         #f
                                         clone-297
                                         clone-296
                                         clone-295
                                         clone-294
                                         var-305
                                         var-304)))))
      (loop_3-26-430 clone-298
                     clone-297
                     clone-296
                     clone-295
                     clone-294
                     clone-291
                     clone-290
                     clone-288
                     clone-293
                     clone-292
                     clone-289)))
(define (loop_3-26-430 clone-441
                       clone-440
                       clone-439
                       clone-438
                       clone-437
                       clone-434
                       clone-433
                       clone-431
                       clone-436
                       clone-435
                       clone-432)
  (if (= clone-434 clone-433)
      (_let 1
            clone-436
            (lambda (var-447)
              (_let 1
                    clone-435
                    (lambda (var-448)
                      (continue_1-23-307 clone-441
                                         clone-431
                                         clone-440
                                         clone-439
                                         clone-438
                                         clone-437
                                         var-448
                                         var-447)))))
      (let ((var-450 (_op 1 'stream-car clone-432)))
        (_let 1
              var-450
              (lambda (var-451)
                (inner-loop_1-27-463 clone-441
                                     clone-440
                                     clone-439
                                     clone-438
                                     clone-437
                                     clone-434
                                     clone-433
                                     clone-431
                                     var-451
                                     clone-436
                                     clone-435
                                     clone-432))))))
(define (inner-loop_1-27-463 clone-474
                             clone-473
                             clone-472
                             clone-471
                             clone-470
                             clone-467
                             clone-466
                             clone-464
                             clone-475
                             clone-469
                             clone-468
                             clone-465)
  (if (null? clone-464)
      (_let 1
            clone-469
            (lambda (var-481)
              (_let 1
                    clone-468
                    (lambda (var-482)
                      (continue_1-23-307 clone-474
                                         #f
                                         clone-473
                                         clone-472
                                         clone-471
                                         clone-470
                                         var-482
                                         var-481)))))
      (multi-memo 1
                  'inner-loop_1-28-484
                  '(0 0 0 0 0 0 0 0 1 1 1 1)
                  (list clone-474
                        clone-473
                        clone-472
                        clone-471
                        clone-470
                        clone-467
                        clone-466
                        clone-464
                        clone-475
                        clone-469
                        clone-468
                        clone-465))))
(define (inner-loop_1-28-484 clone-495
                             clone-494
                             clone-493
                             clone-492
                             clone-491
                             clone-488
                             clone-487
                             clone-485
                             clone-496
                             clone-490
                             clone-489
                             clone-486)
  (_let 1
        clone-496
        (lambda (var-497)
          (let ((var-498 (caar clone-485)))
            (_let 1
                  (_op 1 'equal? var-497 (_lift0 1 (car var-498)))
                  (lambda (var-499)
                    (_if 1
                         (multi-memo 1
                                     'loop_4-30-500
                                     '(0 1 1)
                                     (list var-498 var-499 var-497))
                         (lambda ()
                           (let ((var-522 (cdar clone-485)))
                             (let ((var-523 (+ clone-488 1)))
                               (_let 1
                                     (inner-loop_1-29-510 clone-488
                                                          clone-487
                                                          clone-486)
                                     (lambda (var-524)
                                       (loop_3-25-287 clone-495
                                                      clone-494
                                                      clone-493
                                                      clone-492
                                                      clone-491
                                                      var-523
                                                      clone-487
                                                      var-522
                                                      clone-490
                                                      clone-489
                                                      var-524))))))
                         (lambda ()
                           (_let 1
                                 clone-496
                                 (lambda (var-525)
                                   (let ((var-536 (cdr clone-485)))
                                     (inner-loop_1-27-463 clone-495
                                                          clone-494
                                                          clone-493
                                                          clone-492
                                                          clone-491
                                                          clone-488
                                                          clone-487
                                                          var-536
                                                          var-525
                                                          clone-490
                                                          clone-489
                                                          clone-486))))))))))))
(define (inner-loop_1-29-510 clone-513 clone-512 clone-511)
  (if (= (+ clone-513 1) clone-512)
      (_lift0 1 '())
      (_op 1 'stream-cdr clone-511)))
(define (loop_4-30-500 clone-501 clone-503 clone-502)
  (_if 1
       clone-503
       (lambda ()
         clone-503)
       (lambda ()
         (loop_4-31-504 clone-501 clone-502))))
(define (loop_4-31-504 clone-505 clone-506)
  (if (null? (cdr clone-505))
      (_lift0 1 #f)
      (_let 1
            clone-506
            (lambda (var-507)
              (let ((var-508 (cdr clone-505)))
                (_let 1
                      (_op 1 'equal? var-507 (_lift0 1 (car var-508)))
                      (lambda (var-509)
                        (multi-memo 1
                                    'loop_4-30-500
                                    '(0 1 1)
                                    (list var-508 var-509 var-507)))))))))
(define (continue_1-23-307 clone-315
                           clone-314
                           clone-313
                           clone-312
                           clone-311
                           clone-310
                           clone-309
                           clone-308)
  (if (not clone-314)
      (_let 1
            (_op 1 'stream-car clone-308)
            (lambda (var-316)
              (let ((var-318 (_op 1 'car var-316)))
                (_let 1
                      var-318
                      (lambda (var-319)
                        (loop_2-32-328 clone-313
                                       clone-312
                                       clone-311
                                       clone-310
                                       clone-315
                                       var-319
                                       var-316
                                       clone-309
                                       clone-308))))))
      (let ((var-376 (length (production-rhs clone-314))))
        (_let 1
              clone-309
              (lambda (var-380)
                (let ((var-390 (top->mylist-7-381 var-376 var-380)))
                  (let ((var-397 (loop_1-34-392 var-390
                                                (static-constructor 'mynil
                                                                    mynil
                                                                    (list)
                                                                    '()))))
                    (let ((var-401 (_op 1
                                        'apply
                                        (production-attribution clone-314)
                                        (my-list->list-9-398 var-397))))
                      (continue_1-24-402 clone-313
                                         clone-312
                                         clone-311
                                         clone-310
                                         (production-lhs clone-314)
                                         var-376
                                         clone-309
                                         var-401
                                         clone-308)))))))))
(define (continue_1-24-402 clone-411
                           clone-410
                           clone-409
                           clone-408
                           clone-406
                           clone-405
                           clone-407
                           clone-404
                           clone-403)
  (if (zero? clone-405)
      (_let 1
            clone-407
            (lambda (var-418)
              (let ((var-420 (goto clone-408 clone-406)))
                (let ((var-427 (_op 1 'cons clone-404 var-418)))
                  (_let 1
                        clone-403
                        (lambda (var-428)
                          (_let 1
                                (direct-parse-12-50 var-420
                                                    clone-411
                                                    clone-409
                                                    clone-410
                                                    var-427
                                                    var-428)
                                (lambda (var-429)
                                  (parse-bar-16-148 var-420
                                                    clone-411
                                                    clone-410
                                                    clone-409
                                                    clone-408
                                                    var-429
                                                    var-418)))))))))
      (_op 1
           'parse-result
           (_lift0 1 clone-406)
           (_lift0 1 clone-405)
           clone-404
           clone-403)))
(define (my-list->list-9-398 clone-399)
  (if (mynil? (clone-399 'value))
      (_lift0 1 '())
      (let ((var-400 (mycdr (clone-399 'value))))
        (_op 1
             'cons
             (mycar (clone-399 'value))
             (my-list->list-9-398 var-400)))))
(define (loop_1-34-392 clone-394 clone-393)
  (if (mynil? (clone-394 'value))
      clone-393
      (let ((var-395 (mycdr (clone-394 'value))))
        (loop_1-34-392 var-395
                       (static-constructor 'mycons
                                           mycons
                                           (list (mycar (clone-394 'value))
                                                 clone-393)
                                           '(1 0))))))
(define (top->mylist-7-381 clone-383 clone-382)
  (if (zero? clone-383)
      (static-constructor 'mynil mynil (list) '())
      (top->mylist-8-384 clone-383 clone-382)))
(define (top->mylist-8-384 clone-386 clone-385)
  (if (= clone-386 1)
      (static-constructor 'mycons
                          mycons
                          (list (_op 1 'car clone-385)
                                (static-constructor 'mynil
                                                    mynil
                                                    (list)
                                                    '()))
                          '(1 0))
      (let ((var-387 (- clone-386 1)))
        (_let 1
              (_op 1 'cdr clone-385)
              (lambda (var-388)
                (static-constructor 'mycons
                                    mycons
                                    (list (_op 1 'car clone-385)
                                          (top->mylist-7-381 var-387 var-388))
                                    '(1 0)))))))
(define (loop_2-32-328 clone-336
                       clone-335
                       clone-334
                       clone-333
                       clone-329
                       clone-337
                       clone-332
                       clone-331
                       clone-330)
  (if (null? clone-329)
      (_op 1
           '_sim-error
           (_lift0 1 'direct-parse)
           (_lift0 1 "can't shift on")
           clone-337)
      (multi-memo 1
                  'loop_2-33-338
                  '(0 0 0 0 0 1 1 1 1)
                  (list clone-336
                        clone-335
                        clone-334
                        clone-333
                        clone-329
                        clone-337
                        clone-332
                        clone-331
                        clone-330))))
(define (loop_2-33-338 clone-346
                       clone-345
                       clone-344
                       clone-343
                       clone-339
                       clone-347
                       clone-342
                       clone-341
                       clone-340)
  (_if 1
       (_op 1 'equal? (_lift0 1 (car clone-339)) clone-347)
       (lambda ()
         (let ((var-354 (_op 1 'cdr clone-342)))
           (_let 1
                 clone-341
                 (lambda (var-355)
                   (let ((var-356 (_op 1 'stream-cdr clone-340)))
                     (let ((var-357 (goto clone-343 (car clone-339))))
                       (let ((var-364 (_op 1 'cons var-354 var-355)))
                         (_let 1
                               var-356
                               (lambda (var-365)
                                 (_let 1
                                       (direct-parse-12-50 var-357
                                                           clone-346
                                                           clone-344
                                                           clone-345
                                                           var-364
                                                           var-365)
                                       (lambda (var-366)
                                         (parse-bar-16-148 var-357
                                                           clone-346
                                                           clone-345
                                                           clone-344
                                                           clone-343
                                                           var-366
                                                           var-355))))))))))))
       (lambda ()
         (_let 1
               clone-347
               (lambda (var-367)
                 (let ((var-375 (cdr clone-339)))
                   (loop_2-32-328 clone-346
                                  clone-345
                                  clone-344
                                  clone-343
                                  var-375
                                  var-367
                                  clone-342
                                  clone-341
                                  clone-340)))))))
(define (loop_5-21-109 clone-117
                       clone-116
                       clone-115
                       clone-114
                       clone-110
                       clone-118
                       clone-113
                       clone-112
                       clone-111)
  (if (null? clone-110)
      (_op 1
           '_sim-error
           (_lift0 1 'direct-parse)
           (_lift0 1 "can't shift on")
           clone-118)
      (multi-memo 1
                  'loop_5-22-119
                  '(0 0 0 0 0 1 1 1 1)
                  (list clone-117
                        clone-116
                        clone-115
                        clone-114
                        clone-110
                        clone-118
                        clone-113
                        clone-112
                        clone-111))))
(define (loop_5-22-119 clone-127
                       clone-126
                       clone-125
                       clone-124
                       clone-120
                       clone-128
                       clone-123
                       clone-122
                       clone-121)
  (_if 1
       (_op 1 'equal? (_lift0 1 (car clone-120)) clone-128)
       (lambda ()
         (let ((var-135 (_op 1 'cdr clone-123)))
           (_let 1
                 clone-122
                 (lambda (var-136)
                   (let ((var-137 (_op 1 'stream-cdr clone-121)))
                     (let ((var-138 (goto clone-124 (car clone-120))))
                       (let ((var-145 (_op 1 'cons var-135 var-136)))
                         (_let 1
                               var-137
                               (lambda (var-146)
                                 (_let 1
                                       (direct-parse-12-50 var-138
                                                           clone-127
                                                           clone-125
                                                           clone-126
                                                           var-145
                                                           var-146)
                                       (lambda (var-147)
                                         (parse-bar-16-148 var-138
                                                           clone-127
                                                           clone-126
                                                           clone-125
                                                           clone-124
                                                           var-147
                                                           var-136))))))))))))
       (lambda ()
         (_let 1
               clone-128
               (lambda (var-257)
                 (let ((var-265 (cdr clone-120)))
                   (loop_5-21-109 clone-127
                                  clone-126
                                  clone-125
                                  clone-124
                                  var-265
                                  var-257
                                  clone-123
                                  clone-122
                                  clone-121)))))))
(define (parse-bar-16-148 clone-155
                          clone-153
                          clone-152
                          clone-151
                          clone-150
                          clone-154
                          clone-149)
  (if (final? clone-155 clone-153)
      clone-154
      (let ((var-156 (_op 1 'result-lhs clone-154)))
        (_let 1
              (_op 1 'result-dot clone-154)
              (lambda (var-157)
                (let ((var-158 (_op 1 'result-att clone-154)))
                  (let ((var-159 (_op 1 'result-inp clone-154)))
                    (let ((var-160 (next-nonterminals clone-150 clone-153)))
                      (parse-bar-17-161 clone-153
                                        clone-152
                                        clone-151
                                        clone-150
                                        var-160
                                        var-157
                                        var-156
                                        var-158
                                        clone-149
                                        var-159)))))))))
(define (parse-bar-17-161 clone-169
                          clone-168
                          clone-167
                          clone-166
                          clone-162
                          clone-171
                          clone-170
                          clone-165
                          clone-164
                          clone-163)
  (if (null? clone-162)
      (_op 1
           'parse-result
           clone-170
           (_op 1 '- clone-171 (_lift0 1 1))
           clone-165
           clone-163)
      (multi-memo 1
                  'parse-bar-18-172
                  '(0 0 0 0 0 1 1 1 1 1)
                  (list clone-169
                        clone-168
                        clone-167
                        clone-166
                        clone-162
                        clone-171
                        clone-170
                        clone-165
                        clone-164
                        clone-163))))
(define (parse-bar-18-172 clone-180
                          clone-179
                          clone-178
                          clone-177
                          clone-173
                          clone-182
                          clone-181
                          clone-176
                          clone-175
                          clone-174)
  (_if 1
       (_op 1 '< (_lift0 1 1) clone-182)
       (lambda ()
         (_op 1
              'parse-result
              clone-181
              (_op 1 '- clone-182 (_lift0 1 1))
              clone-176
              clone-174))
       (lambda ()
         (_let 1
               clone-181
               (lambda (var-183)
                 (loop_6-19-192 clone-180
                                clone-179
                                clone-178
                                clone-177
                                clone-173
                                var-183
                                clone-176
                                clone-175
                                clone-174))))))
(define (loop_6-19-192 clone-200
                       clone-199
                       clone-198
                       clone-197
                       clone-193
                       clone-201
                       clone-196
                       clone-195
                       clone-194)
  (if (null? (cdr clone-193))
      (_let 1
            clone-195
            (lambda (var-208)
              (let ((var-210 (goto clone-197 (car clone-193))))
                (let ((var-217 (_op 1 'cons clone-196 var-208)))
                  (_let 1
                        clone-194
                        (lambda (var-218)
                          (_let 1
                                (direct-parse-12-50 var-210
                                                    clone-200
                                                    clone-198
                                                    clone-199
                                                    var-217
                                                    var-218)
                                (lambda (var-219)
                                  (parse-bar-16-148 var-210
                                                    clone-200
                                                    clone-199
                                                    clone-198
                                                    clone-197
                                                    var-219
                                                    var-208)))))))))
      (multi-memo 1
                  'loop_6-20-220
                  '(0 0 0 0 0 1 1 1 1)
                  (list clone-200
                        clone-199
                        clone-198
                        clone-197
                        clone-193
                        clone-201
                        clone-196
                        clone-195
                        clone-194))))
(define (loop_6-20-220 clone-228
                       clone-227
                       clone-226
                       clone-225
                       clone-221
                       clone-229
                       clone-224
                       clone-223
                       clone-222)
  (_if 1
       (_op 1 'equal? (_lift0 1 (car clone-221)) clone-229)
       (lambda ()
         (_let 1
               clone-223
               (lambda (var-236)
                 (let ((var-238 (goto clone-225 (car clone-221))))
                   (let ((var-245 (_op 1 'cons clone-224 var-236)))
                     (_let 1
                           clone-222
                           (lambda (var-246)
                             (_let 1
                                   (direct-parse-12-50 var-238
                                                       clone-228
                                                       clone-226
                                                       clone-227
                                                       var-245
                                                       var-246)
                                   (lambda (var-247)
                                     (parse-bar-16-148 var-238
                                                       clone-228
                                                       clone-227
                                                       clone-226
                                                       clone-225
                                                       var-247
                                                       var-236))))))))))
       (lambda ()
         (_let 1
               clone-229
               (lambda (var-248)
                 (let ((var-256 (cdr clone-221)))
                   (loop_6-19-192 clone-228
                                  clone-227
                                  clone-226
                                  clone-225
                                  var-256
                                  var-248
                                  clone-224
                                  clone-223
                                  clone-222)))))))
(define (direct-parse-14-64 clone-67 clone-66 clone-65)
  (if (final? clone-67 clone-66)
      (let ((var-72 (static-constructor '1
                                        (lambda ()
                                          (lambda (t_1-68)
                                            (not (equal? t_1-68 '$))))
                                        (list)
                                        '())))
        (filter-20-10-75 var-72 clone-65 '()))
      clone-65))
(define (filter-20-10-75 clone-78 clone-77 clone-76)
  (if (null? clone-77)
      (reverse clone-76)
      (let ((var-84 (cdr clone-77)))
        (filter-20-10-75 clone-78
                         var-84
                         (filter-20-11-79 clone-78 clone-77 clone-76)))))
(define (filter-20-11-79 clone-82 clone-81 clone-80)
  (if ((clone-82 'value) (car clone-81))
      (cons (car clone-81) clone-80)
      clone-80))
(define (direct-parse-13-57 clone-59 clone-58)
  (let ((var-60 (_op 1 'stream-car clone-58)))
    (_if 1
         (_op 1 'equal? (_lift0 1 '$) (_op 1 'car var-60))
         (lambda ()
           (_op 1 'car clone-59))
         (lambda ()
           (let ((var-61 (_op 1 'stream-car clone-58)))
             (_op 1
                  '_sim-error
                  (_lift0 1 'direct-parse)
                  (_lift0 1 "expecting eof")
                  (_op 1 'car var-61)))))))
