(define ($goal-88-117 clone-1)
  (let ((var-3 (stream-car clone-1)))
    (let ((var-4 (car var-3)))
      (loop_5-17-96-118 var-4 var-3 '() clone-1))))
(define (loop_5-17-96-118 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-119 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-125 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-217 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-217 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-154 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-125 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (parse-bar-13-99-125 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-126 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-126 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'e clone-4)
      (let ((var-7 (direct-parse-8-90-127 (cons clone-3 clone-2) clone-1)))
        var-7)
      (loop_6-15-101-128 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-128 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-129 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-125 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-137 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-125 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (inner-loop_1-23-113-129 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '$)))
    (if (loop_4-25-114-130 var-6 clone-4)
        (parse-result 'e 1 ((lambda ($1) $1) (car clone-2)) clone-3)
        (let ((var-14 (stream-car clone-3)))
          (let ((var-15 (car var-14)))
            (loop_2-28-105-131 var-15 var-14 clone-2 clone-3))))))
(define (loop_2-28-105-131 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '- clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-132 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (loop_2-28-105-210 clone-4 clone-3 clone-2 clone-1)))
(define (loop_2-28-105-210 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '+ clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-211 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (loop_5-17-96-211 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-119 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-212 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-216 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-216 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-154 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-212 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (parse-bar-13-99-212 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-213 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-213 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'e clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-214 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-212 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (loop_6-15-101-215 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-215 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-129 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-212 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-137 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-212 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (inner-loop_1-23-113-214 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '$)))
    (if (loop_4-25-114-130 var-6 clone-4)
        (let ((var-11 (cdr clone-2)))
          (let ((var-12 (cdr var-11)))
            (parse-result 'e
                          3
                          ((lambda ($1 $2 $3) (+ $1 $3))
                           (car var-12)
                           (car var-11)
                           (car clone-2))
                          clone-3)))
        (let ((var-16 (stream-car clone-3)))
          (let ((var-17 (car var-16)))
            (_sim-error 'direct-parse "can't shift on" var-17))))))
(define (loop_5-17-96-132 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-119 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-133 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-209 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-209 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-154 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-133 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (parse-bar-13-99-133 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-134 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-134 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'e clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-135 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-133 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (loop_6-15-101-136 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-136 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-129 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-133 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-137 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-133 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (inner-loop_1-23-113-137 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '$)))
    (if (loop_4-25-114-138 var-6 clone-4)
        (parse-result 't 1 ((lambda ($1) $1) (car clone-2)) clone-3)
        (let ((var-14 (stream-car clone-3)))
          (let ((var-15 (car var-14)))
            (loop_2-28-105-141 var-15 var-14 clone-2 clone-3))))))
(define (loop_2-28-105-141 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '/ clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-142 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (loop_2-28-105-208 clone-4 clone-3 clone-2 clone-1)))
(define (loop_2-28-105-208 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '* clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-149 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (loop_5-17-96-142 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-119 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-143 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-207 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-207 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-154 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-143 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (parse-bar-13-99-143 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-144 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-144 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-145 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-143 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-146 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-143 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (inner-loop_1-23-113-146 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '$)))
    (if (loop_4-25-114-138 var-6 clone-4)
        (parse-result 't 1 ((lambda ($1) $1) (car clone-2)) clone-3)
        (let ((var-14 (stream-car clone-3)))
          (let ((var-15 (car var-14)))
            (loop_2-28-105-147 var-15 var-14 clone-2 clone-3))))))
(define (loop_2-28-105-147 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '/ clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-142 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (loop_2-28-105-148 clone-4 clone-3 clone-2 clone-1)))
(define (loop_2-28-105-148 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '* clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-149 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (loop_5-17-96-149 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-119 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-150 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-153 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-153 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-154 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-150 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (loop_5-17-96-154 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-155 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-161 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-206 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-206 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-189 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-161 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (parse-bar-13-99-161 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-162 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-162 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'e clone-4)
      (let ((var-7 (stream-car clone-1)))
        (let ((var-8 (car var-7)))
          (let ((var-9 (loop_5-17-96-163 var-8
                                         var-7
                                         (cons clone-3 clone-2)
                                         clone-1)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-161 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_6-15-101-165 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-165 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-166 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-161 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-173 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-161 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (inner-loop_1-23-113-166 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 'r)))
    (if (loop_4-25-114-160 var-6 clone-4)
        (parse-result 'e 1 ((lambda ($1) $1) (car clone-2)) clone-3)
        (let ((var-14 (stream-car clone-3)))
          (let ((var-15 (car var-14)))
            (loop_2-28-105-167 var-15 var-14 clone-2 clone-3))))))
(define (loop_2-28-105-167 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '- clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-168 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (loop_2-28-105-199 clone-4 clone-3 clone-2 clone-1)))
(define (loop_2-28-105-199 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '+ clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-200 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (loop_5-17-96-200 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-155 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-201 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-205 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-205 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-189 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-201 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (parse-bar-13-99-201 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-202 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-202 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'e clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-203 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-201 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (loop_6-15-101-204 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-204 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-166 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-201 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-173 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-201 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (inner-loop_1-23-113-203 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 'r)))
    (if (loop_4-25-114-160 var-6 clone-4)
        (let ((var-11 (cdr clone-2)))
          (let ((var-12 (cdr var-11)))
            (parse-result 'e
                          3
                          ((lambda ($1 $2 $3) (+ $1 $3))
                           (car var-12)
                           (car var-11)
                           (car clone-2))
                          clone-3)))
        (let ((var-16 (stream-car clone-3)))
          (let ((var-17 (car var-16)))
            (_sim-error 'direct-parse "can't shift on" var-17))))))
(define (loop_5-17-96-168 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-155 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-169 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-198 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-198 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-189 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-169 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (parse-bar-13-99-169 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-170 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-170 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'e clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-171 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-169 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (loop_6-15-101-172 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-172 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-166 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-169 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-173 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-169 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (inner-loop_1-23-113-173 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '+)))
    (if (loop_4-25-114-174 var-6 clone-4)
        (parse-result 't 1 ((lambda ($1) $1) (car clone-2)) clone-3)
        (let ((var-14 (stream-car clone-3)))
          (let ((var-15 (car var-14)))
            (loop_2-28-105-176 var-15 var-14 clone-2 clone-3))))))
(define (loop_2-28-105-176 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '/ clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-177 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (loop_2-28-105-197 clone-4 clone-3 clone-2 clone-1)))
(define (loop_2-28-105-197 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '* clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-184 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (loop_5-17-96-177 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-155 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-178 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-196 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-196 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-189 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-178 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (parse-bar-13-99-178 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-179 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-179 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-180 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-178 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-181 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-178 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (inner-loop_1-23-113-181 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '+)))
    (if (loop_4-25-114-174 var-6 clone-4)
        (parse-result 't 1 ((lambda ($1) $1) (car clone-2)) clone-3)
        (let ((var-14 (stream-car clone-3)))
          (let ((var-15 (car var-14)))
            (loop_2-28-105-182 var-15 var-14 clone-2 clone-3))))))
(define (loop_2-28-105-182 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '/ clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-177 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (loop_2-28-105-183 clone-4 clone-3 clone-2 clone-1)))
(define (loop_2-28-105-183 clone-4 clone-3 clone-2 clone-1)
  (if (equal? '* clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-184 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-result (result-lhs var-9)
                              (- var-10 1)
                              (result-att var-9)
                              (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (loop_5-17-96-184 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-155 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-185 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-188 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-188 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-189 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-185 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (loop_5-17-96-189 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'n clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-155 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-190 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_5-17-96-195 clone-4 clone-3 clone-2 clone-1)))
(define (loop_5-17-96-195 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'l clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-7 (stream-car var-6)))
          (let ((var-8 (car var-7)))
            (let ((var-9 (loop_5-17-96-189 var-8
                                           var-7
                                           (cons (cdr clone-3) clone-2)
                                           var-6)))
              (let ((var-10 (result-dot var-9)))
                (parse-bar-13-99-190 var-10
                                     (result-lhs var-9)
                                     (result-att var-9)
                                     clone-2
                                     (result-inp var-9)))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (parse-bar-13-99-190 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-191 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-191 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'e clone-4)
      (let ((var-7 (stream-car clone-1)))
        (let ((var-8 (car var-7)))
          (let ((var-9 (loop_5-17-96-192 var-8
                                         var-7
                                         (cons clone-3 clone-2)
                                         clone-1)))
            (let ((var-10 (result-dot var-9)))
              (parse-bar-13-99-190 var-10
                                   (result-lhs var-9)
                                   (result-att var-9)
                                   clone-2
                                   (result-inp var-9))))))
      (loop_6-15-101-194 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-194 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-166 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-190 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-173 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-190 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (loop_5-17-96-192 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'r clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-193 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-result (result-lhs var-9)
                            (- var-10 1)
                            (result-att var-9)
                            (result-inp var-9))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (inner-loop_1-23-113-193 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '*)))
    (if (loop_4-25-114-156 var-6 clone-4)
        (let ((var-11 (cdr clone-2)))
          (let ((var-12 (cdr var-11)))
            (parse-result 'p
                          3
                          ((lambda ($1 $2 $3) $2)
                           (car var-12)
                           (car var-11)
                           (car clone-2))
                          clone-3)))
        (let ((var-16 (stream-car clone-3)))
          (let ((var-17 (car var-16)))
            (_sim-error 'direct-parse "can't shift on" var-17))))))
(define (parse-bar-13-99-185 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-186 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-186 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-187 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-185 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-181 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-185 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (inner-loop_1-23-113-187 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '+)))
    (if (loop_4-25-114-174 var-6 clone-4)
        (let ((var-11 (cdr clone-2)))
          (let ((var-12 (cdr var-11)))
            (parse-result 't
                          3
                          ((lambda ($1 $2 $3) (* $1 $3))
                           (car var-12)
                           (car var-11)
                           (car clone-2))
                          clone-3)))
        (let ((var-16 (stream-car clone-3)))
          (let ((var-17 (car var-16)))
            (_sim-error 'direct-parse "can't shift on" var-17))))))
(define (inner-loop_1-23-113-180 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '+)))
    (if (loop_4-25-114-174 var-6 clone-4)
        (let ((var-11 (cdr clone-2)))
          (let ((var-12 (cdr var-11)))
            (parse-result 't
                          3
                          ((lambda ($1 $2 $3) (/ $1 $3))
                           (car var-12)
                           (car var-11)
                           (car clone-2))
                          clone-3)))
        (let ((var-16 (stream-car clone-3)))
          (let ((var-17 (car var-16)))
            (_sim-error 'direct-parse "can't shift on" var-17))))))
(define (loop_4-25-114-174 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 '-)))
        (loop_4-25-114-175 var-4 clone-1))))
(define (loop_4-25-114-175 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 'r)))
        (loop_4-25-114-160 var-4 clone-1))))
(define (inner-loop_1-23-113-171 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 'r)))
    (if (loop_4-25-114-160 var-6 clone-4)
        (let ((var-11 (cdr clone-2)))
          (let ((var-12 (cdr var-11)))
            (parse-result 'e
                          3
                          ((lambda ($1 $2 $3) (- $1 $3))
                           (car var-12)
                           (car var-11)
                           (car clone-2))
                          clone-3)))
        (let ((var-16 (stream-car clone-3)))
          (let ((var-17 (car var-16)))
            (_sim-error 'direct-parse "can't shift on" var-17))))))
(define (loop_5-17-96-163 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 'r clone-4)
      (let ((var-6 (stream-cdr clone-1)))
        (let ((var-8 (stream-car var-6)))
          (let ((var-9 (inner-loop_1-23-113-164 var-8
                                                var-6
                                                (cons (cdr clone-3) clone-2)
                                                var-6)))
            (let ((var-10 (result-dot var-9)))
              (parse-result (result-lhs var-9)
                            (- var-10 1)
                            (result-att var-9)
                            (result-inp var-9))))))
      (_sim-error 'direct-parse "can't shift on" clone-4)))
(define (inner-loop_1-23-113-164 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '$)))
    (if (loop_4-25-114-120 var-6 clone-4)
        (let ((var-11 (cdr clone-2)))
          (let ((var-12 (cdr var-11)))
            (parse-result 'p
                          3
                          ((lambda ($1 $2 $3) $2)
                           (car var-12)
                           (car var-11)
                           (car clone-2))
                          clone-3)))
        (let ((var-16 (stream-car clone-3)))
          (let ((var-17 (car var-16)))
            (_sim-error 'direct-parse "can't shift on" var-17))))))
(define (inner-loop_1-23-113-155 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '*)))
    (if (loop_4-25-114-156 var-6 clone-4)
        (parse-result 'p 1 ((lambda ($1) $1) (car clone-2)) clone-3)
        (let ((var-14 (stream-car clone-3)))
          (let ((var-15 (car var-14)))
            (_sim-error 'direct-parse "can't shift on" var-15))))))
(define (loop_4-25-114-156 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 '+)))
        (loop_4-25-114-157 var-4 clone-1))))
(define (loop_4-25-114-157 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 '-)))
        (loop_4-25-114-158 var-4 clone-1))))
(define (loop_4-25-114-158 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 '/)))
        (loop_4-25-114-159 var-4 clone-1))))
(define (loop_4-25-114-159 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 'r)))
        (loop_4-25-114-160 var-4 clone-1))))
(define (loop_4-25-114-160 clone-2 clone-1)
  (if clone-2 clone-2 #f))
(define (parse-bar-13-99-150 clone-5 clone-4 clone-3 clone-2 clone-1)
  (if (< 1 clone-5)
      (parse-result clone-4 (- clone-5 1) clone-3 clone-1)
      (loop_6-15-101-151 clone-4 clone-3 clone-2 clone-1)))
(define (loop_6-15-101-151 clone-4 clone-3 clone-2 clone-1)
  (if (equal? 't clone-4)
      (let ((var-8 (stream-car clone-1)))
        (let ((var-9 (inner-loop_1-23-113-152 var-8
                                              clone-1
                                              (cons clone-3 clone-2)
                                              clone-1)))
          (let ((var-10 (result-dot var-9)))
            (parse-bar-13-99-150 var-10
                                 (result-lhs var-9)
                                 (result-att var-9)
                                 clone-2
                                 (result-inp var-9)))))
      (let ((var-15 (stream-car clone-1)))
        (let ((var-16 (inner-loop_1-23-113-146 var-15
                                               clone-1
                                               (cons clone-3 clone-2)
                                               clone-1)))
          (let ((var-17 (result-dot var-16)))
            (parse-bar-13-99-150 var-17
                                 (result-lhs var-16)
                                 (result-att var-16)
                                 clone-2
                                 (result-inp var-16)))))))
(define (inner-loop_1-23-113-152 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '$)))
    (if (loop_4-25-114-138 var-6 clone-4)
        (let ((var-11 (cdr clone-2)))
          (let ((var-12 (cdr var-11)))
            (parse-result 't
                          3
                          ((lambda ($1 $2 $3) (* $1 $3))
                           (car var-12)
                           (car var-11)
                           (car clone-2))
                          clone-3)))
        (let ((var-16 (stream-car clone-3)))
          (let ((var-17 (car var-16)))
            (_sim-error 'direct-parse "can't shift on" var-17))))))
(define (inner-loop_1-23-113-145 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '$)))
    (if (loop_4-25-114-138 var-6 clone-4)
        (let ((var-11 (cdr clone-2)))
          (let ((var-12 (cdr var-11)))
            (parse-result 't
                          3
                          ((lambda ($1 $2 $3) (/ $1 $3))
                           (car var-12)
                           (car var-11)
                           (car clone-2))
                          clone-3)))
        (let ((var-16 (stream-car clone-3)))
          (let ((var-17 (car var-16)))
            (_sim-error 'direct-parse "can't shift on" var-17))))))
(define (loop_4-25-114-138 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 '+)))
        (loop_4-25-114-139 var-4 clone-1))))
(define (loop_4-25-114-139 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 '-)))
        (loop_4-25-114-140 var-4 clone-1))))
(define (loop_4-25-114-140 clone-2 clone-1)
  (if clone-2 clone-2 #f))
(define (inner-loop_1-23-113-135 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '$)))
    (if (loop_4-25-114-130 var-6 clone-4)
        (let ((var-11 (cdr clone-2)))
          (let ((var-12 (cdr var-11)))
            (parse-result 'e
                          3
                          ((lambda ($1 $2 $3) (- $1 $3))
                           (car var-12)
                           (car var-11)
                           (car clone-2))
                          clone-3)))
        (let ((var-16 (stream-car clone-3)))
          (let ((var-17 (car var-16)))
            (_sim-error 'direct-parse "can't shift on" var-17))))))
(define (loop_4-25-114-130 clone-2 clone-1)
  (if clone-2 clone-2 #f))
(define (direct-parse-8-90-127 clone-2 clone-1)
  (if (equal? '$ (car (stream-car clone-1)))
      (car clone-2)
      (_sim-error 'direct-parse "expecting eof" (car (stream-car clone-1)))))
(define (inner-loop_1-23-113-119 clone-4 clone-3 clone-2 clone-1)
  (let ((var-6 (equal? clone-4 '$)))
    (if (loop_4-25-114-120 var-6 clone-4)
        (parse-result 'p 1 ((lambda ($1) $1) (car clone-2)) clone-3)
        (let ((var-14 (stream-car clone-3)))
          (let ((var-15 (car var-14)))
            (_sim-error 'direct-parse "can't shift on" var-15))))))
(define (loop_4-25-114-120 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 '*)))
        (loop_4-25-114-121 var-4 clone-1))))
(define (loop_4-25-114-121 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 '+)))
        (loop_4-25-114-122 var-4 clone-1))))
(define (loop_4-25-114-122 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 '-)))
        (loop_4-25-114-123 var-4 clone-1))))
(define (loop_4-25-114-123 clone-2 clone-1)
  (if clone-2
      clone-2
      (let ((var-4 (equal? clone-1 '/)))
        (loop_4-25-114-124 var-4 clone-1))))
(define (loop_4-25-114-124 clone-2 clone-1)
  (if clone-2 clone-2 #f))
