(define ($goal-1358-1860 clone-1861)
  (let ((var-1865 (stream-car clone-1861)))
    (let ((var-1867 (car var-1865)))
      (loop_5-1345-1442-1872 var-1867
                             var-1865
                             '()
                             clone-1861))))
(define (loop_5-1345-1442-1872 clone-1876 clone-1875 clone-1874 clone-1873)
  (if (equal? 'n clone-1876)
      (let ((var-1878 (cdr clone-1875)))
        (let ((var-1880 (stream-cdr clone-1873)))
          (let ((var-1883 (cons var-1878 clone-1874)))
            (let ((var-1891 (stream-car var-1880)))
              (let ((var-1946 (inner-loop_1-1351-1807-1896 var-1891
                                                           var-1880
                                                           var-1883
                                                           var-1880)))
                (let ((var-1947 (result-lhs var-1946)))
                  (let ((var-1948 (result-dot var-1946)))
                    (let ((var-1949 (result-att var-1946)))
                      (let ((var-1950 (result-inp var-1946)))
                        (parse-bar-1341-1495-1951 var-1948
                                                  var-1947
                                                  var-1949
                                                  clone-1874
                                                  var-1950))))))))))
      (loop_5-1345-1442-4438 clone-1876 clone-1875 clone-1874 clone-1873)))
(define (loop_5-1345-1442-4438 clone-4442 clone-4441 clone-4440 clone-4439)
  (if (equal? 'l clone-4442)
      (let ((var-4444 (cdr clone-4441)))
        (let ((var-4446 (stream-cdr clone-4439)))
          (let ((var-4449 (cons var-4444 clone-4440)))
            (let ((var-4451 (stream-car var-4446)))
              (let ((var-4453 (car var-4451)))
                (let ((var-4458 (loop_5-1345-1442-2607 var-4453
                                                       var-4451
                                                       var-4449
                                                       var-4446)))
                  (let ((var-4459 (result-lhs var-4458)))
                    (let ((var-4460 (result-dot var-4458)))
                      (let ((var-4461 (result-att var-4458)))
                        (let ((var-4462 (result-inp var-4458)))
                          (parse-bar-1341-1495-1951 var-4460
                                                    var-4459
                                                    var-4461
                                                    clone-4440
                                                    var-4462)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-4442)))
(define (parse-bar-1341-1495-1951 clone-1956
                                  clone-1955
                                  clone-1954
                                  clone-1953
                                  clone-1952)
  (if (< 1 clone-1956)
      (parse-result clone-1955 (- clone-1956 1) clone-1954 clone-1952)
      (loop_6-1343-1543-1961 clone-1955 clone-1954 clone-1953 clone-1952)))
(define (loop_6-1343-1543-1961 clone-1965 clone-1964 clone-1963 clone-1962)
  (if (equal? 'e clone-1965)
      (let ((var-1971 (cons clone-1964 clone-1963)))
        (let ((var-1978 (direct-parse-1336-1380-1973 var-1971 clone-1962)))
          var-1978))
      (loop_6-1343-1543-1983 clone-1965 clone-1964 clone-1963 clone-1962)))
(define (loop_6-1343-1543-1983 clone-1987 clone-1986 clone-1985 clone-1984)
  (if (equal? 't clone-1987)
      (let ((var-1993 (cons clone-1986 clone-1985)))
        (let ((var-2001 (stream-car clone-1984)))
          (let ((var-4402 (inner-loop_1-1351-1807-2006 var-2001
                                                       clone-1984
                                                       var-1993
                                                       clone-1984)))
            (let ((var-4403 (result-lhs var-4402)))
              (let ((var-4404 (result-dot var-4402)))
                (let ((var-4405 (result-att var-4402)))
                  (let ((var-4406 (result-inp var-4402)))
                    (parse-bar-1341-1495-1951 var-4404
                                              var-4403
                                              var-4405
                                              clone-1985
                                              var-4406))))))))
      (let ((var-4416 (cons clone-1986 clone-1985)))
        (let ((var-4424 (stream-car clone-1984)))
          (let ((var-4429 (inner-loop_1-1351-1807-2206 var-4424
                                                       clone-1984
                                                       var-4416
                                                       clone-1984)))
            (let ((var-4430 (result-lhs var-4429)))
              (let ((var-4431 (result-dot var-4429)))
                (let ((var-4432 (result-att var-4429)))
                  (let ((var-4433 (result-inp var-4429)))
                    (parse-bar-1341-1495-1951 var-4431
                                              var-4430
                                              var-4432
                                              clone-1985
                                              var-4433))))))))))
(define (inner-loop_1-1351-1807-2006 clone-2010
                                     clone-2009
                                     clone-2008
                                     clone-2007)
  (let ((var-2012 (equal? clone-2010 '$)))
    (if (loop_4-1353-1823-2013 var-2012 clone-2010)
        (let ((var-2022 (apply (lambda ($1) $1)
                               (cons (car clone-2008)
                                     '()))))
          (parse-result 'e 1 var-2022 clone-2009))
        (let ((var-2029 (stream-car clone-2009)))
          (let ((var-2031 (car var-2029)))
            (loop_2-1356-1661-2036 var-2031 var-2029 clone-2008 clone-2009))))))
(define (loop_2-1356-1661-2036 clone-2040 clone-2039 clone-2038 clone-2037)
  (if (equal? '- clone-2040)
      (let ((var-2042 (cdr clone-2039)))
        (let ((var-2044 (stream-cdr clone-2037)))
          (let ((var-2047 (cons var-2042 clone-2038)))
            (let ((var-2049 (stream-car var-2044)))
              (let ((var-2051 (car var-2049)))
                (let ((var-4176 (loop_5-1345-1442-2056 var-2051
                                                       var-2049
                                                       var-2047
                                                       var-2044)))
                  (let ((var-4177 (result-lhs var-4176)))
                    (let ((var-4178 (result-dot var-4176)))
                      (let ((var-4179 (result-att var-4176)))
                        (let ((var-4180 (result-inp var-4176)))
                          (parse-result var-4177
                                        (- var-4178 1)
                                        var-4179
                                        var-4180)))))))))))
      (loop_2-1356-1661-4185 clone-2040 clone-2039 clone-2038 clone-2037)))
(define (loop_2-1356-1661-4185 clone-4189 clone-4188 clone-4187 clone-4186)
  (if (equal? '+ clone-4189)
      (let ((var-4191 (cdr clone-4188)))
        (let ((var-4193 (stream-cdr clone-4186)))
          (let ((var-4196 (cons var-4191 clone-4187)))
            (let ((var-4198 (stream-car var-4193)))
              (let ((var-4200 (car var-4198)))
                (let ((var-4393 (loop_5-1345-1442-4205 var-4200
                                                       var-4198
                                                       var-4196
                                                       var-4193)))
                  (let ((var-4394 (result-lhs var-4393)))
                    (let ((var-4395 (result-dot var-4393)))
                      (let ((var-4396 (result-att var-4393)))
                        (let ((var-4397 (result-inp var-4393)))
                          (parse-result var-4394
                                        (- var-4395 1)
                                        var-4396
                                        var-4397)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-4189)))
(define (loop_5-1345-1442-4205 clone-4209 clone-4208 clone-4207 clone-4206)
  (if (equal? 'n clone-4209)
      (let ((var-4211 (cdr clone-4208)))
        (let ((var-4213 (stream-cdr clone-4206)))
          (let ((var-4216 (cons var-4211 clone-4207)))
            (let ((var-4224 (stream-car var-4213)))
              (let ((var-4229 (inner-loop_1-1351-1807-1896 var-4224
                                                           var-4213
                                                           var-4216
                                                           var-4213)))
                (let ((var-4230 (result-lhs var-4229)))
                  (let ((var-4231 (result-dot var-4229)))
                    (let ((var-4232 (result-att var-4229)))
                      (let ((var-4233 (result-inp var-4229)))
                        (parse-bar-1341-1495-4234 var-4231
                                                  var-4230
                                                  var-4232
                                                  clone-4207
                                                  var-4233))))))))))
      (loop_5-1345-1442-4364 clone-4209 clone-4208 clone-4207 clone-4206)))
(define (loop_5-1345-1442-4364 clone-4368 clone-4367 clone-4366 clone-4365)
  (if (equal? 'l clone-4368)
      (let ((var-4370 (cdr clone-4367)))
        (let ((var-4372 (stream-cdr clone-4365)))
          (let ((var-4375 (cons var-4370 clone-4366)))
            (let ((var-4377 (stream-car var-4372)))
              (let ((var-4379 (car var-4377)))
                (let ((var-4384 (loop_5-1345-1442-2607 var-4379
                                                       var-4377
                                                       var-4375
                                                       var-4372)))
                  (let ((var-4385 (result-lhs var-4384)))
                    (let ((var-4386 (result-dot var-4384)))
                      (let ((var-4387 (result-att var-4384)))
                        (let ((var-4388 (result-inp var-4384)))
                          (parse-bar-1341-1495-4234 var-4386
                                                    var-4385
                                                    var-4387
                                                    clone-4366
                                                    var-4388)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-4368)))
(define (parse-bar-1341-1495-4234 clone-4239
                                  clone-4238
                                  clone-4237
                                  clone-4236
                                  clone-4235)
  (if (< 1 clone-4239)
      (parse-result clone-4238 (- clone-4239 1) clone-4237 clone-4235)
      (loop_6-1343-1543-4244 clone-4238 clone-4237 clone-4236 clone-4235)))
(define (loop_6-1343-1543-4244 clone-4248 clone-4247 clone-4246 clone-4245)
  (if (equal? 'e clone-4248)
      (let ((var-4254 (cons clone-4247 clone-4246)))
        (let ((var-4262 (stream-car clone-4245)))
          (let ((var-4296 (inner-loop_1-1351-1807-4267 var-4262
                                                       clone-4245
                                                       var-4254
                                                       clone-4245)))
            (let ((var-4297 (result-lhs var-4296)))
              (let ((var-4298 (result-dot var-4296)))
                (let ((var-4299 (result-att var-4296)))
                  (let ((var-4300 (result-inp var-4296)))
                    (parse-bar-1341-1495-4234 var-4298
                                              var-4297
                                              var-4299
                                              clone-4246
                                              var-4300))))))))
      (loop_6-1343-1543-4305 clone-4248 clone-4247 clone-4246 clone-4245)))
(define (loop_6-1343-1543-4305 clone-4309 clone-4308 clone-4307 clone-4306)
  (if (equal? 't clone-4309)
      (let ((var-4315 (cons clone-4308 clone-4307)))
        (let ((var-4323 (stream-car clone-4306)))
          (let ((var-4328 (inner-loop_1-1351-1807-2006 var-4323
                                                       clone-4306
                                                       var-4315
                                                       clone-4306)))
            (let ((var-4329 (result-lhs var-4328)))
              (let ((var-4330 (result-dot var-4328)))
                (let ((var-4331 (result-att var-4328)))
                  (let ((var-4332 (result-inp var-4328)))
                    (parse-bar-1341-1495-4234 var-4330
                                              var-4329
                                              var-4331
                                              clone-4307
                                              var-4332))))))))
      (let ((var-4342 (cons clone-4308 clone-4307)))
        (let ((var-4350 (stream-car clone-4306)))
          (let ((var-4355 (inner-loop_1-1351-1807-2206 var-4350
                                                       clone-4306
                                                       var-4342
                                                       clone-4306)))
            (let ((var-4356 (result-lhs var-4355)))
              (let ((var-4357 (result-dot var-4355)))
                (let ((var-4358 (result-att var-4355)))
                  (let ((var-4359 (result-inp var-4355)))
                    (parse-bar-1341-1495-4234 var-4357
                                              var-4356
                                              var-4358
                                              clone-4307
                                              var-4359))))))))))
(define (inner-loop_1-1351-1807-4267 clone-4271
                                     clone-4270
                                     clone-4269
                                     clone-4268)
  (let ((var-4273 (equal? clone-4271 '$)))
    (if (loop_4-1353-1823-2013 var-4273 clone-4271)
        (let ((var-4280 (cdr clone-4269)))
          (let ((var-4281 (cdr var-4280)))
            (let ((var-4282 (apply (lambda ($1 $2 $3) (+ $1 $3))
                                   (cons (car var-4281)
                                         (cons (car var-4280)
                                               (cons (car clone-4269)
                                                     '()))))))
              (parse-result 'e 3 var-4282 clone-4270))))
        (let ((var-4289 (stream-car clone-4270)))
          (let ((var-4291 (car var-4289)))
            (_sim-error 'direct-parse "can't shift on" var-4291))))))
(define (loop_5-1345-1442-2056 clone-2060 clone-2059 clone-2058 clone-2057)
  (if (equal? 'n clone-2060)
      (let ((var-2062 (cdr clone-2059)))
        (let ((var-2064 (stream-cdr clone-2057)))
          (let ((var-2067 (cons var-2062 clone-2058)))
            (let ((var-2075 (stream-car var-2064)))
              (let ((var-2080 (inner-loop_1-1351-1807-1896 var-2075
                                                           var-2064
                                                           var-2067
                                                           var-2064)))
                (let ((var-2081 (result-lhs var-2080)))
                  (let ((var-2082 (result-dot var-2080)))
                    (let ((var-2083 (result-att var-2080)))
                      (let ((var-2084 (result-inp var-2080)))
                        (parse-bar-1341-1495-2085 var-2082
                                                  var-2081
                                                  var-2083
                                                  clone-2058
                                                  var-2084))))))))))
      (loop_5-1345-1442-4147 clone-2060 clone-2059 clone-2058 clone-2057)))
(define (loop_5-1345-1442-4147 clone-4151 clone-4150 clone-4149 clone-4148)
  (if (equal? 'l clone-4151)
      (let ((var-4153 (cdr clone-4150)))
        (let ((var-4155 (stream-cdr clone-4148)))
          (let ((var-4158 (cons var-4153 clone-4149)))
            (let ((var-4160 (stream-car var-4155)))
              (let ((var-4162 (car var-4160)))
                (let ((var-4167 (loop_5-1345-1442-2607 var-4162
                                                       var-4160
                                                       var-4158
                                                       var-4155)))
                  (let ((var-4168 (result-lhs var-4167)))
                    (let ((var-4169 (result-dot var-4167)))
                      (let ((var-4170 (result-att var-4167)))
                        (let ((var-4171 (result-inp var-4167)))
                          (parse-bar-1341-1495-2085 var-4169
                                                    var-4168
                                                    var-4170
                                                    clone-4149
                                                    var-4171)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-4151)))
(define (parse-bar-1341-1495-2085 clone-2090
                                  clone-2089
                                  clone-2088
                                  clone-2087
                                  clone-2086)
  (if (< 1 clone-2090)
      (parse-result clone-2089 (- clone-2090 1) clone-2088 clone-2086)
      (loop_6-1343-1543-2095 clone-2089 clone-2088 clone-2087 clone-2086)))
(define (loop_6-1343-1543-2095 clone-2099 clone-2098 clone-2097 clone-2096)
  (if (equal? 'e clone-2099)
      (let ((var-2105 (cons clone-2098 clone-2097)))
        (let ((var-2113 (stream-car clone-2096)))
          (let ((var-2147 (inner-loop_1-1351-1807-2118 var-2113
                                                       clone-2096
                                                       var-2105
                                                       clone-2096)))
            (let ((var-2148 (result-lhs var-2147)))
              (let ((var-2149 (result-dot var-2147)))
                (let ((var-2150 (result-att var-2147)))
                  (let ((var-2151 (result-inp var-2147)))
                    (parse-bar-1341-1495-2085 var-2149
                                              var-2148
                                              var-2150
                                              clone-2097
                                              var-2151))))))))
      (loop_6-1343-1543-2156 clone-2099 clone-2098 clone-2097 clone-2096)))
(define (loop_6-1343-1543-2156 clone-2160 clone-2159 clone-2158 clone-2157)
  (if (equal? 't clone-2160)
      (let ((var-2166 (cons clone-2159 clone-2158)))
        (let ((var-2174 (stream-car clone-2157)))
          (let ((var-2179 (inner-loop_1-1351-1807-2006 var-2174
                                                       clone-2157
                                                       var-2166
                                                       clone-2157)))
            (let ((var-2180 (result-lhs var-2179)))
              (let ((var-2181 (result-dot var-2179)))
                (let ((var-2182 (result-att var-2179)))
                  (let ((var-2183 (result-inp var-2179)))
                    (parse-bar-1341-1495-2085 var-2181
                                              var-2180
                                              var-2182
                                              clone-2158
                                              var-2183))))))))
      (let ((var-2193 (cons clone-2159 clone-2158)))
        (let ((var-2201 (stream-car clone-2157)))
          (let ((var-4138 (inner-loop_1-1351-1807-2206 var-2201
                                                       clone-2157
                                                       var-2193
                                                       clone-2157)))
            (let ((var-4139 (result-lhs var-4138)))
              (let ((var-4140 (result-dot var-4138)))
                (let ((var-4141 (result-att var-4138)))
                  (let ((var-4142 (result-inp var-4138)))
                    (parse-bar-1341-1495-2085 var-4140
                                              var-4139
                                              var-4141
                                              clone-2158
                                              var-4142))))))))))
(define (inner-loop_1-1351-1807-2206 clone-2210
                                     clone-2209
                                     clone-2208
                                     clone-2207)
  (let ((var-2212 (equal? clone-2210 '$)))
    (if (loop_4-1353-1823-2213 var-2212 clone-2210)
        (let ((var-2232 (apply (lambda ($1) $1)
                               (cons (car clone-2208)
                                     '()))))
          (parse-result 't 1 var-2232 clone-2209))
        (let ((var-2239 (stream-car clone-2209)))
          (let ((var-2241 (car var-2239)))
            (loop_2-1356-1661-2246 var-2241 var-2239 clone-2208 clone-2209))))))
(define (loop_2-1356-1661-2246 clone-2250 clone-2249 clone-2248 clone-2247)
  (if (equal? '/ clone-2250)
      (let ((var-2252 (cdr clone-2249)))
        (let ((var-2254 (stream-cdr clone-2247)))
          (let ((var-2257 (cons var-2252 clone-2248)))
            (let ((var-2259 (stream-car var-2254)))
              (let ((var-2261 (car var-2259)))
                (let ((var-4100 (loop_5-1345-1442-2266 var-2261
                                                       var-2259
                                                       var-2257
                                                       var-2254)))
                  (let ((var-4101 (result-lhs var-4100)))
                    (let ((var-4102 (result-dot var-4100)))
                      (let ((var-4103 (result-att var-4100)))
                        (let ((var-4104 (result-inp var-4100)))
                          (parse-result var-4101
                                        (- var-4102 1)
                                        var-4103
                                        var-4104)))))))))))
      (loop_2-1356-1661-4109 clone-2250 clone-2249 clone-2248 clone-2247)))
(define (loop_2-1356-1661-4109 clone-4113 clone-4112 clone-4111 clone-4110)
  (if (equal? '* clone-4113)
      (let ((var-4115 (cdr clone-4112)))
        (let ((var-4117 (stream-cdr clone-4110)))
          (let ((var-4120 (cons var-4115 clone-4111)))
            (let ((var-4122 (stream-car var-4117)))
              (let ((var-4124 (car var-4122)))
                (let ((var-4129 (loop_5-1345-1442-2460 var-4124
                                                       var-4122
                                                       var-4120
                                                       var-4117)))
                  (let ((var-4130 (result-lhs var-4129)))
                    (let ((var-4131 (result-dot var-4129)))
                      (let ((var-4132 (result-att var-4129)))
                        (let ((var-4133 (result-inp var-4129)))
                          (parse-result var-4130
                                        (- var-4131 1)
                                        var-4132
                                        var-4133)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-4113)))
(define (loop_5-1345-1442-2266 clone-2270 clone-2269 clone-2268 clone-2267)
  (if (equal? 'n clone-2270)
      (let ((var-2272 (cdr clone-2269)))
        (let ((var-2274 (stream-cdr clone-2267)))
          (let ((var-2277 (cons var-2272 clone-2268)))
            (let ((var-2285 (stream-car var-2274)))
              (let ((var-2290 (inner-loop_1-1351-1807-1896 var-2285
                                                           var-2274
                                                           var-2277
                                                           var-2274)))
                (let ((var-2291 (result-lhs var-2290)))
                  (let ((var-2292 (result-dot var-2290)))
                    (let ((var-2293 (result-att var-2290)))
                      (let ((var-2294 (result-inp var-2290)))
                        (parse-bar-1341-1495-2295 var-2292
                                                  var-2291
                                                  var-2293
                                                  clone-2268
                                                  var-2294))))))))))
      (loop_5-1345-1442-4071 clone-2270 clone-2269 clone-2268 clone-2267)))
(define (loop_5-1345-1442-4071 clone-4075 clone-4074 clone-4073 clone-4072)
  (if (equal? 'l clone-4075)
      (let ((var-4077 (cdr clone-4074)))
        (let ((var-4079 (stream-cdr clone-4072)))
          (let ((var-4082 (cons var-4077 clone-4073)))
            (let ((var-4084 (stream-car var-4079)))
              (let ((var-4086 (car var-4084)))
                (let ((var-4091 (loop_5-1345-1442-2607 var-4086
                                                       var-4084
                                                       var-4082
                                                       var-4079)))
                  (let ((var-4092 (result-lhs var-4091)))
                    (let ((var-4093 (result-dot var-4091)))
                      (let ((var-4094 (result-att var-4091)))
                        (let ((var-4095 (result-inp var-4091)))
                          (parse-bar-1341-1495-2295 var-4093
                                                    var-4092
                                                    var-4094
                                                    clone-4073
                                                    var-4095)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-4075)))
(define (parse-bar-1341-1495-2295 clone-2300
                                  clone-2299
                                  clone-2298
                                  clone-2297
                                  clone-2296)
  (if (< 1 clone-2300)
      (parse-result clone-2299 (- clone-2300 1) clone-2298 clone-2296)
      (loop_6-1343-1543-2305 clone-2299 clone-2298 clone-2297 clone-2296)))
(define (loop_6-1343-1543-2305 clone-2309 clone-2308 clone-2307 clone-2306)
  (if (equal? 't clone-2309)
      (let ((var-2315 (cons clone-2308 clone-2307)))
        (let ((var-2323 (stream-car clone-2306)))
          (let ((var-2357 (inner-loop_1-1351-1807-2328 var-2323
                                                       clone-2306
                                                       var-2315
                                                       clone-2306)))
            (let ((var-2358 (result-lhs var-2357)))
              (let ((var-2359 (result-dot var-2357)))
                (let ((var-2360 (result-att var-2357)))
                  (let ((var-2361 (result-inp var-2357)))
                    (parse-bar-1341-1495-2295 var-2359
                                              var-2358
                                              var-2360
                                              clone-2307
                                              var-2361))))))))
      (let ((var-2371 (cons clone-2308 clone-2307)))
        (let ((var-2379 (stream-car clone-2306)))
          (let ((var-4062 (inner-loop_1-1351-1807-2384 var-2379
                                                       clone-2306
                                                       var-2371
                                                       clone-2306)))
            (let ((var-4063 (result-lhs var-4062)))
              (let ((var-4064 (result-dot var-4062)))
                (let ((var-4065 (result-att var-4062)))
                  (let ((var-4066 (result-inp var-4062)))
                    (parse-bar-1341-1495-2295 var-4064
                                              var-4063
                                              var-4065
                                              clone-2307
                                              var-4066))))))))))
(define (inner-loop_1-1351-1807-2384 clone-2388
                                     clone-2387
                                     clone-2386
                                     clone-2385)
  (let ((var-2390 (equal? clone-2388 '$)))
    (if (loop_4-1353-1823-2213 var-2390 clone-2388)
        (let ((var-2397 (apply (lambda ($1) $1)
                               (cons (car clone-2386)
                                     '()))))
          (parse-result 't 1 var-2397 clone-2387))
        (let ((var-2404 (stream-car clone-2387)))
          (let ((var-2406 (car var-2404)))
            (loop_2-1356-1661-2411 var-2406 var-2404 clone-2386 clone-2387))))))
(define (loop_2-1356-1661-2411 clone-2415 clone-2414 clone-2413 clone-2412)
  (if (equal? '/ clone-2415)
      (let ((var-2417 (cdr clone-2414)))
        (let ((var-2419 (stream-cdr clone-2412)))
          (let ((var-2422 (cons var-2417 clone-2413)))
            (let ((var-2424 (stream-car var-2419)))
              (let ((var-2426 (car var-2424)))
                (let ((var-2431 (loop_5-1345-1442-2266 var-2426
                                                       var-2424
                                                       var-2422
                                                       var-2419)))
                  (let ((var-2432 (result-lhs var-2431)))
                    (let ((var-2433 (result-dot var-2431)))
                      (let ((var-2434 (result-att var-2431)))
                        (let ((var-2435 (result-inp var-2431)))
                          (parse-result var-2432
                                        (- var-2433 1)
                                        var-2434
                                        var-2435)))))))))))
      (loop_2-1356-1661-2440 clone-2415 clone-2414 clone-2413 clone-2412)))
(define (loop_2-1356-1661-2440 clone-2444 clone-2443 clone-2442 clone-2441)
  (if (equal? '* clone-2444)
      (let ((var-2446 (cdr clone-2443)))
        (let ((var-2448 (stream-cdr clone-2441)))
          (let ((var-2451 (cons var-2446 clone-2442)))
            (let ((var-2453 (stream-car var-2448)))
              (let ((var-2455 (car var-2453)))
                (let ((var-4053 (loop_5-1345-1442-2460 var-2455
                                                       var-2453
                                                       var-2451
                                                       var-2448)))
                  (let ((var-4054 (result-lhs var-4053)))
                    (let ((var-4055 (result-dot var-4053)))
                      (let ((var-4056 (result-att var-4053)))
                        (let ((var-4057 (result-inp var-4053)))
                          (parse-result var-4054
                                        (- var-4055 1)
                                        var-4056
                                        var-4057)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-2444)))
(define (loop_5-1345-1442-2460 clone-2464 clone-2463 clone-2462 clone-2461)
  (if (equal? 'n clone-2464)
      (let ((var-2466 (cdr clone-2463)))
        (let ((var-2468 (stream-cdr clone-2461)))
          (let ((var-2471 (cons var-2466 clone-2462)))
            (let ((var-2479 (stream-car var-2468)))
              (let ((var-2484 (inner-loop_1-1351-1807-1896 var-2479
                                                           var-2468
                                                           var-2471
                                                           var-2468)))
                (let ((var-2485 (result-lhs var-2484)))
                  (let ((var-2486 (result-dot var-2484)))
                    (let ((var-2487 (result-att var-2484)))
                      (let ((var-2488 (result-inp var-2484)))
                        (parse-bar-1341-1495-2489 var-2486
                                                  var-2485
                                                  var-2487
                                                  clone-2462
                                                  var-2488))))))))))
      (loop_5-1345-1442-2587 clone-2464 clone-2463 clone-2462 clone-2461)))
(define (loop_5-1345-1442-2587 clone-2591 clone-2590 clone-2589 clone-2588)
  (if (equal? 'l clone-2591)
      (let ((var-2593 (cdr clone-2590)))
        (let ((var-2595 (stream-cdr clone-2588)))
          (let ((var-2598 (cons var-2593 clone-2589)))
            (let ((var-2600 (stream-car var-2595)))
              (let ((var-2602 (car var-2600)))
                (let ((var-4044 (loop_5-1345-1442-2607 var-2602
                                                       var-2600
                                                       var-2598
                                                       var-2595)))
                  (let ((var-4045 (result-lhs var-4044)))
                    (let ((var-4046 (result-dot var-4044)))
                      (let ((var-4047 (result-att var-4044)))
                        (let ((var-4048 (result-inp var-4044)))
                          (parse-bar-1341-1495-2489 var-4046
                                                    var-4045
                                                    var-4047
                                                    clone-2589
                                                    var-4048)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-2591)))
(define (loop_5-1345-1442-2607 clone-2611 clone-2610 clone-2609 clone-2608)
  (if (equal? 'n clone-2611)
      (let ((var-2613 (cdr clone-2610)))
        (let ((var-2615 (stream-cdr clone-2608)))
          (let ((var-2618 (cons var-2613 clone-2609)))
            (let ((var-2626 (stream-car var-2615)))
              (let ((var-2681 (inner-loop_1-1351-1807-2631 var-2626
                                                           var-2615
                                                           var-2618
                                                           var-2615)))
                (let ((var-2682 (result-lhs var-2681)))
                  (let ((var-2683 (result-dot var-2681)))
                    (let ((var-2684 (result-att var-2681)))
                      (let ((var-2685 (result-inp var-2681)))
                        (parse-bar-1341-1495-2686 var-2683
                                                  var-2682
                                                  var-2684
                                                  clone-2609
                                                  var-2685))))))))))
      (loop_5-1345-1442-4015 clone-2611 clone-2610 clone-2609 clone-2608)))
(define (loop_5-1345-1442-4015 clone-4019 clone-4018 clone-4017 clone-4016)
  (if (equal? 'l clone-4019)
      (let ((var-4021 (cdr clone-4018)))
        (let ((var-4023 (stream-cdr clone-4016)))
          (let ((var-4026 (cons var-4021 clone-4017)))
            (let ((var-4028 (stream-car var-4023)))
              (let ((var-4030 (car var-4028)))
                (let ((var-4035 (loop_5-1345-1442-3404 var-4030
                                                       var-4028
                                                       var-4026
                                                       var-4023)))
                  (let ((var-4036 (result-lhs var-4035)))
                    (let ((var-4037 (result-dot var-4035)))
                      (let ((var-4038 (result-att var-4035)))
                        (let ((var-4039 (result-inp var-4035)))
                          (parse-bar-1341-1495-2686 var-4037
                                                    var-4036
                                                    var-4038
                                                    clone-4017
                                                    var-4039)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-4019)))
(define (parse-bar-1341-1495-2686 clone-2691
                                  clone-2690
                                  clone-2689
                                  clone-2688
                                  clone-2687)
  (if (< 1 clone-2691)
      (parse-result clone-2690 (- clone-2691 1) clone-2689 clone-2687)
      (loop_6-1343-1543-2696 clone-2690 clone-2689 clone-2688 clone-2687)))
(define (loop_6-1343-1543-2696 clone-2700 clone-2699 clone-2698 clone-2697)
  (if (equal? 'e clone-2700)
      (let ((var-2706 (cons clone-2699 clone-2698)))
        (let ((var-2708 (stream-car clone-2697)))
          (let ((var-2710 (car var-2708)))
            (let ((var-2777 (loop_5-1345-1442-2715 var-2710
                                                   var-2708
                                                   var-2706
                                                   clone-2697)))
              (let ((var-2778 (result-lhs var-2777)))
                (let ((var-2779 (result-dot var-2777)))
                  (let ((var-2780 (result-att var-2777)))
                    (let ((var-2781 (result-inp var-2777)))
                      (parse-bar-1341-1495-2686 var-2779
                                                var-2778
                                                var-2780
                                                clone-2698
                                                var-2781)))))))))
      (loop_6-1343-1543-2786 clone-2700 clone-2699 clone-2698 clone-2697)))
(define (loop_6-1343-1543-2786 clone-2790 clone-2789 clone-2788 clone-2787)
  (if (equal? 't clone-2790)
      (let ((var-2796 (cons clone-2789 clone-2788)))
        (let ((var-2804 (stream-car clone-2787)))
          (let ((var-3979 (inner-loop_1-1351-1807-2809 var-2804
                                                       clone-2787
                                                       var-2796
                                                       clone-2787)))
            (let ((var-3980 (result-lhs var-3979)))
              (let ((var-3981 (result-dot var-3979)))
                (let ((var-3982 (result-att var-3979)))
                  (let ((var-3983 (result-inp var-3979)))
                    (parse-bar-1341-1495-2686 var-3981
                                              var-3980
                                              var-3982
                                              clone-2788
                                              var-3983))))))))
      (let ((var-3993 (cons clone-2789 clone-2788)))
        (let ((var-4001 (stream-car clone-2787)))
          (let ((var-4006 (inner-loop_1-1351-1807-3006 var-4001
                                                       clone-2787
                                                       var-3993
                                                       clone-2787)))
            (let ((var-4007 (result-lhs var-4006)))
              (let ((var-4008 (result-dot var-4006)))
                (let ((var-4009 (result-att var-4006)))
                  (let ((var-4010 (result-inp var-4006)))
                    (parse-bar-1341-1495-2686 var-4008
                                              var-4007
                                              var-4009
                                              clone-2788
                                              var-4010))))))))))
(define (inner-loop_1-1351-1807-2809 clone-2813
                                     clone-2812
                                     clone-2811
                                     clone-2810)
  (let ((var-2815 (equal? clone-2813 'r)))
    (if (loop_4-1353-1823-2658 var-2815 clone-2813)
        (let ((var-2822 (apply (lambda ($1) $1)
                               (cons (car clone-2811)
                                     '()))))
          (parse-result 'e 1 var-2822 clone-2812))
        (let ((var-2829 (stream-car clone-2812)))
          (let ((var-2831 (car var-2829)))
            (loop_2-1356-1661-2836 var-2831 var-2829 clone-2811 clone-2812))))))
(define (loop_2-1356-1661-2836 clone-2840 clone-2839 clone-2838 clone-2837)
  (if (equal? '- clone-2840)
      (let ((var-2842 (cdr clone-2839)))
        (let ((var-2844 (stream-cdr clone-2837)))
          (let ((var-2847 (cons var-2842 clone-2838)))
            (let ((var-2849 (stream-car var-2844)))
              (let ((var-2851 (car var-2849)))
                (let ((var-3753 (loop_5-1345-1442-2856 var-2851
                                                       var-2849
                                                       var-2847
                                                       var-2844)))
                  (let ((var-3754 (result-lhs var-3753)))
                    (let ((var-3755 (result-dot var-3753)))
                      (let ((var-3756 (result-att var-3753)))
                        (let ((var-3757 (result-inp var-3753)))
                          (parse-result var-3754
                                        (- var-3755 1)
                                        var-3756
                                        var-3757)))))))))))
      (loop_2-1356-1661-3762 clone-2840 clone-2839 clone-2838 clone-2837)))
(define (loop_2-1356-1661-3762 clone-3766 clone-3765 clone-3764 clone-3763)
  (if (equal? '+ clone-3766)
      (let ((var-3768 (cdr clone-3765)))
        (let ((var-3770 (stream-cdr clone-3763)))
          (let ((var-3773 (cons var-3768 clone-3764)))
            (let ((var-3775 (stream-car var-3770)))
              (let ((var-3777 (car var-3775)))
                (let ((var-3970 (loop_5-1345-1442-3782 var-3777
                                                       var-3775
                                                       var-3773
                                                       var-3770)))
                  (let ((var-3971 (result-lhs var-3970)))
                    (let ((var-3972 (result-dot var-3970)))
                      (let ((var-3973 (result-att var-3970)))
                        (let ((var-3974 (result-inp var-3970)))
                          (parse-result var-3971
                                        (- var-3972 1)
                                        var-3973
                                        var-3974)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-3766)))
(define (loop_5-1345-1442-3782 clone-3786 clone-3785 clone-3784 clone-3783)
  (if (equal? 'n clone-3786)
      (let ((var-3788 (cdr clone-3785)))
        (let ((var-3790 (stream-cdr clone-3783)))
          (let ((var-3793 (cons var-3788 clone-3784)))
            (let ((var-3801 (stream-car var-3790)))
              (let ((var-3806 (inner-loop_1-1351-1807-2631 var-3801
                                                           var-3790
                                                           var-3793
                                                           var-3790)))
                (let ((var-3807 (result-lhs var-3806)))
                  (let ((var-3808 (result-dot var-3806)))
                    (let ((var-3809 (result-att var-3806)))
                      (let ((var-3810 (result-inp var-3806)))
                        (parse-bar-1341-1495-3811 var-3808
                                                  var-3807
                                                  var-3809
                                                  clone-3784
                                                  var-3810))))))))))
      (loop_5-1345-1442-3941 clone-3786 clone-3785 clone-3784 clone-3783)))
(define (loop_5-1345-1442-3941 clone-3945 clone-3944 clone-3943 clone-3942)
  (if (equal? 'l clone-3945)
      (let ((var-3947 (cdr clone-3944)))
        (let ((var-3949 (stream-cdr clone-3942)))
          (let ((var-3952 (cons var-3947 clone-3943)))
            (let ((var-3954 (stream-car var-3949)))
              (let ((var-3956 (car var-3954)))
                (let ((var-3961 (loop_5-1345-1442-3404 var-3956
                                                       var-3954
                                                       var-3952
                                                       var-3949)))
                  (let ((var-3962 (result-lhs var-3961)))
                    (let ((var-3963 (result-dot var-3961)))
                      (let ((var-3964 (result-att var-3961)))
                        (let ((var-3965 (result-inp var-3961)))
                          (parse-bar-1341-1495-3811 var-3963
                                                    var-3962
                                                    var-3964
                                                    clone-3943
                                                    var-3965)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-3945)))
(define (parse-bar-1341-1495-3811 clone-3816
                                  clone-3815
                                  clone-3814
                                  clone-3813
                                  clone-3812)
  (if (< 1 clone-3816)
      (parse-result clone-3815 (- clone-3816 1) clone-3814 clone-3812)
      (loop_6-1343-1543-3821 clone-3815 clone-3814 clone-3813 clone-3812)))
(define (loop_6-1343-1543-3821 clone-3825 clone-3824 clone-3823 clone-3822)
  (if (equal? 'e clone-3825)
      (let ((var-3831 (cons clone-3824 clone-3823)))
        (let ((var-3839 (stream-car clone-3822)))
          (let ((var-3873 (inner-loop_1-1351-1807-3844 var-3839
                                                       clone-3822
                                                       var-3831
                                                       clone-3822)))
            (let ((var-3874 (result-lhs var-3873)))
              (let ((var-3875 (result-dot var-3873)))
                (let ((var-3876 (result-att var-3873)))
                  (let ((var-3877 (result-inp var-3873)))
                    (parse-bar-1341-1495-3811 var-3875
                                              var-3874
                                              var-3876
                                              clone-3823
                                              var-3877))))))))
      (loop_6-1343-1543-3882 clone-3825 clone-3824 clone-3823 clone-3822)))
(define (loop_6-1343-1543-3882 clone-3886 clone-3885 clone-3884 clone-3883)
  (if (equal? 't clone-3886)
      (let ((var-3892 (cons clone-3885 clone-3884)))
        (let ((var-3900 (stream-car clone-3883)))
          (let ((var-3905 (inner-loop_1-1351-1807-2809 var-3900
                                                       clone-3883
                                                       var-3892
                                                       clone-3883)))
            (let ((var-3906 (result-lhs var-3905)))
              (let ((var-3907 (result-dot var-3905)))
                (let ((var-3908 (result-att var-3905)))
                  (let ((var-3909 (result-inp var-3905)))
                    (parse-bar-1341-1495-3811 var-3907
                                              var-3906
                                              var-3908
                                              clone-3884
                                              var-3909))))))))
      (let ((var-3919 (cons clone-3885 clone-3884)))
        (let ((var-3927 (stream-car clone-3883)))
          (let ((var-3932 (inner-loop_1-1351-1807-3006 var-3927
                                                       clone-3883
                                                       var-3919
                                                       clone-3883)))
            (let ((var-3933 (result-lhs var-3932)))
              (let ((var-3934 (result-dot var-3932)))
                (let ((var-3935 (result-att var-3932)))
                  (let ((var-3936 (result-inp var-3932)))
                    (parse-bar-1341-1495-3811 var-3934
                                              var-3933
                                              var-3935
                                              clone-3884
                                              var-3936))))))))))
(define (inner-loop_1-1351-1807-3844 clone-3848
                                     clone-3847
                                     clone-3846
                                     clone-3845)
  (let ((var-3850 (equal? clone-3848 'r)))
    (if (loop_4-1353-1823-2658 var-3850 clone-3848)
        (let ((var-3857 (cdr clone-3846)))
          (let ((var-3858 (cdr var-3857)))
            (let ((var-3859 (apply (lambda ($1 $2 $3) (+ $1 $3))
                                   (cons (car var-3858)
                                         (cons (car var-3857)
                                               (cons (car clone-3846)
                                                     '()))))))
              (parse-result 'e 3 var-3859 clone-3847))))
        (let ((var-3866 (stream-car clone-3847)))
          (let ((var-3868 (car var-3866)))
            (_sim-error 'direct-parse "can't shift on" var-3868))))))
(define (loop_5-1345-1442-2856 clone-2860 clone-2859 clone-2858 clone-2857)
  (if (equal? 'n clone-2860)
      (let ((var-2862 (cdr clone-2859)))
        (let ((var-2864 (stream-cdr clone-2857)))
          (let ((var-2867 (cons var-2862 clone-2858)))
            (let ((var-2875 (stream-car var-2864)))
              (let ((var-2880 (inner-loop_1-1351-1807-2631 var-2875
                                                           var-2864
                                                           var-2867
                                                           var-2864)))
                (let ((var-2881 (result-lhs var-2880)))
                  (let ((var-2882 (result-dot var-2880)))
                    (let ((var-2883 (result-att var-2880)))
                      (let ((var-2884 (result-inp var-2880)))
                        (parse-bar-1341-1495-2885 var-2882
                                                  var-2881
                                                  var-2883
                                                  clone-2858
                                                  var-2884))))))))))
      (loop_5-1345-1442-3724 clone-2860 clone-2859 clone-2858 clone-2857)))
(define (loop_5-1345-1442-3724 clone-3728 clone-3727 clone-3726 clone-3725)
  (if (equal? 'l clone-3728)
      (let ((var-3730 (cdr clone-3727)))
        (let ((var-3732 (stream-cdr clone-3725)))
          (let ((var-3735 (cons var-3730 clone-3726)))
            (let ((var-3737 (stream-car var-3732)))
              (let ((var-3739 (car var-3737)))
                (let ((var-3744 (loop_5-1345-1442-3404 var-3739
                                                       var-3737
                                                       var-3735
                                                       var-3732)))
                  (let ((var-3745 (result-lhs var-3744)))
                    (let ((var-3746 (result-dot var-3744)))
                      (let ((var-3747 (result-att var-3744)))
                        (let ((var-3748 (result-inp var-3744)))
                          (parse-bar-1341-1495-2885 var-3746
                                                    var-3745
                                                    var-3747
                                                    clone-3726
                                                    var-3748)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-3728)))
(define (parse-bar-1341-1495-2885 clone-2890
                                  clone-2889
                                  clone-2888
                                  clone-2887
                                  clone-2886)
  (if (< 1 clone-2890)
      (parse-result clone-2889 (- clone-2890 1) clone-2888 clone-2886)
      (loop_6-1343-1543-2895 clone-2889 clone-2888 clone-2887 clone-2886)))
(define (loop_6-1343-1543-2895 clone-2899 clone-2898 clone-2897 clone-2896)
  (if (equal? 'e clone-2899)
      (let ((var-2905 (cons clone-2898 clone-2897)))
        (let ((var-2913 (stream-car clone-2896)))
          (let ((var-2947 (inner-loop_1-1351-1807-2918 var-2913
                                                       clone-2896
                                                       var-2905
                                                       clone-2896)))
            (let ((var-2948 (result-lhs var-2947)))
              (let ((var-2949 (result-dot var-2947)))
                (let ((var-2950 (result-att var-2947)))
                  (let ((var-2951 (result-inp var-2947)))
                    (parse-bar-1341-1495-2885 var-2949
                                              var-2948
                                              var-2950
                                              clone-2897
                                              var-2951))))))))
      (loop_6-1343-1543-2956 clone-2899 clone-2898 clone-2897 clone-2896)))
(define (loop_6-1343-1543-2956 clone-2960 clone-2959 clone-2958 clone-2957)
  (if (equal? 't clone-2960)
      (let ((var-2966 (cons clone-2959 clone-2958)))
        (let ((var-2974 (stream-car clone-2957)))
          (let ((var-2979 (inner-loop_1-1351-1807-2809 var-2974
                                                       clone-2957
                                                       var-2966
                                                       clone-2957)))
            (let ((var-2980 (result-lhs var-2979)))
              (let ((var-2981 (result-dot var-2979)))
                (let ((var-2982 (result-att var-2979)))
                  (let ((var-2983 (result-inp var-2979)))
                    (parse-bar-1341-1495-2885 var-2981
                                              var-2980
                                              var-2982
                                              clone-2958
                                              var-2983))))))))
      (let ((var-2993 (cons clone-2959 clone-2958)))
        (let ((var-3001 (stream-car clone-2957)))
          (let ((var-3715 (inner-loop_1-1351-1807-3006 var-3001
                                                       clone-2957
                                                       var-2993
                                                       clone-2957)))
            (let ((var-3716 (result-lhs var-3715)))
              (let ((var-3717 (result-dot var-3715)))
                (let ((var-3718 (result-att var-3715)))
                  (let ((var-3719 (result-inp var-3715)))
                    (parse-bar-1341-1495-2885 var-3717
                                              var-3716
                                              var-3718
                                              clone-2958
                                              var-3719))))))))))
(define (inner-loop_1-1351-1807-3006 clone-3010
                                     clone-3009
                                     clone-3008
                                     clone-3007)
  (let ((var-3012 (equal? clone-3010 '+)))
    (if (loop_4-1353-1823-3013 var-3012 clone-3010)
        (let ((var-3029 (apply (lambda ($1) $1)
                               (cons (car clone-3008)
                                     '()))))
          (parse-result 't 1 var-3029 clone-3009))
        (let ((var-3036 (stream-car clone-3009)))
          (let ((var-3038 (car var-3036)))
            (loop_2-1356-1661-3043 var-3038 var-3036 clone-3008 clone-3009))))))
(define (loop_2-1356-1661-3043 clone-3047 clone-3046 clone-3045 clone-3044)
  (if (equal? '/ clone-3047)
      (let ((var-3049 (cdr clone-3046)))
        (let ((var-3051 (stream-cdr clone-3044)))
          (let ((var-3054 (cons var-3049 clone-3045)))
            (let ((var-3056 (stream-car var-3051)))
              (let ((var-3058 (car var-3056)))
                (let ((var-3677 (loop_5-1345-1442-3063 var-3058
                                                       var-3056
                                                       var-3054
                                                       var-3051)))
                  (let ((var-3678 (result-lhs var-3677)))
                    (let ((var-3679 (result-dot var-3677)))
                      (let ((var-3680 (result-att var-3677)))
                        (let ((var-3681 (result-inp var-3677)))
                          (parse-result var-3678
                                        (- var-3679 1)
                                        var-3680
                                        var-3681)))))))))))
      (loop_2-1356-1661-3686 clone-3047 clone-3046 clone-3045 clone-3044)))
(define (loop_2-1356-1661-3686 clone-3690 clone-3689 clone-3688 clone-3687)
  (if (equal? '* clone-3690)
      (let ((var-3692 (cdr clone-3689)))
        (let ((var-3694 (stream-cdr clone-3687)))
          (let ((var-3697 (cons var-3692 clone-3688)))
            (let ((var-3699 (stream-car var-3694)))
              (let ((var-3701 (car var-3699)))
                (let ((var-3706 (loop_5-1345-1442-3257 var-3701
                                                       var-3699
                                                       var-3697
                                                       var-3694)))
                  (let ((var-3707 (result-lhs var-3706)))
                    (let ((var-3708 (result-dot var-3706)))
                      (let ((var-3709 (result-att var-3706)))
                        (let ((var-3710 (result-inp var-3706)))
                          (parse-result var-3707
                                        (- var-3708 1)
                                        var-3709
                                        var-3710)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-3690)))
(define (loop_5-1345-1442-3063 clone-3067 clone-3066 clone-3065 clone-3064)
  (if (equal? 'n clone-3067)
      (let ((var-3069 (cdr clone-3066)))
        (let ((var-3071 (stream-cdr clone-3064)))
          (let ((var-3074 (cons var-3069 clone-3065)))
            (let ((var-3082 (stream-car var-3071)))
              (let ((var-3087 (inner-loop_1-1351-1807-2631 var-3082
                                                           var-3071
                                                           var-3074
                                                           var-3071)))
                (let ((var-3088 (result-lhs var-3087)))
                  (let ((var-3089 (result-dot var-3087)))
                    (let ((var-3090 (result-att var-3087)))
                      (let ((var-3091 (result-inp var-3087)))
                        (parse-bar-1341-1495-3092 var-3089
                                                  var-3088
                                                  var-3090
                                                  clone-3065
                                                  var-3091))))))))))
      (loop_5-1345-1442-3648 clone-3067 clone-3066 clone-3065 clone-3064)))
(define (loop_5-1345-1442-3648 clone-3652 clone-3651 clone-3650 clone-3649)
  (if (equal? 'l clone-3652)
      (let ((var-3654 (cdr clone-3651)))
        (let ((var-3656 (stream-cdr clone-3649)))
          (let ((var-3659 (cons var-3654 clone-3650)))
            (let ((var-3661 (stream-car var-3656)))
              (let ((var-3663 (car var-3661)))
                (let ((var-3668 (loop_5-1345-1442-3404 var-3663
                                                       var-3661
                                                       var-3659
                                                       var-3656)))
                  (let ((var-3669 (result-lhs var-3668)))
                    (let ((var-3670 (result-dot var-3668)))
                      (let ((var-3671 (result-att var-3668)))
                        (let ((var-3672 (result-inp var-3668)))
                          (parse-bar-1341-1495-3092 var-3670
                                                    var-3669
                                                    var-3671
                                                    clone-3650
                                                    var-3672)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-3652)))
(define (parse-bar-1341-1495-3092 clone-3097
                                  clone-3096
                                  clone-3095
                                  clone-3094
                                  clone-3093)
  (if (< 1 clone-3097)
      (parse-result clone-3096 (- clone-3097 1) clone-3095 clone-3093)
      (loop_6-1343-1543-3102 clone-3096 clone-3095 clone-3094 clone-3093)))
(define (loop_6-1343-1543-3102 clone-3106 clone-3105 clone-3104 clone-3103)
  (if (equal? 't clone-3106)
      (let ((var-3112 (cons clone-3105 clone-3104)))
        (let ((var-3120 (stream-car clone-3103)))
          (let ((var-3154 (inner-loop_1-1351-1807-3125 var-3120
                                                       clone-3103
                                                       var-3112
                                                       clone-3103)))
            (let ((var-3155 (result-lhs var-3154)))
              (let ((var-3156 (result-dot var-3154)))
                (let ((var-3157 (result-att var-3154)))
                  (let ((var-3158 (result-inp var-3154)))
                    (parse-bar-1341-1495-3092 var-3156
                                              var-3155
                                              var-3157
                                              clone-3104
                                              var-3158))))))))
      (let ((var-3168 (cons clone-3105 clone-3104)))
        (let ((var-3176 (stream-car clone-3103)))
          (let ((var-3639 (inner-loop_1-1351-1807-3181 var-3176
                                                       clone-3103
                                                       var-3168
                                                       clone-3103)))
            (let ((var-3640 (result-lhs var-3639)))
              (let ((var-3641 (result-dot var-3639)))
                (let ((var-3642 (result-att var-3639)))
                  (let ((var-3643 (result-inp var-3639)))
                    (parse-bar-1341-1495-3092 var-3641
                                              var-3640
                                              var-3642
                                              clone-3104
                                              var-3643))))))))))
(define (inner-loop_1-1351-1807-3181 clone-3185
                                     clone-3184
                                     clone-3183
                                     clone-3182)
  (let ((var-3187 (equal? clone-3185 '+)))
    (if (loop_4-1353-1823-3013 var-3187 clone-3185)
        (let ((var-3194 (apply (lambda ($1) $1)
                               (cons (car clone-3183)
                                     '()))))
          (parse-result 't 1 var-3194 clone-3184))
        (let ((var-3201 (stream-car clone-3184)))
          (let ((var-3203 (car var-3201)))
            (loop_2-1356-1661-3208 var-3203 var-3201 clone-3183 clone-3184))))))
(define (loop_2-1356-1661-3208 clone-3212 clone-3211 clone-3210 clone-3209)
  (if (equal? '/ clone-3212)
      (let ((var-3214 (cdr clone-3211)))
        (let ((var-3216 (stream-cdr clone-3209)))
          (let ((var-3219 (cons var-3214 clone-3210)))
            (let ((var-3221 (stream-car var-3216)))
              (let ((var-3223 (car var-3221)))
                (let ((var-3228 (loop_5-1345-1442-3063 var-3223
                                                       var-3221
                                                       var-3219
                                                       var-3216)))
                  (let ((var-3229 (result-lhs var-3228)))
                    (let ((var-3230 (result-dot var-3228)))
                      (let ((var-3231 (result-att var-3228)))
                        (let ((var-3232 (result-inp var-3228)))
                          (parse-result var-3229
                                        (- var-3230 1)
                                        var-3231
                                        var-3232)))))))))))
      (loop_2-1356-1661-3237 clone-3212 clone-3211 clone-3210 clone-3209)))
(define (loop_2-1356-1661-3237 clone-3241 clone-3240 clone-3239 clone-3238)
  (if (equal? '* clone-3241)
      (let ((var-3243 (cdr clone-3240)))
        (let ((var-3245 (stream-cdr clone-3238)))
          (let ((var-3248 (cons var-3243 clone-3239)))
            (let ((var-3250 (stream-car var-3245)))
              (let ((var-3252 (car var-3250)))
                (let ((var-3630 (loop_5-1345-1442-3257 var-3252
                                                       var-3250
                                                       var-3248
                                                       var-3245)))
                  (let ((var-3631 (result-lhs var-3630)))
                    (let ((var-3632 (result-dot var-3630)))
                      (let ((var-3633 (result-att var-3630)))
                        (let ((var-3634 (result-inp var-3630)))
                          (parse-result var-3631
                                        (- var-3632 1)
                                        var-3633
                                        var-3634)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-3241)))
(define (loop_5-1345-1442-3257 clone-3261 clone-3260 clone-3259 clone-3258)
  (if (equal? 'n clone-3261)
      (let ((var-3263 (cdr clone-3260)))
        (let ((var-3265 (stream-cdr clone-3258)))
          (let ((var-3268 (cons var-3263 clone-3259)))
            (let ((var-3276 (stream-car var-3265)))
              (let ((var-3281 (inner-loop_1-1351-1807-2631 var-3276
                                                           var-3265
                                                           var-3268
                                                           var-3265)))
                (let ((var-3282 (result-lhs var-3281)))
                  (let ((var-3283 (result-dot var-3281)))
                    (let ((var-3284 (result-att var-3281)))
                      (let ((var-3285 (result-inp var-3281)))
                        (parse-bar-1341-1495-3286 var-3283
                                                  var-3282
                                                  var-3284
                                                  clone-3259
                                                  var-3285))))))))))
      (loop_5-1345-1442-3384 clone-3261 clone-3260 clone-3259 clone-3258)))
(define (loop_5-1345-1442-3384 clone-3388 clone-3387 clone-3386 clone-3385)
  (if (equal? 'l clone-3388)
      (let ((var-3390 (cdr clone-3387)))
        (let ((var-3392 (stream-cdr clone-3385)))
          (let ((var-3395 (cons var-3390 clone-3386)))
            (let ((var-3397 (stream-car var-3392)))
              (let ((var-3399 (car var-3397)))
                (let ((var-3621 (loop_5-1345-1442-3404 var-3399
                                                       var-3397
                                                       var-3395
                                                       var-3392)))
                  (let ((var-3622 (result-lhs var-3621)))
                    (let ((var-3623 (result-dot var-3621)))
                      (let ((var-3624 (result-att var-3621)))
                        (let ((var-3625 (result-inp var-3621)))
                          (parse-bar-1341-1495-3286 var-3623
                                                    var-3622
                                                    var-3624
                                                    clone-3386
                                                    var-3625)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-3388)))
(define (loop_5-1345-1442-3404 clone-3408 clone-3407 clone-3406 clone-3405)
  (if (equal? 'n clone-3408)
      (let ((var-3410 (cdr clone-3407)))
        (let ((var-3412 (stream-cdr clone-3405)))
          (let ((var-3415 (cons var-3410 clone-3406)))
            (let ((var-3423 (stream-car var-3412)))
              (let ((var-3428 (inner-loop_1-1351-1807-2631 var-3423
                                                           var-3412
                                                           var-3415
                                                           var-3412)))
                (let ((var-3429 (result-lhs var-3428)))
                  (let ((var-3430 (result-dot var-3428)))
                    (let ((var-3431 (result-att var-3428)))
                      (let ((var-3432 (result-inp var-3428)))
                        (parse-bar-1341-1495-3433 var-3430
                                                  var-3429
                                                  var-3431
                                                  clone-3406
                                                  var-3432))))))))))
      (loop_5-1345-1442-3592 clone-3408 clone-3407 clone-3406 clone-3405)))
(define (loop_5-1345-1442-3592 clone-3596 clone-3595 clone-3594 clone-3593)
  (if (equal? 'l clone-3596)
      (let ((var-3598 (cdr clone-3595)))
        (let ((var-3600 (stream-cdr clone-3593)))
          (let ((var-3603 (cons var-3598 clone-3594)))
            (let ((var-3605 (stream-car var-3600)))
              (let ((var-3607 (car var-3605)))
                (let ((var-3612 (loop_5-1345-1442-3404 var-3607
                                                       var-3605
                                                       var-3603
                                                       var-3600)))
                  (let ((var-3613 (result-lhs var-3612)))
                    (let ((var-3614 (result-dot var-3612)))
                      (let ((var-3615 (result-att var-3612)))
                        (let ((var-3616 (result-inp var-3612)))
                          (parse-bar-1341-1495-3433 var-3614
                                                    var-3613
                                                    var-3615
                                                    clone-3594
                                                    var-3616)))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-3596)))
(define (parse-bar-1341-1495-3433 clone-3438
                                  clone-3437
                                  clone-3436
                                  clone-3435
                                  clone-3434)
  (if (< 1 clone-3438)
      (parse-result clone-3437 (- clone-3438 1) clone-3436 clone-3434)
      (loop_6-1343-1543-3443 clone-3437 clone-3436 clone-3435 clone-3434)))
(define (loop_6-1343-1543-3443 clone-3447 clone-3446 clone-3445 clone-3444)
  (if (equal? 'e clone-3447)
      (let ((var-3453 (cons clone-3446 clone-3445)))
        (let ((var-3455 (stream-car clone-3444)))
          (let ((var-3457 (car var-3455)))
            (let ((var-3524 (loop_5-1345-1442-3462 var-3457
                                                   var-3455
                                                   var-3453
                                                   clone-3444)))
              (let ((var-3525 (result-lhs var-3524)))
                (let ((var-3526 (result-dot var-3524)))
                  (let ((var-3527 (result-att var-3524)))
                    (let ((var-3528 (result-inp var-3524)))
                      (parse-bar-1341-1495-3433 var-3526
                                                var-3525
                                                var-3527
                                                clone-3445
                                                var-3528)))))))))
      (loop_6-1343-1543-3533 clone-3447 clone-3446 clone-3445 clone-3444)))
(define (loop_6-1343-1543-3533 clone-3537 clone-3536 clone-3535 clone-3534)
  (if (equal? 't clone-3537)
      (let ((var-3543 (cons clone-3536 clone-3535)))
        (let ((var-3551 (stream-car clone-3534)))
          (let ((var-3556 (inner-loop_1-1351-1807-2809 var-3551
                                                       clone-3534
                                                       var-3543
                                                       clone-3534)))
            (let ((var-3557 (result-lhs var-3556)))
              (let ((var-3558 (result-dot var-3556)))
                (let ((var-3559 (result-att var-3556)))
                  (let ((var-3560 (result-inp var-3556)))
                    (parse-bar-1341-1495-3433 var-3558
                                              var-3557
                                              var-3559
                                              clone-3535
                                              var-3560))))))))
      (let ((var-3570 (cons clone-3536 clone-3535)))
        (let ((var-3578 (stream-car clone-3534)))
          (let ((var-3583 (inner-loop_1-1351-1807-3006 var-3578
                                                       clone-3534
                                                       var-3570
                                                       clone-3534)))
            (let ((var-3584 (result-lhs var-3583)))
              (let ((var-3585 (result-dot var-3583)))
                (let ((var-3586 (result-att var-3583)))
                  (let ((var-3587 (result-inp var-3583)))
                    (parse-bar-1341-1495-3433 var-3585
                                              var-3584
                                              var-3586
                                              clone-3535
                                              var-3587))))))))))
(define (loop_5-1345-1442-3462 clone-3466 clone-3465 clone-3464 clone-3463)
  (if (equal? 'r clone-3466)
      (let ((var-3468 (cdr clone-3465)))
        (let ((var-3470 (stream-cdr clone-3463)))
          (let ((var-3473 (cons var-3468 clone-3464)))
            (let ((var-3481 (stream-car var-3470)))
              (let ((var-3515 (inner-loop_1-1351-1807-3486 var-3481
                                                           var-3470
                                                           var-3473
                                                           var-3470)))
                (let ((var-3516 (result-lhs var-3515)))
                  (let ((var-3517 (result-dot var-3515)))
                    (let ((var-3518 (result-att var-3515)))
                      (let ((var-3519 (result-inp var-3515)))
                        (parse-result var-3516
                                      (- var-3517 1)
                                      var-3518
                                      var-3519))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-3466)))
(define (inner-loop_1-1351-1807-3486 clone-3490
                                     clone-3489
                                     clone-3488
                                     clone-3487)
  (let ((var-3492 (equal? clone-3490 '*)))
    (if (loop_4-1353-1823-2638 var-3492 clone-3490)
        (let ((var-3499 (cdr clone-3488)))
          (let ((var-3500 (cdr var-3499)))
            (let ((var-3501 (apply (lambda ($1 $2 $3) $2)
                                   (cons (car var-3500)
                                         (cons (car var-3499)
                                               (cons (car clone-3488)
                                                     '()))))))
              (parse-result 'p 3 var-3501 clone-3489))))
        (let ((var-3508 (stream-car clone-3489)))
          (let ((var-3510 (car var-3508)))
            (_sim-error 'direct-parse "can't shift on" var-3510))))))
(define (parse-bar-1341-1495-3286 clone-3291
                                  clone-3290
                                  clone-3289
                                  clone-3288
                                  clone-3287)
  (if (< 1 clone-3291)
      (parse-result clone-3290 (- clone-3291 1) clone-3289 clone-3287)
      (loop_6-1343-1543-3296 clone-3290 clone-3289 clone-3288 clone-3287)))
(define (loop_6-1343-1543-3296 clone-3300 clone-3299 clone-3298 clone-3297)
  (if (equal? 't clone-3300)
      (let ((var-3306 (cons clone-3299 clone-3298)))
        (let ((var-3314 (stream-car clone-3297)))
          (let ((var-3348 (inner-loop_1-1351-1807-3319 var-3314
                                                       clone-3297
                                                       var-3306
                                                       clone-3297)))
            (let ((var-3349 (result-lhs var-3348)))
              (let ((var-3350 (result-dot var-3348)))
                (let ((var-3351 (result-att var-3348)))
                  (let ((var-3352 (result-inp var-3348)))
                    (parse-bar-1341-1495-3286 var-3350
                                              var-3349
                                              var-3351
                                              clone-3298
                                              var-3352))))))))
      (let ((var-3362 (cons clone-3299 clone-3298)))
        (let ((var-3370 (stream-car clone-3297)))
          (let ((var-3375 (inner-loop_1-1351-1807-3181 var-3370
                                                       clone-3297
                                                       var-3362
                                                       clone-3297)))
            (let ((var-3376 (result-lhs var-3375)))
              (let ((var-3377 (result-dot var-3375)))
                (let ((var-3378 (result-att var-3375)))
                  (let ((var-3379 (result-inp var-3375)))
                    (parse-bar-1341-1495-3286 var-3377
                                              var-3376
                                              var-3378
                                              clone-3298
                                              var-3379))))))))))
(define (inner-loop_1-1351-1807-3319 clone-3323
                                     clone-3322
                                     clone-3321
                                     clone-3320)
  (let ((var-3325 (equal? clone-3323 '+)))
    (if (loop_4-1353-1823-3013 var-3325 clone-3323)
        (let ((var-3332 (cdr clone-3321)))
          (let ((var-3333 (cdr var-3332)))
            (let ((var-3334 (apply (lambda ($1 $2 $3) (* $1 $3))
                                   (cons (car var-3333)
                                         (cons (car var-3332)
                                               (cons (car clone-3321)
                                                     '()))))))
              (parse-result 't 3 var-3334 clone-3322))))
        (let ((var-3341 (stream-car clone-3322)))
          (let ((var-3343 (car var-3341)))
            (_sim-error 'direct-parse "can't shift on" var-3343))))))
(define (inner-loop_1-1351-1807-3125 clone-3129
                                     clone-3128
                                     clone-3127
                                     clone-3126)
  (let ((var-3131 (equal? clone-3129 '+)))
    (if (loop_4-1353-1823-3013 var-3131 clone-3129)
        (let ((var-3138 (cdr clone-3127)))
          (let ((var-3139 (cdr var-3138)))
            (let ((var-3140 (apply (lambda ($1 $2 $3) (/ $1 $3))
                                   (cons (car var-3139)
                                         (cons (car var-3138)
                                               (cons (car clone-3127)
                                                     '()))))))
              (parse-result 't 3 var-3140 clone-3128))))
        (let ((var-3147 (stream-car clone-3128)))
          (let ((var-3149 (car var-3147)))
            (_sim-error 'direct-parse "can't shift on" var-3149))))))
(define (loop_4-1353-1823-3013 clone-3015 clone-3014)
  (if clone-3015
      clone-3015
      (let ((var-3017 (equal? clone-3014 '-)))
        (loop_4-1353-1823-3018 var-3017 clone-3014))))
(define (loop_4-1353-1823-3018 clone-3020 clone-3019)
  (if clone-3020
      clone-3020
      (let ((var-3022 (equal? clone-3019 'r)))
        (loop_4-1353-1823-2658 var-3022 clone-3019))))
(define (inner-loop_1-1351-1807-2918 clone-2922
                                     clone-2921
                                     clone-2920
                                     clone-2919)
  (let ((var-2924 (equal? clone-2922 'r)))
    (if (loop_4-1353-1823-2658 var-2924 clone-2922)
        (let ((var-2931 (cdr clone-2920)))
          (let ((var-2932 (cdr var-2931)))
            (let ((var-2933 (apply (lambda ($1 $2 $3) (- $1 $3))
                                   (cons (car var-2932)
                                         (cons (car var-2931)
                                               (cons (car clone-2920)
                                                     '()))))))
              (parse-result 'e 3 var-2933 clone-2921))))
        (let ((var-2940 (stream-car clone-2921)))
          (let ((var-2942 (car var-2940)))
            (_sim-error 'direct-parse "can't shift on" var-2942))))))
(define (loop_5-1345-1442-2715 clone-2719 clone-2718 clone-2717 clone-2716)
  (if (equal? 'r clone-2719)
      (let ((var-2721 (cdr clone-2718)))
        (let ((var-2723 (stream-cdr clone-2716)))
          (let ((var-2726 (cons var-2721 clone-2717)))
            (let ((var-2734 (stream-car var-2723)))
              (let ((var-2768 (inner-loop_1-1351-1807-2739 var-2734
                                                           var-2723
                                                           var-2726
                                                           var-2723)))
                (let ((var-2769 (result-lhs var-2768)))
                  (let ((var-2770 (result-dot var-2768)))
                    (let ((var-2771 (result-att var-2768)))
                      (let ((var-2772 (result-inp var-2768)))
                        (parse-result var-2769
                                      (- var-2770 1)
                                      var-2771
                                      var-2772))))))))))
      (_sim-error 'direct-parse "can't shift on" clone-2719)))
(define (inner-loop_1-1351-1807-2739 clone-2743
                                     clone-2742
                                     clone-2741
                                     clone-2740)
  (let ((var-2745 (equal? clone-2743 '$)))
    (if (loop_4-1353-1823-1903 var-2745 clone-2743)
        (let ((var-2752 (cdr clone-2741)))
          (let ((var-2753 (cdr var-2752)))
            (let ((var-2754 (apply (lambda ($1 $2 $3) $2)
                                   (cons (car var-2753)
                                         (cons (car var-2752)
                                               (cons (car clone-2741)
                                                     '()))))))
              (parse-result 'p 3 var-2754 clone-2742))))
        (let ((var-2761 (stream-car clone-2742)))
          (let ((var-2763 (car var-2761)))
            (_sim-error 'direct-parse "can't shift on" var-2763))))))
(define (inner-loop_1-1351-1807-2631 clone-2635
                                     clone-2634
                                     clone-2633
                                     clone-2632)
  (let ((var-2637 (equal? clone-2635 '*)))
    (if (loop_4-1353-1823-2638 var-2637 clone-2635)
        (let ((var-2667 (apply (lambda ($1) $1)
                               (cons (car clone-2633)
                                     '()))))
          (parse-result 'p 1 var-2667 clone-2634))
        (let ((var-2674 (stream-car clone-2634)))
          (let ((var-2676 (car var-2674)))
            (_sim-error 'direct-parse "can't shift on" var-2676))))))
(define (loop_4-1353-1823-2638 clone-2640 clone-2639)
  (if clone-2640
      clone-2640
      (let ((var-2642 (equal? clone-2639 '+)))
        (loop_4-1353-1823-2643 var-2642 clone-2639))))
(define (loop_4-1353-1823-2643 clone-2645 clone-2644)
  (if clone-2645
      clone-2645
      (let ((var-2647 (equal? clone-2644 '-)))
        (loop_4-1353-1823-2648 var-2647 clone-2644))))
(define (loop_4-1353-1823-2648 clone-2650 clone-2649)
  (if clone-2650
      clone-2650
      (let ((var-2652 (equal? clone-2649 '/)))
        (loop_4-1353-1823-2653 var-2652 clone-2649))))
(define (loop_4-1353-1823-2653 clone-2655 clone-2654)
  (if clone-2655
      clone-2655
      (let ((var-2657 (equal? clone-2654 'r)))
        (loop_4-1353-1823-2658 var-2657 clone-2654))))
(define (loop_4-1353-1823-2658 clone-2660 clone-2659)
  (if clone-2660 clone-2660 #f))
(define (parse-bar-1341-1495-2489 clone-2494
                                  clone-2493
                                  clone-2492
                                  clone-2491
                                  clone-2490)
  (if (< 1 clone-2494)
      (parse-result clone-2493 (- clone-2494 1) clone-2492 clone-2490)
      (loop_6-1343-1543-2499 clone-2493 clone-2492 clone-2491 clone-2490)))
(define (loop_6-1343-1543-2499 clone-2503 clone-2502 clone-2501 clone-2500)
  (if (equal? 't clone-2503)
      (let ((var-2509 (cons clone-2502 clone-2501)))
        (let ((var-2517 (stream-car clone-2500)))
          (let ((var-2551 (inner-loop_1-1351-1807-2522 var-2517
                                                       clone-2500
                                                       var-2509
                                                       clone-2500)))
            (let ((var-2552 (result-lhs var-2551)))
              (let ((var-2553 (result-dot var-2551)))
                (let ((var-2554 (result-att var-2551)))
                  (let ((var-2555 (result-inp var-2551)))
                    (parse-bar-1341-1495-2489 var-2553
                                              var-2552
                                              var-2554
                                              clone-2501
                                              var-2555))))))))
      (let ((var-2565 (cons clone-2502 clone-2501)))
        (let ((var-2573 (stream-car clone-2500)))
          (let ((var-2578 (inner-loop_1-1351-1807-2384 var-2573
                                                       clone-2500
                                                       var-2565
                                                       clone-2500)))
            (let ((var-2579 (result-lhs var-2578)))
              (let ((var-2580 (result-dot var-2578)))
                (let ((var-2581 (result-att var-2578)))
                  (let ((var-2582 (result-inp var-2578)))
                    (parse-bar-1341-1495-2489 var-2580
                                              var-2579
                                              var-2581
                                              clone-2501
                                              var-2582))))))))))
(define (inner-loop_1-1351-1807-2522 clone-2526
                                     clone-2525
                                     clone-2524
                                     clone-2523)
  (let ((var-2528 (equal? clone-2526 '$)))
    (if (loop_4-1353-1823-2213 var-2528 clone-2526)
        (let ((var-2535 (cdr clone-2524)))
          (let ((var-2536 (cdr var-2535)))
            (let ((var-2537 (apply (lambda ($1 $2 $3) (* $1 $3))
                                   (cons (car var-2536)
                                         (cons (car var-2535)
                                               (cons (car clone-2524)
                                                     '()))))))
              (parse-result 't 3 var-2537 clone-2525))))
        (let ((var-2544 (stream-car clone-2525)))
          (let ((var-2546 (car var-2544)))
            (_sim-error 'direct-parse "can't shift on" var-2546))))))
(define (inner-loop_1-1351-1807-2328 clone-2332
                                     clone-2331
                                     clone-2330
                                     clone-2329)
  (let ((var-2334 (equal? clone-2332 '$)))
    (if (loop_4-1353-1823-2213 var-2334 clone-2332)
        (let ((var-2341 (cdr clone-2330)))
          (let ((var-2342 (cdr var-2341)))
            (let ((var-2343 (apply (lambda ($1 $2 $3) (/ $1 $3))
                                   (cons (car var-2342)
                                         (cons (car var-2341)
                                               (cons (car clone-2330)
                                                     '()))))))
              (parse-result 't 3 var-2343 clone-2331))))
        (let ((var-2350 (stream-car clone-2331)))
          (let ((var-2352 (car var-2350)))
            (_sim-error 'direct-parse "can't shift on" var-2352))))))
(define (loop_4-1353-1823-2213 clone-2215 clone-2214)
  (if clone-2215
      clone-2215
      (let ((var-2217 (equal? clone-2214 '+)))
        (loop_4-1353-1823-2218 var-2217 clone-2214))))
(define (loop_4-1353-1823-2218 clone-2220 clone-2219)
  (if clone-2220
      clone-2220
      (let ((var-2222 (equal? clone-2219 '-)))
        (loop_4-1353-1823-2223 var-2222 clone-2219))))
(define (loop_4-1353-1823-2223 clone-2225 clone-2224)
  (if clone-2225 clone-2225 #f))
(define (inner-loop_1-1351-1807-2118 clone-2122
                                     clone-2121
                                     clone-2120
                                     clone-2119)
  (let ((var-2124 (equal? clone-2122 '$)))
    (if (loop_4-1353-1823-2013 var-2124 clone-2122)
        (let ((var-2131 (cdr clone-2120)))
          (let ((var-2132 (cdr var-2131)))
            (let ((var-2133 (apply (lambda ($1 $2 $3) (- $1 $3))
                                   (cons (car var-2132)
                                         (cons (car var-2131)
                                               (cons (car clone-2120)
                                                     '()))))))
              (parse-result 'e 3 var-2133 clone-2121))))
        (let ((var-2140 (stream-car clone-2121)))
          (let ((var-2142 (car var-2140)))
            (_sim-error 'direct-parse "can't shift on" var-2142))))))
(define (loop_4-1353-1823-2013 clone-2015 clone-2014)
  (if clone-2015 clone-2015 #f))
(define (direct-parse-1336-1380-1973 clone-1975 clone-1974)
  (let ((var-1976 (stream-car clone-1974)))
    (if (equal? '$ (car var-1976))
        (car clone-1975)
        (let ((var-1977 (stream-car clone-1974)))
          (_sim-error 'direct-parse "expecting eof" (car var-1977))))))
(define (inner-loop_1-1351-1807-1896 clone-1900
                                     clone-1899
                                     clone-1898
                                     clone-1897)
  (let ((var-1902 (equal? clone-1900 '$)))
    (if (loop_4-1353-1823-1903 var-1902 clone-1900)
        (let ((var-1932 (apply (lambda ($1) $1)
                               (cons (car clone-1898)
                                     '()))))
          (parse-result 'p 1 var-1932 clone-1899))
        (let ((var-1939 (stream-car clone-1899)))
          (let ((var-1941 (car var-1939)))
            (_sim-error 'direct-parse "can't shift on" var-1941))))))
(define (loop_4-1353-1823-1903 clone-1905 clone-1904)
  (if clone-1905
      clone-1905
      (let ((var-1907 (equal? clone-1904 '*)))
        (loop_4-1353-1823-1908 var-1907 clone-1904))))
(define (loop_4-1353-1823-1908 clone-1910 clone-1909)
  (if clone-1910
      clone-1910
      (let ((var-1912 (equal? clone-1909 '+)))
        (loop_4-1353-1823-1913 var-1912 clone-1909))))
(define (loop_4-1353-1823-1913 clone-1915 clone-1914)
  (if clone-1915
      clone-1915
      (let ((var-1917 (equal? clone-1914 '-)))
        (loop_4-1353-1823-1918 var-1917 clone-1914))))
(define (loop_4-1353-1823-1918 clone-1920 clone-1919)
  (if clone-1920
      clone-1920
      (let ((var-1922 (equal? clone-1919 '/)))
        (loop_4-1353-1823-1923 var-1922 clone-1919))))
(define (loop_4-1353-1823-1923 clone-1925 clone-1924)
  (if clone-1925 clone-1925 #f))
