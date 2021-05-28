
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                                                                            [], [] | 1000000 |          9 |     2 |    -7 |    -77 |   0.22 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         45 |    18 |   -27 |    -60 |   0.40 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         33 |    14 |   -19 |    -57 |   0.42 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         33 |    14 |   -19 |    -57 |   0.42 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         32 |    14 |   -18 |    -56 |   0.44 |
|                                                                                     [], (str "1") | 1000000 |         20 |     9 |   -11 |    -55 |   0.45 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        135 |    62 |   -73 |    -54 |   0.46 |
|                                                                                     [], (str nil) | 1000000 |         13 |     6 |    -7 |    -53 |   0.46 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        743 |   353 |  -390 |    -52 |   0.48 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         72 |    35 |   -37 |    -51 |   0.49 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        122 |    59 |   -63 |    -51 |   0.48 |
|                                                                               [x 1], (identity x) | 1000000 |         12 |     6 |    -6 |    -50 |   0.50 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        112 |    57 |   -55 |    -49 |   0.51 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         81 |    42 |   -39 |    -48 |   0.52 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         50 |    27 |   -23 |    -46 |   0.54 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |         65 |    35 |   -30 |    -46 |   0.54 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         83 |    45 |   -38 |    -45 |   0.54 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |         73 |    40 |   -33 |    -45 |   0.55 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         18 |    10 |    -8 |    -44 |   0.56 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        410 |   227 |  -183 |    -44 |   0.55 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         50 |    28 |   -22 |    -44 |   0.56 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        523 |   302 |  -221 |    -42 |   0.58 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        147 |    85 |   -62 |    -42 |   0.58 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        334 |   198 |  -136 |    -40 |   0.59 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         49 |    29 |   -20 |    -40 |   0.59 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         32 |    19 |   -13 |    -40 |   0.59 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         25 |    15 |   -10 |    -40 |   0.60 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         46 |    28 |   -18 |    -39 |   0.61 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        405 |   247 |  -158 |    -39 |   0.61 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        143 |    86 |   -57 |    -39 |   0.60 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        144 |    89 |   -55 |    -38 |   0.62 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        163 |   100 |   -63 |    -38 |   0.61 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        166 |   102 |   -64 |    -38 |   0.61 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        417 |   261 |  -156 |    -37 |   0.63 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        464 |   290 |  -174 |    -37 |   0.63 |
|                                                                           [], (simple-multi :foo) | 1000000 |        262 |   167 |   -95 |    -36 |   0.64 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        292 |   185 |  -107 |    -36 |   0.63 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        114 |    73 |   -41 |    -35 |   0.64 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        171 |   110 |   -61 |    -35 |   0.64 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         26 |    17 |    -9 |    -34 |   0.65 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         80 |    53 |   -27 |    -33 |   0.66 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        228 |   151 |   -77 |    -33 |   0.66 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        599 |   400 |  -199 |    -33 |   0.67 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        421 |   278 |  -143 |    -33 |   0.66 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         28 |    19 |    -9 |    -32 |   0.68 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        312 |   214 |   -98 |    -31 |   0.69 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        117 |    80 |   -37 |    -31 |   0.68 |
|                                                                     [r (range 1000000)], (last r) |       1 |         83 |    57 |   -26 |    -31 |   0.69 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        344 |   237 |  -107 |    -31 |   0.69 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         71 |    49 |   -22 |    -30 |   0.69 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        428 |   298 |  -130 |    -30 |   0.70 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         20 |    14 |    -6 |    -30 |   0.70 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        505 |   349 |  -156 |    -30 |   0.69 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        900 |   626 |  -274 |    -30 |   0.70 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        111 |    78 |   -33 |    -29 |   0.70 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        251 |   176 |   -75 |    -29 |   0.70 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        183 |   130 |   -53 |    -28 |   0.71 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |         76 |    54 |   -22 |    -28 |   0.71 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         52 |    37 |   -15 |    -28 |   0.71 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        269 |   191 |   -78 |    -28 |   0.71 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1784 |  1272 |  -512 |    -28 |   0.71 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        304 |   219 |   -85 |    -27 |   0.72 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        131 |    95 |   -36 |    -27 |   0.73 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         29 |    21 |    -8 |    -27 |   0.72 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         72 |    52 |   -20 |    -27 |   0.72 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |         73 |    54 |   -19 |    -26 |   0.74 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |         83 |    61 |   -22 |    -26 |   0.73 |
|                                                                                  [], (list 1 2 3) | 1000000 |         30 |    22 |    -8 |    -26 |   0.73 |
|                                                                                        [], (list) | 1000000 |         30 |    22 |    -8 |    -26 |   0.73 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        647 |   479 |  -168 |    -25 |   0.74 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |         43 |    32 |   -11 |    -25 |   0.74 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        608 |   456 |  -152 |    -25 |   0.75 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        261 |   195 |   -66 |    -25 |   0.75 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |         73 |    55 |   -18 |    -24 |   0.75 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        181 |   136 |   -45 |    -24 |   0.75 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        560 |   421 |  -139 |    -24 |   0.75 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         71 |    54 |   -17 |    -23 |   0.76 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        188 |   143 |   -45 |    -23 |   0.76 |
|                                                                                       [], (str 1) | 1000000 |         45 |    35 |   -10 |    -22 |   0.78 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         31 |    24 |    -7 |    -22 |   0.77 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        219 |   172 |   -47 |    -21 |   0.79 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |         68 |    54 |   -14 |    -20 |   0.79 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        708 |   580 |  -128 |    -18 |   0.82 |
|                                                                                [x 10], (pr-str x) |    1000 |         11 |     9 |    -2 |    -18 |   0.82 |
|                                                                              [x true], (pr-str x) |    1000 |         13 |    11 |    -2 |    -15 |   0.85 |
|               [xs (into [] (range 1000000))], (r/reduce + (r/map inc (r/map inc (r/map inc xs)))) |       1 |        649 |   558 |   -91 |    -14 |   0.86 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        649 |   566 |   -83 |    -12 |   0.87 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        424 |   388 |   -36 |     -8 |   0.92 |
|                                                                                   [r r], (last r) |       1 |        199 |   186 |   -13 |     -6 |   0.93 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1688 |  1612 |   -76 |     -4 |   0.95 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        367 |   353 |   -14 |     -3 |   0.96 |
|                                                                                 [], (str "1" "2") | 1000000 |        309 |   327 |    18 |      5 |   1.06 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         26 |    28 |     2 |      7 |   1.08 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1376 |  1551 |   175 |     12 |   1.13 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        151 |   171 |    20 |     13 |   1.13 |
|                                                                                            Totals |         |      23313 | 17411 | -5902 |        |   0.75 |

