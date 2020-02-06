
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                                                           [], (simple-multi :foo) | 1000000 |        764 |   243 |  -521 |    -68 |   0.32 |
|                                                                                [x 10], (pr-str x) |    1000 |         18 |    13 |    -5 |    -27 |   0.72 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        131 |   105 |   -26 |    -19 |   0.80 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        199 |   160 |   -39 |    -19 |   0.80 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        154 |   130 |   -24 |    -15 |   0.84 |
|                                                                                        [], (list) | 1000000 |         34 |    29 |    -5 |    -14 |   0.85 |
|                                                                              [x true], (pr-str x) |    1000 |         15 |    13 |    -2 |    -13 |   0.87 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        207 |   186 |   -21 |    -10 |   0.90 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        118 |   106 |   -12 |    -10 |   0.90 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        155 |   141 |   -14 |     -9 |   0.91 |
|                                                                                  [], (list 1 2 3) | 1000000 |         33 |    30 |    -3 |     -9 |   0.91 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        325 |   293 |   -32 |     -9 |   0.90 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        185 |   168 |   -17 |     -9 |   0.91 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        205 |   185 |   -20 |     -9 |   0.90 |
|                                                                                     [], (str "1") | 1000000 |         23 |    21 |    -2 |     -8 |   0.91 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        659 |   608 |   -51 |     -7 |   0.92 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         76 |    70 |    -6 |     -7 |   0.92 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        227 |   214 |   -13 |     -5 |   0.94 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        585 |   555 |   -30 |     -5 |   0.95 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         18 |    17 |    -1 |     -5 |   0.94 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |         47 |    45 |    -2 |     -4 |   0.96 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         48 |    46 |    -2 |     -4 |   0.96 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         71 |    68 |    -3 |     -4 |   0.96 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        675 |   648 |   -27 |     -4 |   0.96 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         30 |    29 |    -1 |     -3 |   0.97 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        718 |   692 |   -26 |     -3 |   0.96 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        314 |   302 |   -12 |     -3 |   0.96 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         35 |    34 |    -1 |     -2 |   0.97 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |        278 |   272 |    -6 |     -2 |   0.98 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |        171 |   167 |    -4 |     -2 |   0.98 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |         85 |    83 |    -2 |     -2 |   0.98 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1892 |  1869 |   -23 |     -1 |   0.99 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        200 |   197 |    -3 |     -1 |   0.98 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |        167 |   165 |    -2 |     -1 |   0.99 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         48 |    48 |     0 |      0 |   1.00 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        130 |   130 |     0 |      0 |   1.00 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        333 |   333 |     0 |      0 |   1.00 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         44 |    44 |     0 |      0 |   1.00 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         25 |    25 |     0 |      0 |   1.00 |
|                                                                               [x 1], (identity x) | 1000000 |         12 |    12 |     0 |      0 |   1.00 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         22 |    22 |     0 |      0 |   1.00 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         51 |    51 |     0 |      0 |   1.00 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |        129 |   128 |    -1 |      0 |   0.99 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         26 |    26 |     0 |      0 |   1.00 |
|                                                                                   [r r], (last r) |       1 |        427 |   429 |     2 |      0 |   1.00 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         48 |    48 |     0 |      0 |   1.00 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        315 |   312 |    -3 |      0 |   0.99 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         83 |    84 |     1 |      1 |   1.01 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        118 |   120 |     2 |      1 |   1.02 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1604 |  1641 |    37 |      2 |   1.02 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         82 |    84 |     2 |      2 |   1.02 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        135 |   138 |     3 |      2 |   1.02 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         47 |    48 |     1 |      2 |   1.02 |
|                                                                                       [], (str 1) | 1000000 |         46 |    47 |     1 |      2 |   1.02 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        404 |   417 |    13 |      3 |   1.03 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         33 |    34 |     1 |      3 |   1.03 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1301 |  1349 |    48 |      3 |   1.04 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        341 |   352 |    11 |      3 |   1.03 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         69 |    72 |     3 |      4 |   1.04 |
|                                                                                 [], (str "1" "2") | 1000000 |        292 |   304 |    12 |      4 |   1.04 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        147 |   154 |     7 |      4 |   1.05 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        531 |   554 |    23 |      4 |   1.04 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        251 |   262 |    11 |      4 |   1.04 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |        128 |   134 |     6 |      4 |   1.05 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        395 |   416 |    21 |      5 |   1.05 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        169 |   179 |    10 |      5 |   1.06 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        135 |   142 |     7 |      5 |   1.05 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        398 |   418 |    20 |      5 |   1.05 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        381 |   403 |    22 |      5 |   1.06 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         35 |    37 |     2 |      5 |   1.06 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         29 |    31 |     2 |      6 |   1.07 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        501 |   535 |    34 |      6 |   1.07 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        594 |   641 |    47 |      7 |   1.08 |
|                                                                                     [], (str nil) | 1000000 |         13 |    14 |     1 |      7 |   1.08 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        488 |   523 |    35 |      7 |   1.07 |
|                                                                     [r (range 1000000)], (last r) |       1 |        159 |   171 |    12 |      7 |   1.08 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        282 |   303 |    21 |      7 |   1.07 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        806 |   875 |    69 |      8 |   1.09 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         77 |    84 |     7 |      9 |   1.09 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         22 |    24 |     2 |      9 |   1.09 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        277 |   304 |    27 |      9 |   1.10 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        132 |   145 |    13 |      9 |   1.10 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         29 |    32 |     3 |     10 |   1.10 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        218 |   240 |    22 |     10 |   1.10 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        313 |   347 |    34 |     10 |   1.11 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        122 |   135 |    13 |     10 |   1.11 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        429 |   474 |    45 |     10 |   1.10 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        240 |   270 |    30 |     12 |   1.13 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         65 |    74 |     9 |     13 |   1.14 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        390 |   454 |    64 |     16 |   1.16 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        583 |   700 |   117 |     20 |   1.20 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |         93 |   113 |    20 |     21 |   1.22 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         25 |    31 |     6 |     24 |   1.24 |
|                                                                                            [], [] | 1000000 |          7 |     9 |     2 |     28 |   1.29 |
|                                                                                            Totals |         |      23491 | 23430 |   -61 |        |   1.00 |

