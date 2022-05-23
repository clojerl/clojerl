
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        597 |   367 |  -230 |    -38 |   0.61 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         16 |    10 |    -6 |    -37 |   0.63 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |         58 |    37 |   -21 |    -36 |   0.64 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        541 |   346 |  -195 |    -36 |   0.64 |
|               [xs (into [] (range 1000000))], (r/reduce + (r/map inc (r/map inc (r/map inc xs)))) |       1 |        534 |   345 |  -189 |    -35 |   0.65 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        516 |   340 |  -176 |    -34 |   0.66 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         21 |    14 |    -7 |    -33 |   0.67 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |         85 |    59 |   -26 |    -30 |   0.69 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        300 |   210 |   -90 |    -30 |   0.70 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        279 |   199 |   -80 |    -28 |   0.71 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         28 |    20 |    -8 |    -28 |   0.71 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        206 |   148 |   -58 |    -28 |   0.72 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         45 |    32 |   -13 |    -28 |   0.71 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         44 |    32 |   -12 |    -27 |   0.73 |
|                                                                                   [r r], (last r) |       1 |        181 |   134 |   -47 |    -25 |   0.74 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         28 |    21 |    -7 |    -25 |   0.75 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        602 |   457 |  -145 |    -24 |   0.76 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         28 |    22 |    -6 |    -21 |   0.79 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         51 |    40 |   -11 |    -21 |   0.78 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        288 |   229 |   -59 |    -20 |   0.80 |
|                                                                               [x 1], (identity x) | 1000000 |          5 |     4 |    -1 |    -20 |   0.80 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |         64 |    51 |   -13 |    -20 |   0.80 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |         81 |    65 |   -16 |    -19 |   0.80 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        283 |   230 |   -53 |    -18 |   0.81 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        371 |   303 |   -68 |    -18 |   0.82 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         44 |    36 |    -8 |    -18 |   0.82 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        440 |   357 |   -83 |    -18 |   0.81 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        267 |   219 |   -48 |    -17 |   0.82 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        233 |   192 |   -41 |    -17 |   0.82 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        225 |   187 |   -38 |    -16 |   0.83 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        213 |   177 |   -36 |    -16 |   0.83 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         13 |    11 |    -2 |    -15 |   0.85 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        202 |   171 |   -31 |    -15 |   0.85 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |         83 |    70 |   -13 |    -15 |   0.84 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        148 |   125 |   -23 |    -15 |   0.84 |
|                                                                                       [], (str 1) | 1000000 |         34 |    29 |    -5 |    -14 |   0.85 |
|                                                                                 [], (str "1" "2") | 1000000 |        302 |   259 |   -43 |    -14 |   0.86 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        446 |   380 |   -66 |    -14 |   0.85 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1509 |  1287 |  -222 |    -14 |   0.85 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        177 |   151 |   -26 |    -14 |   0.85 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1273 |  1093 |  -180 |    -14 |   0.86 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        150 |   130 |   -20 |    -13 |   0.87 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |         61 |    53 |    -8 |    -13 |   0.87 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        399 |   345 |   -54 |    -13 |   0.86 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        136 |   119 |   -17 |    -12 |   0.88 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        367 |   320 |   -47 |    -12 |   0.87 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        192 |   168 |   -24 |    -12 |   0.88 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        149 |   130 |   -19 |    -12 |   0.87 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |         65 |    57 |    -8 |    -12 |   0.88 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        184 |   161 |   -23 |    -12 |   0.88 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |         70 |    62 |    -8 |    -11 |   0.89 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        142 |   125 |   -17 |    -11 |   0.88 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1590 |  1406 |  -184 |    -11 |   0.88 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        346 |   306 |   -40 |    -11 |   0.88 |
|                                                                                     [], (str "1") | 1000000 |         10 |     9 |    -1 |    -10 |   0.90 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        191 |   172 |   -19 |     -9 |   0.90 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         36 |    33 |    -3 |     -8 |   0.92 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |         80 |    73 |    -7 |     -8 |   0.91 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |         49 |    45 |    -4 |     -8 |   0.92 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        168 |   154 |   -14 |     -8 |   0.92 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |         47 |    43 |    -4 |     -8 |   0.91 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        141 |   130 |   -11 |     -7 |   0.92 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        362 |   336 |   -26 |     -7 |   0.93 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         38 |    35 |    -3 |     -7 |   0.92 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        100 |    95 |    -5 |     -5 |   0.95 |
|                                                                     [r (range 1000000)], (last r) |       1 |         55 |    52 |    -3 |     -5 |   0.95 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |         49 |    47 |    -2 |     -4 |   0.96 |
|                                                                           [], (simple-multi :foo) | 1000000 |        160 |   155 |    -5 |     -3 |   0.97 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |         74 |    73 |    -1 |     -1 |   0.99 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         48 |    48 |     0 |      0 |   1.00 |
|                                                                              [x true], (pr-str x) |    1000 |          9 |     9 |     0 |      0 |   1.00 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         19 |    19 |     0 |      0 |   1.00 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |         79 |    79 |     0 |      0 |   1.00 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |         31 |    31 |     0 |      0 |   1.00 |
|                                                                                            [], [] | 1000000 |          2 |     2 |     0 |      0 |   1.00 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         10 |    10 |     0 |      0 |   1.00 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         29 |    29 |     0 |      0 |   1.00 |
|                                                                                        [], (list) | 1000000 |          6 |     6 |     0 |      0 |   1.00 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |         96 |   100 |     4 |      4 |   1.04 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |         35 |    37 |     2 |      5 |   1.06 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         18 |    19 |     1 |      5 |   1.06 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |         41 |    44 |     3 |      7 |   1.07 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        107 |   117 |    10 |      9 |   1.09 |
|                                                                                  [], (list 1 2 3) | 1000000 |         21 |    23 |     2 |      9 |   1.10 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         20 |    22 |     2 |     10 |   1.10 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         14 |    16 |     2 |     14 |   1.14 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         14 |    16 |     2 |     14 |   1.14 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |         49 |    56 |     7 |     14 |   1.14 |
|                                                                                     [], (str nil) | 1000000 |          5 |     6 |     1 |     20 |   1.20 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         52 |    64 |    12 |     23 |   1.23 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         13 |    16 |     3 |     23 |   1.23 |
|                                                                                [x 10], (pr-str x) |    1000 |          8 |    10 |     2 |     25 |   1.25 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         26 |    34 |     8 |     30 |   1.31 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         18 |    24 |     6 |     33 |   1.33 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         14 |    19 |     5 |     35 |   1.36 |
|                                                                                            Totals |         |      17056 | 14149 | -2907 |        |   0.83 |

