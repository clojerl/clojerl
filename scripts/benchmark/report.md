
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |        272 |    73 |  -199 |    -73 |   0.27 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |        167 |    72 |   -95 |    -56 |   0.43 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |        165 |    71 |   -94 |    -56 |   0.43 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |        128 |    81 |   -47 |    -36 |   0.63 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         31 |    25 |    -6 |    -19 |   0.81 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        141 |   117 |   -24 |    -17 |   0.83 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         29 |    25 |    -4 |    -13 |   0.86 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         22 |    20 |    -2 |     -9 |   0.91 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |        113 |   102 |   -11 |     -9 |   0.90 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         84 |    77 |    -7 |     -8 |   0.92 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        303 |   276 |   -27 |     -8 |   0.91 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        474 |   435 |   -39 |     -8 |   0.92 |
|                                                                                       [], (str 1) | 1000000 |         47 |    43 |    -4 |     -8 |   0.91 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         74 |    68 |    -6 |     -8 |   0.92 |
|                                                                                     [], (str nil) | 1000000 |         14 |    13 |    -1 |     -7 |   0.93 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         31 |    29 |    -2 |     -6 |   0.94 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        262 |   246 |   -16 |     -6 |   0.94 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        347 |   327 |   -20 |     -5 |   0.94 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        700 |   669 |   -31 |     -4 |   0.96 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |         45 |    43 |    -2 |     -4 |   0.96 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         46 |    44 |    -2 |     -4 |   0.96 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         24 |    23 |    -1 |     -4 |   0.96 |
|                                                                                     [], (str "1") | 1000000 |         21 |    20 |    -1 |     -4 |   0.95 |
|                                                                                 [], (str "1" "2") | 1000000 |        304 |   293 |   -11 |     -3 |   0.96 |
|                                                                                  [], (list 1 2 3) | 1000000 |         30 |    29 |    -1 |     -3 |   0.97 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        416 |   402 |   -14 |     -3 |   0.97 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        454 |   439 |   -15 |     -3 |   0.97 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         84 |    81 |    -3 |     -3 |   0.96 |
|                                                                           [], (simple-multi :foo) | 1000000 |        243 |   235 |    -8 |     -3 |   0.97 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        417 |   407 |   -10 |     -2 |   0.98 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        554 |   538 |   -16 |     -2 |   0.97 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         48 |    47 |    -1 |     -2 |   0.98 |
|                                                                                   [r r], (last r) |       1 |        429 |   418 |   -11 |     -2 |   0.97 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        641 |   633 |    -8 |     -1 |   0.99 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        120 |   118 |    -2 |     -1 |   0.98 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        418 |   413 |    -5 |     -1 |   0.99 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        130 |   128 |    -2 |     -1 |   0.98 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        403 |   397 |    -6 |     -1 |   0.99 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        197 |   194 |    -3 |     -1 |   0.98 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        648 |   641 |    -7 |     -1 |   0.99 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         48 |    48 |     0 |      0 |   1.00 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        130 |   130 |     0 |      0 |   1.00 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        333 |   336 |     3 |      0 |   1.01 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         25 |    25 |     0 |      0 |   1.00 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        875 |   867 |    -8 |      0 |   0.99 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        293 |   293 |     0 |      0 |   1.00 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        535 |   540 |     5 |      0 |   1.01 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         34 |    34 |     0 |      0 |   1.00 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        692 |   696 |     4 |      0 |   1.01 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1349 |  1355 |     6 |      0 |   1.00 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        160 |   161 |     1 |      0 |   1.01 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        179 |   178 |    -1 |      0 |   0.99 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |        134 |   134 |     0 |      0 |   1.00 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        168 |   168 |     0 |      0 |   1.00 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         26 |    26 |     0 |      0 |   1.00 |
|                                                                              [x true], (pr-str x) |    1000 |         13 |    13 |     0 |      0 |   1.00 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        142 |   142 |     0 |      0 |   1.00 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        555 |   557 |     2 |      0 |   1.00 |
|                                                                                [x 10], (pr-str x) |    1000 |         13 |    13 |     0 |      0 |   1.00 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        185 |   184 |    -1 |      0 |   0.99 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        302 |   304 |     2 |      0 |   1.01 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        135 |   134 |    -1 |      0 |   0.99 |
|                                                                                        [], (list) | 1000000 |         29 |    29 |     0 |      0 |   1.00 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        312 |   315 |     3 |      0 |   1.01 |
|                                                                                            [], [] | 1000000 |          9 |     9 |     0 |      0 |   1.00 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |         83 |    83 |     0 |      0 |   1.00 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         37 |    37 |     0 |      0 |   1.00 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         72 |    73 |     1 |      1 |   1.01 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        105 |   107 |     2 |      1 |   1.02 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         51 |    52 |     1 |      1 |   1.02 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        523 |   530 |     7 |      1 |   1.01 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        270 |   274 |     4 |      1 |   1.01 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        304 |   310 |     6 |      1 |   1.02 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        138 |   140 |     2 |      1 |   1.01 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         70 |    71 |     1 |      1 |   1.01 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         44 |    45 |     1 |      2 |   1.02 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        106 |   109 |     3 |      2 |   1.03 |
|                                                                     [r (range 1000000)], (last r) |       1 |        171 |   175 |     4 |      2 |   1.02 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         68 |    70 |     2 |      2 |   1.03 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        145 |   148 |     3 |      2 |   1.02 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         48 |    49 |     1 |      2 |   1.02 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         32 |    33 |     1 |      3 |   1.03 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        154 |   159 |     5 |      3 |   1.03 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        214 |   222 |     8 |      3 |   1.04 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         84 |    87 |     3 |      3 |   1.04 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        240 |   250 |    10 |      4 |   1.04 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1641 |  1721 |    80 |      4 |   1.05 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         34 |    36 |     2 |      5 |   1.06 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        608 |   642 |    34 |      5 |   1.06 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        352 |   380 |    28 |      7 |   1.08 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        186 |   204 |    18 |      9 |   1.10 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1869 |  2069 |   200 |     10 |   1.11 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         17 |    19 |     2 |     11 |   1.12 |
|                                                                               [x 1], (identity x) | 1000000 |         12 |    14 |     2 |     16 |   1.17 |
|                                                                                            Totals |         |      23430 | 23112 |  -318 |        |   0.99 |

