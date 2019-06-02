|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                                                                     [], (str "1") | 1000000 |         43 |    31 |   -12 |    -27 |   0.72 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         36 |    27 |    -9 |    -25 |   0.75 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        174 |   136 |   -38 |    -21 |   0.78 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         90 |    72 |   -18 |    -20 |   0.80 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        182 |   146 |   -36 |    -19 |   0.80 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        178 |   143 |   -35 |    -19 |   0.80 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        854 |   697 |  -157 |    -18 |   0.82 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |        385 |   319 |   -66 |    -17 |   0.83 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        880 |   738 |  -142 |    -16 |   0.84 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |        236 |   200 |   -36 |    -15 |   0.85 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |        228 |   194 |   -34 |    -14 |   0.85 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |       1029 |   881 |  -148 |    -14 |   0.86 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         81 |    70 |   -11 |    -13 |   0.86 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        493 |   426 |   -67 |    -13 |   0.86 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        298 |   257 |   -41 |    -13 |   0.86 |
|                                                                                       [], (str 1) | 1000000 |         56 |    49 |    -7 |    -12 |   0.88 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         24 |    21 |    -3 |    -12 |   0.88 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        126 |   111 |   -15 |    -11 |   0.88 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        544 |   484 |   -60 |    -11 |   0.89 |
|                                                                           [], (simple-multi :foo) | 1000000 |        950 |   850 |  -100 |    -10 |   0.89 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        380 |   342 |   -38 |    -10 |   0.90 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        839 |   757 |   -82 |     -9 |   0.90 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        297 |   268 |   -29 |     -9 |   0.90 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        419 |   382 |   -37 |     -8 |   0.91 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        474 |   432 |   -42 |     -8 |   0.91 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        310 |   285 |   -25 |     -8 |   0.92 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        933 |   856 |   -77 |     -8 |   0.92 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        625 |   573 |   -52 |     -8 |   0.92 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        170 |   156 |   -14 |     -8 |   0.92 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        544 |   501 |   -43 |     -7 |   0.92 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        353 |   327 |   -26 |     -7 |   0.93 |
|                                                                                     [], (str nil) | 1000000 |         14 |    13 |    -1 |     -7 |   0.93 |
|                                                                                        [], (list) | 1000000 |         30 |    28 |    -2 |     -6 |   0.93 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        367 |   342 |   -25 |     -6 |   0.93 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        610 |   576 |   -34 |     -5 |   0.94 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        166 |   157 |    -9 |     -5 |   0.95 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         56 |    53 |    -3 |     -5 |   0.95 |
|                                                                                 [], (str "1" "2") | 1000000 |        414 |   392 |   -22 |     -5 |   0.95 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        792 |   757 |   -35 |     -4 |   0.96 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         25 |    24 |    -1 |     -4 |   0.96 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         29 |    28 |    -1 |     -3 |   0.97 |
|                                                                                  [], (list 1 2 3) | 1000000 |         30 |    29 |    -1 |     -3 |   0.97 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        440 |   424 |   -16 |     -3 |   0.96 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        180 |   176 |    -4 |     -2 |   0.98 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        146 |   142 |    -4 |     -2 |   0.97 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        480 |   467 |   -13 |     -2 |   0.97 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        181 |   176 |    -5 |     -2 |   0.97 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         76 |    75 |    -1 |     -1 |   0.99 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        282 |   279 |    -3 |     -1 |   0.99 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        231 |   228 |    -3 |     -1 |   0.99 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        689 |   681 |    -8 |     -1 |   0.99 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         83 |    82 |    -1 |     -1 |   0.99 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         54 |    54 |     0 |      0 |   1.00 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |        119 |   119 |     0 |      0 |   1.00 |
|                                                                                            [], [] | 1000000 |          8 |     8 |     0 |      0 |   1.00 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        156 |   155 |    -1 |      0 |   0.99 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        434 |   438 |     4 |      0 |   1.01 |
|                                                                                   [r r], (last r) |       1 |        451 |   448 |    -3 |      0 |   0.99 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        114 |   114 |     0 |      0 |   1.00 |
|                                                                              [x true], (pr-str x) |    1000 |         14 |    14 |     0 |      0 |   1.00 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        199 |   200 |     1 |      0 |   1.01 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |        134 |   134 |     0 |      0 |   1.00 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        150 |   151 |     1 |      0 |   1.01 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |        156 |   157 |     1 |      0 |   1.01 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        518 |   513 |    -5 |      0 |   0.99 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1416 |  1420 |     4 |      0 |   1.00 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         53 |    53 |     0 |      0 |   1.00 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1595 |  1587 |    -8 |      0 |   0.99 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         25 |    25 |     0 |      0 |   1.00 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        423 |   421 |    -2 |      0 |   1.00 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         67 |    67 |     0 |      0 |   1.00 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |        246 |   244 |    -2 |      0 |   0.99 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1853 |  1858 |     5 |      0 |   1.00 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         53 |    53 |     0 |      0 |   1.00 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        318 |   319 |     1 |      0 |   1.00 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        232 |   231 |    -1 |      0 |   1.00 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         29 |    29 |     0 |      0 |   1.00 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        138 |   139 |     1 |      0 |   1.01 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         74 |    75 |     1 |      1 |   1.01 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         89 |    90 |     1 |      1 |   1.01 |
|                                                                     [r (range 1000000)], (last r) |       1 |        164 |   166 |     2 |      1 |   1.01 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        386 |   391 |     5 |      1 |   1.01 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         54 |    55 |     1 |      1 |   1.02 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         50 |    51 |     1 |      2 |   1.02 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         91 |    93 |     2 |      2 |   1.02 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        600 |   621 |    21 |      3 |   1.03 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         66 |    68 |     2 |      3 |   1.03 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         25 |    26 |     1 |      4 |   1.04 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |        146 |   154 |     8 |      5 |   1.05 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         47 |    51 |     4 |      8 |   1.09 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         24 |    26 |     2 |      8 |   1.08 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         22 |    24 |     2 |      9 |   1.09 |
|                                                                               [x 1], (identity x) | 1000000 |         11 |    12 |     1 |      9 |   1.09 |
|                                                                                [x 10], (pr-str x) |    1000 |         13 |    16 |     3 |     23 |   1.23 |
|                                                                                            Totals |         |      28369 | 26730 | -1639 |        |   0.94 |
