
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                                                                        [], (list) | 1000000 |         22 |     6 |   -16 |    -72 |   0.27 |
|                                                                              [x true], (pr-str x) |    1000 |         11 |     9 |    -2 |    -18 |   0.82 |
|                                                                                     [], (str nil) | 1000000 |          6 |     5 |    -1 |    -16 |   0.83 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |         95 |    79 |   -16 |    -16 |   0.83 |
|                                                                               [x 1], (identity x) | 1000000 |          6 |     5 |    -1 |    -16 |   0.83 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        171 |   148 |   -23 |    -13 |   0.87 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         52 |    45 |    -7 |    -13 |   0.87 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        172 |   150 |   -22 |    -12 |   0.87 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         24 |    21 |    -3 |    -12 |   0.88 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        219 |   192 |   -27 |    -12 |   0.88 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |         54 |    47 |    -7 |    -12 |   0.87 |
|                                                                                [x 10], (pr-str x) |    1000 |          9 |     8 |    -1 |    -11 |   0.89 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |         55 |    49 |    -6 |    -10 |   0.89 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |         54 |    49 |    -5 |     -9 |   0.91 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |         54 |    49 |    -5 |     -9 |   0.91 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        566 |   516 |   -50 |     -8 |   0.91 |
|                                                                                 [], (str "1" "2") | 1000000 |        327 |   302 |   -25 |     -7 |   0.92 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         28 |    26 |    -2 |     -7 |   0.93 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         14 |    13 |    -1 |     -7 |   0.93 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        290 |   267 |   -23 |     -7 |   0.92 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        400 |   371 |   -29 |     -7 |   0.93 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         14 |    13 |    -1 |     -7 |   0.93 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        191 |   177 |   -14 |     -7 |   0.93 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        302 |   283 |   -19 |     -6 |   0.94 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        151 |   141 |   -10 |     -6 |   0.93 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        479 |   446 |   -33 |     -6 |   0.93 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        580 |   541 |   -39 |     -6 |   0.93 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         15 |    14 |    -1 |     -6 |   0.93 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        227 |   213 |   -14 |     -6 |   0.94 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |         78 |    74 |    -4 |     -5 |   0.95 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        214 |   202 |   -12 |     -5 |   0.94 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        388 |   367 |   -21 |     -5 |   0.95 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         54 |    51 |    -3 |     -5 |   0.94 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         19 |    18 |    -1 |     -5 |   0.95 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        237 |   225 |   -12 |     -5 |   0.95 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         17 |    16 |    -1 |     -5 |   0.94 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        247 |   233 |   -14 |     -5 |   0.94 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        421 |   399 |   -22 |     -5 |   0.95 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |         86 |    81 |    -5 |     -5 |   0.94 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        195 |   184 |   -11 |     -5 |   0.94 |
|                                                                           [], (simple-multi :foo) | 1000000 |        167 |   160 |    -7 |     -4 |   0.96 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |         89 |    85 |    -4 |     -4 |   0.96 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        100 |    96 |    -4 |     -4 |   0.96 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |         73 |    70 |    -3 |     -4 |   0.96 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |         61 |    58 |    -3 |     -4 |   0.95 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        176 |   168 |    -8 |     -4 |   0.95 |
|                                                                                  [], (list 1 2 3) | 1000000 |         22 |    21 |    -1 |     -4 |   0.95 |
|               [xs (into [] (range 1000000))], (r/reduce + (r/map inc (r/map inc (r/map inc xs)))) |       1 |        558 |   534 |   -24 |     -4 |   0.96 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         21 |    20 |    -1 |     -4 |   0.95 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        298 |   288 |   -10 |     -3 |   0.97 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        198 |   191 |    -7 |     -3 |   0.96 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         29 |    28 |    -1 |     -3 |   0.97 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |         32 |    31 |    -1 |     -3 |   0.97 |
|                                                                     [r (range 1000000)], (last r) |       1 |         57 |    55 |    -2 |     -3 |   0.96 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        456 |   440 |   -16 |     -3 |   0.96 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        626 |   602 |   -24 |     -3 |   0.96 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         49 |    48 |    -1 |     -2 |   0.98 |
|                                                                                       [], (str 1) | 1000000 |         35 |    34 |    -1 |     -2 |   0.97 |
|                                                                                   [r r], (last r) |       1 |        186 |   181 |    -5 |     -2 |   0.97 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |         85 |    83 |    -2 |     -2 |   0.98 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        110 |   107 |    -3 |     -2 |   0.97 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1551 |  1509 |   -42 |     -2 |   0.97 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         45 |    44 |    -1 |     -2 |   0.98 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         53 |    52 |    -1 |     -1 |   0.98 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        102 |   100 |    -2 |     -1 |   0.98 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1612 |  1590 |   -22 |     -1 |   0.99 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         14 |    14 |     0 |      0 |   1.00 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         14 |    14 |     0 |      0 |   1.00 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |         80 |    80 |     0 |      0 |   1.00 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |         35 |    35 |     0 |      0 |   1.00 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         19 |    19 |     0 |      0 |   1.00 |
|                                                                                            [], [] | 1000000 |          2 |     2 |     0 |      0 |   1.00 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         18 |    18 |     0 |      0 |   1.00 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         10 |    10 |     0 |      0 |   1.00 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        349 |   346 |    -3 |      0 |   0.99 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1272 |  1273 |     1 |      0 |   1.00 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         28 |    28 |     0 |      0 |   1.00 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         35 |    36 |     1 |      2 |   1.03 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        353 |   362 |     9 |      2 |   1.03 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         37 |    38 |     1 |      2 |   1.03 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |         40 |    41 |     1 |      2 |   1.02 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |         59 |    61 |     2 |      3 |   1.03 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         27 |    28 |     1 |      3 |   1.04 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         28 |    29 |     1 |      3 |   1.04 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        130 |   136 |     6 |      4 |   1.05 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        143 |   149 |     6 |      4 |   1.04 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        136 |   142 |     6 |      4 |   1.04 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         42 |    44 |     2 |      4 |   1.05 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |         62 |    65 |     3 |      4 |   1.05 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        261 |   279 |    18 |      6 |   1.07 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        278 |   300 |    22 |      7 |   1.08 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        185 |   206 |    21 |     11 |   1.11 |
|                                                                                     [], (str "1") | 1000000 |          9 |    10 |     1 |     11 |   1.11 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |         57 |    64 |     7 |     12 |   1.12 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        353 |   597 |   244 |     69 |   1.69 |
|                                                                                            Totals |         |      17411 | 17056 |  -355 |        |   0.98 |

