
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                                                       [coll "foobar"], (seq coll) | 1000000 |        254 |    47 |  -207 |    -81 |   0.19 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        291 |   130 |  -161 |    -55 |   0.45 |
|                                                                                            [], [] | 1000000 |          8 |     7 |    -1 |    -12 |   0.88 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        435 |   395 |   -40 |     -9 |   0.91 |
|                                                                                     [], (str nil) | 1000000 |         14 |    13 |    -1 |     -7 |   0.93 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        337 |   314 |   -23 |     -6 |   0.93 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         69 |    65 |    -4 |     -5 |   0.94 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         19 |    18 |    -1 |     -5 |   0.95 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        178 |   169 |    -9 |     -5 |   0.95 |
|                                                                                 [], (str "1" "2") | 1000000 |        310 |   292 |   -18 |     -5 |   0.94 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        619 |   594 |   -25 |     -4 |   0.96 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         81 |    77 |    -4 |     -4 |   0.95 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        213 |   205 |    -8 |     -3 |   0.96 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        192 |   185 |    -7 |     -3 |   0.96 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        293 |   282 |   -11 |     -3 |   0.96 |
|                                                                     [r (range 1000000)], (last r) |       1 |        164 |   159 |    -5 |     -3 |   0.97 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         26 |    25 |    -1 |     -3 |   0.96 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        346 |   333 |   -13 |     -3 |   0.96 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        214 |   207 |    -7 |     -3 |   0.97 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        350 |   341 |    -9 |     -2 |   0.97 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         49 |    48 |    -1 |     -2 |   0.98 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |         95 |    93 |    -2 |     -2 |   0.98 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        597 |   585 |   -12 |     -2 |   0.98 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        245 |   240 |    -5 |     -2 |   0.98 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        734 |   718 |   -16 |     -2 |   0.98 |
|                                                                                  [], (list 1 2 3) | 1000000 |         34 |    33 |    -1 |     -2 |   0.97 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        600 |   583 |   -17 |     -2 |   0.97 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |         86 |    85 |    -1 |     -1 |   0.99 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        156 |   154 |    -2 |     -1 |   0.99 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        321 |   315 |    -6 |     -1 |   0.98 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        406 |   398 |    -8 |     -1 |   0.98 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         72 |    71 |    -1 |     -1 |   0.99 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        319 |   313 |    -6 |     -1 |   0.98 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        412 |   404 |    -8 |     -1 |   0.98 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        678 |   675 |    -3 |      0 |   1.00 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         35 |    35 |     0 |      0 |   1.00 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        122 |   122 |     0 |      0 |   1.00 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        131 |   132 |     1 |      0 |   1.01 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         47 |    47 |     0 |      0 |   1.00 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        134 |   135 |     1 |      0 |   1.01 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        276 |   277 |     1 |      0 |   1.00 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         26 |    26 |     0 |      0 |   1.00 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |        128 |   128 |     0 |      0 |   1.00 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |        129 |   129 |     0 |      0 |   1.00 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        490 |   488 |    -2 |      0 |   1.00 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         22 |    22 |     0 |      0 |   1.00 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1303 |  1301 |    -2 |      0 |   1.00 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         33 |    33 |     0 |      0 |   1.00 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        497 |   501 |     4 |      0 |   1.01 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         51 |    51 |     0 |      0 |   1.00 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         48 |    48 |     0 |      0 |   1.00 |
|                                                                               [x 1], (identity x) | 1000000 |         12 |    12 |     0 |      0 |   1.00 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        117 |   118 |     1 |      0 |   1.01 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        226 |   227 |     1 |      0 |   1.00 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        803 |   806 |     3 |      0 |   1.00 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        217 |   218 |     1 |      0 |   1.00 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         29 |    29 |     0 |      0 |   1.00 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |        164 |   167 |     3 |      1 |   1.02 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        377 |   381 |     4 |      1 |   1.01 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |        168 |   171 |     3 |      1 |   1.02 |
|                                                                           [], (simple-multi :foo) | 1000000 |        754 |   764 |    10 |      1 |   1.01 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         81 |    82 |     1 |      1 |   1.01 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        385 |   390 |     5 |      1 |   1.01 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1587 |  1604 |    17 |      1 |   1.01 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        153 |   155 |     2 |      1 |   1.01 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         74 |    76 |     2 |      2 |   1.03 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        420 |   429 |     9 |      2 |   1.02 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        115 |   118 |     3 |      2 |   1.03 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         81 |    83 |     2 |      2 |   1.02 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         43 |    44 |     1 |      2 |   1.02 |
|                                                                                        [], (list) | 1000000 |         33 |    34 |     1 |      3 |   1.03 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        130 |   135 |     5 |      3 |   1.04 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        242 |   251 |     9 |      3 |   1.04 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         28 |    29 |     1 |      3 |   1.04 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        142 |   147 |     5 |      3 |   1.04 |
|                                                                                       [], (str 1) | 1000000 |         44 |    46 |     2 |      4 |   1.05 |
|                                                                                     [], (str "1") | 1000000 |         22 |    23 |     1 |      4 |   1.05 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        190 |   199 |     9 |      4 |   1.05 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |        265 |   278 |    13 |      4 |   1.05 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        506 |   531 |    25 |      4 |   1.05 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         24 |    25 |     1 |      4 |   1.04 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         66 |    69 |     3 |      4 |   1.05 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        189 |   200 |    11 |      5 |   1.06 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        624 |   659 |    35 |      5 |   1.06 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1801 |  1892 |    91 |      5 |   1.05 |
|                                                                                   [r r], (last r) |       1 |        400 |   427 |    27 |      6 |   1.07 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         33 |    35 |     2 |      6 |   1.06 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        123 |   131 |     8 |      6 |   1.07 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         45 |    48 |     3 |      6 |   1.07 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        300 |   325 |    25 |      8 |   1.08 |
|                                                                                [x 10], (pr-str x) |    1000 |         16 |    18 |     2 |     12 |   1.13 |
|                                                                              [x true], (pr-str x) |    1000 |         13 |    15 |     2 |     15 |   1.15 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         19 |    22 |     3 |     15 |   1.16 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         24 |    30 |     6 |     25 |   1.25 |
|                                                                                            Totals |         |      23774 | 23491 |  -283 |        |   0.99 |

