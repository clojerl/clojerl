
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         56 |    35 |   -21 |    -37 |   0.63 |
|                                                                                     [], (str "1") | 1000000 |         35 |    22 |   -13 |    -37 |   0.63 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         53 |    33 |   -20 |    -37 |   0.62 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         53 |    33 |   -20 |    -37 |   0.62 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         67 |    43 |   -24 |    -35 |   0.64 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |        145 |    95 |   -50 |    -34 |   0.66 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |        128 |    86 |   -42 |    -32 |   0.67 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        577 |   420 |  -157 |    -27 |   0.73 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         70 |    51 |   -19 |    -27 |   0.73 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         60 |    45 |   -15 |    -25 |   0.75 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |        167 |   129 |   -38 |    -22 |   0.77 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         62 |    48 |   -14 |    -22 |   0.77 |
|                                                                                       [], (str 1) | 1000000 |         55 |    44 |   -11 |    -20 |   0.80 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        596 |   497 |   -99 |    -16 |   0.83 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        359 |   300 |   -59 |    -16 |   0.84 |
|                                                                                 [], (str "1" "2") | 1000000 |        372 |   310 |   -62 |    -16 |   0.83 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        136 |   115 |   -21 |    -15 |   0.85 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        179 |   153 |   -26 |    -14 |   0.85 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        443 |   385 |   -58 |    -13 |   0.87 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        853 |   734 |  -119 |    -13 |   0.86 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         22 |    19 |    -3 |    -13 |   0.86 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        142 |   123 |   -19 |    -13 |   0.87 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        178 |   156 |   -22 |    -12 |   0.88 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         93 |    81 |   -12 |    -12 |   0.87 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        428 |   377 |   -51 |    -11 |   0.88 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        361 |   321 |   -40 |    -11 |   0.89 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        278 |   245 |   -33 |    -11 |   0.88 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        272 |   242 |   -30 |    -11 |   0.89 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         27 |    24 |    -3 |    -11 |   0.89 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        330 |   291 |   -39 |    -11 |   0.88 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         83 |    74 |    -9 |    -10 |   0.89 |
|                                                                                   [r r], (last r) |       1 |        448 |   400 |   -48 |    -10 |   0.89 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         90 |    81 |    -9 |    -10 |   0.90 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        568 |   506 |   -62 |    -10 |   0.89 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        699 |   624 |   -75 |    -10 |   0.89 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        135 |   122 |   -13 |     -9 |   0.90 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         21 |    19 |    -2 |     -9 |   0.90 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        661 |   600 |   -61 |     -9 |   0.91 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |        183 |   168 |   -15 |     -8 |   0.92 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        297 |   276 |   -21 |     -7 |   0.93 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        317 |   293 |   -24 |     -7 |   0.92 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        126 |   117 |    -9 |     -7 |   0.93 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        231 |   214 |   -17 |     -7 |   0.93 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        728 |   678 |   -50 |     -6 |   0.93 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |        176 |   164 |   -12 |     -6 |   0.93 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        229 |   213 |   -16 |     -6 |   0.93 |
|                                                                           [], (simple-multi :foo) | 1000000 |        808 |   754 |   -54 |     -6 |   0.93 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |        282 |   265 |   -17 |     -6 |   0.94 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        859 |   803 |   -56 |     -6 |   0.93 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        232 |   217 |   -15 |     -6 |   0.94 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        138 |   131 |    -7 |     -5 |   0.95 |
|                                                                                [x 10], (pr-str x) |    1000 |         17 |    16 |    -1 |     -5 |   0.94 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        137 |   130 |    -7 |     -5 |   0.95 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |        135 |   128 |    -7 |     -5 |   0.95 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        337 |   319 |   -18 |     -5 |   0.95 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        240 |   226 |   -14 |     -5 |   0.94 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        150 |   142 |    -8 |     -5 |   0.95 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         75 |    72 |    -3 |     -4 |   0.96 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        140 |   134 |    -6 |     -4 |   0.96 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        201 |   192 |    -9 |     -4 |   0.96 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        514 |   490 |   -24 |     -4 |   0.95 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         69 |    66 |    -3 |     -4 |   0.96 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        618 |   597 |   -21 |     -3 |   0.97 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |        264 |   254 |   -10 |     -3 |   0.96 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1870 |  1801 |   -69 |     -3 |   0.96 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        358 |   346 |   -12 |     -3 |   0.97 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        427 |   412 |   -15 |     -3 |   0.96 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        636 |   619 |   -17 |     -2 |   0.97 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         83 |    81 |    -2 |     -2 |   0.98 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        414 |   406 |    -8 |     -1 |   0.98 |
|                                                                     [r (range 1000000)], (last r) |       1 |        167 |   164 |    -3 |     -1 |   0.98 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        353 |   350 |    -3 |      0 |   0.99 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         69 |    69 |     0 |      0 |   1.00 |
|                                                                                            [], [] | 1000000 |          8 |     8 |     0 |      0 |   1.00 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        189 |   189 |     0 |      0 |   1.00 |
|                                                                                        [], (list) | 1000000 |         33 |    33 |     0 |      0 |   1.00 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         49 |    49 |     0 |      0 |   1.00 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         47 |    47 |     0 |      0 |   1.00 |
|                                                                              [x true], (pr-str x) |    1000 |         13 |    13 |     0 |      0 |   1.00 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         26 |    26 |     0 |      0 |   1.00 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        191 |   190 |    -1 |      0 |   0.99 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         22 |    22 |     0 |      0 |   1.00 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1301 |  1303 |     2 |      0 |   1.00 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1588 |  1587 |    -1 |      0 |   1.00 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         28 |    28 |     0 |      0 |   1.00 |
|                                                                               [x 1], (identity x) | 1000000 |         12 |    12 |     0 |      0 |   1.00 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         26 |    26 |     0 |      0 |   1.00 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         24 |    24 |     0 |      0 |   1.00 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         29 |    29 |     0 |      0 |   1.00 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        176 |   178 |     2 |      1 |   1.01 |
|                                                                                  [], (list 1 2 3) | 1000000 |         33 |    34 |     1 |      3 |   1.03 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        318 |   337 |    19 |      5 |   1.06 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        409 |   435 |    26 |      6 |   1.06 |
|                                                                                     [], (str nil) | 1000000 |         13 |    14 |     1 |      7 |   1.08 |
|                                                                                            Totals |         |      25717 | 23774 | -1943 |        |   0.92 |

