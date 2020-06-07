
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         32 |    23 |    -9 |    -28 |   0.72 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |         88 |    70 |   -18 |    -20 |   0.80 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        136 |   113 |   -23 |    -16 |   0.83 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        134 |   114 |   -20 |    -14 |   0.85 |
|                                                                                   [r r], (last r) |       1 |        226 |   201 |   -25 |    -11 |   0.89 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         35 |    32 |    -3 |     -8 |   0.91 |
|                                                                              [x true], (pr-str x) |    1000 |         12 |    11 |    -1 |     -8 |   0.92 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        276 |   254 |   -22 |     -7 |   0.92 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        348 |   322 |   -26 |     -7 |   0.93 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        594 |   556 |   -38 |     -6 |   0.94 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        338 |   317 |   -21 |     -6 |   0.94 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        312 |   294 |   -18 |     -5 |   0.94 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        123 |   116 |    -7 |     -5 |   0.94 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         19 |    18 |    -1 |     -5 |   0.95 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        389 |   368 |   -21 |     -5 |   0.95 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        276 |   263 |   -13 |     -4 |   0.95 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         32 |    31 |    -1 |     -3 |   0.97 |
|                                                                                  [], (list 1 2 3) | 1000000 |         31 |    30 |    -1 |     -3 |   0.97 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        416 |   402 |   -14 |     -3 |   0.97 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        542 |   524 |   -18 |     -3 |   0.97 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        168 |   162 |    -6 |     -3 |   0.96 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        183 |   176 |    -7 |     -3 |   0.96 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         26 |    25 |    -1 |     -3 |   0.96 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         51 |    49 |    -2 |     -3 |   0.96 |
|                                                                                        [], (list) | 1000000 |         31 |    30 |    -1 |     -3 |   0.97 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        629 |   614 |   -15 |     -2 |   0.98 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        891 |   871 |   -20 |     -2 |   0.98 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1398 |  1361 |   -37 |     -2 |   0.97 |
|                                                                     [r (range 1000000)], (last r) |       1 |         81 |    79 |    -2 |     -2 |   0.98 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        151 |   147 |    -4 |     -2 |   0.97 |
|                                                                                       [], (str 1) | 1000000 |         43 |    42 |    -1 |     -2 |   0.98 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         34 |    33 |    -1 |     -2 |   0.97 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         74 |    73 |    -1 |     -1 |   0.99 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        671 |   660 |   -11 |     -1 |   0.98 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        342 |   336 |    -6 |     -1 |   0.98 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1862 |  1834 |   -28 |     -1 |   0.98 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        335 |   330 |    -5 |     -1 |   0.99 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         82 |    81 |    -1 |     -1 |   0.99 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         85 |    84 |    -1 |     -1 |   0.99 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         73 |    72 |    -1 |     -1 |   0.99 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         51 |    50 |    -1 |     -1 |   0.98 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         71 |    70 |    -1 |     -1 |   0.99 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         48 |    48 |     0 |      0 |   1.00 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        419 |   415 |    -4 |      0 |   0.99 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        152 |   152 |     0 |      0 |   1.00 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         33 |    33 |     0 |      0 |   1.00 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        234 |   236 |     2 |      0 |   1.01 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         25 |    25 |     0 |      0 |   1.00 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        675 |   681 |     6 |      0 |   1.01 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         45 |    45 |     0 |      0 |   1.00 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         27 |    27 |     0 |      0 |   1.00 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1661 |  1655 |    -6 |      0 |   1.00 |
|                                                                                     [], (str nil) | 1000000 |         13 |    13 |     0 |      0 |   1.00 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        296 |   297 |     1 |      0 |   1.00 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |         65 |    65 |     0 |      0 |   1.00 |
|                                                                                     [], (str "1") | 1000000 |         19 |    19 |     0 |      0 |   1.00 |
|                                                                                            [], [] | 1000000 |          9 |     9 |     0 |      0 |   1.00 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        666 |   660 |    -6 |      0 |   0.99 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         79 |    80 |     1 |      1 |   1.01 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        111 |   113 |     2 |      1 |   1.02 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         52 |    53 |     1 |      1 |   1.02 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        253 |   258 |     5 |      1 |   1.02 |
|                                                                           [], (simple-multi :foo) | 1000000 |        240 |   244 |     4 |      1 |   1.02 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        136 |   138 |     2 |      1 |   1.01 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         68 |    69 |     1 |      1 |   1.01 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        502 |   517 |    15 |      2 |   1.03 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         44 |    45 |     1 |      2 |   1.02 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        696 |   715 |    19 |      2 |   1.03 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        425 |   437 |    12 |      2 |   1.03 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |         67 |    69 |     2 |      2 |   1.03 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        118 |   122 |     4 |      3 |   1.03 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        224 |   232 |     8 |      3 |   1.04 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         32 |    33 |     1 |      3 |   1.03 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        362 |   376 |    14 |      3 |   1.04 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         25 |    26 |     1 |      4 |   1.04 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |         45 |    47 |     2 |      4 |   1.04 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        288 |   301 |    13 |      4 |   1.05 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |         82 |    86 |     4 |      4 |   1.05 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        128 |   134 |     6 |      4 |   1.05 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        108 |   114 |     6 |      5 |   1.06 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |         77 |    81 |     4 |      5 |   1.05 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         20 |    21 |     1 |      5 |   1.05 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        179 |   188 |     9 |      5 |   1.05 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |         76 |    80 |     4 |      5 |   1.05 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        417 |   442 |    25 |      5 |   1.06 |
|                                                                                 [], (str "1" "2") | 1000000 |        292 |   314 |    22 |      7 |   1.08 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        167 |   182 |    15 |      8 |   1.09 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |         74 |    80 |     6 |      8 |   1.08 |
|                                                                               [x 1], (identity x) | 1000000 |         11 |    12 |     1 |      9 |   1.09 |
|                                                                                [x 10], (pr-str x) |    1000 |         11 |    12 |     1 |      9 |   1.09 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        166 |   183 |    17 |     10 |   1.10 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        148 |   164 |    16 |     10 |   1.11 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        184 |   205 |    21 |     11 |   1.11 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        397 |   449 |    52 |     13 |   1.13 |
|                                                                                            Totals |         |      22382 | 22240 |  -142 |        |   0.99 |

