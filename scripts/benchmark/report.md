
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        342 |   240 |  -102 |    -29 |   0.70 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        501 |   361 |  -140 |    -27 |   0.72 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        432 |   317 |  -115 |    -26 |   0.73 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        484 |   358 |  -126 |    -26 |   0.74 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         27 |    22 |    -5 |    -18 |   0.81 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        738 |   636 |  -102 |    -13 |   0.86 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |        319 |   282 |   -37 |    -11 |   0.88 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         93 |    83 |   -10 |    -10 |   0.89 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |        194 |   176 |   -18 |     -9 |   0.91 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        681 |   618 |   -63 |     -9 |   0.91 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        327 |   297 |   -30 |     -9 |   0.91 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        257 |   232 |   -25 |     -9 |   0.90 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |        200 |   183 |   -17 |     -8 |   0.92 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         24 |    22 |    -2 |     -8 |   0.92 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1420 |  1301 |  -119 |     -8 |   0.92 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        391 |   359 |   -32 |     -8 |   0.92 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        382 |   353 |   -29 |     -7 |   0.92 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         51 |    47 |    -4 |     -7 |   0.92 |
|                                                                              [x true], (pr-str x) |    1000 |         14 |    13 |    -1 |     -7 |   0.93 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         26 |    24 |    -2 |     -7 |   0.92 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        757 |   699 |   -58 |     -7 |   0.92 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        146 |   138 |    -8 |     -5 |   0.95 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |        154 |   145 |    -9 |     -5 |   0.94 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        467 |   443 |   -24 |     -5 |   0.95 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        697 |   661 |   -36 |     -5 |   0.95 |
|                                                                                 [], (str "1" "2") | 1000000 |        392 |   372 |   -20 |     -5 |   0.95 |
|                                                                           [], (simple-multi :foo) | 1000000 |        850 |   808 |   -42 |     -4 |   0.95 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        143 |   137 |    -6 |     -4 |   0.96 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        142 |   136 |    -6 |     -4 |   0.96 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        621 |   596 |   -25 |     -4 |   0.96 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         72 |    69 |    -3 |     -4 |   0.96 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        757 |   728 |   -29 |     -3 |   0.96 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         51 |    49 |    -2 |     -3 |   0.96 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        156 |   150 |    -6 |     -3 |   0.96 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        438 |   428 |   -10 |     -2 |   0.98 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        426 |   414 |   -12 |     -2 |   0.97 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        285 |   278 |    -7 |     -2 |   0.98 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        881 |   859 |   -22 |     -2 |   0.98 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        421 |   409 |   -12 |     -2 |   0.97 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         70 |    69 |    -1 |     -1 |   0.99 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        342 |   337 |    -5 |     -1 |   0.99 |
|                                                                                            [], [] | 1000000 |          8 |     8 |     0 |      0 |   1.00 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        576 |   577 |     1 |      0 |   1.00 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        136 |   135 |    -1 |      0 |   0.99 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         21 |    21 |     0 |      0 |   1.00 |
|                                                                                   [r r], (last r) |       1 |        448 |   448 |     0 |      0 |   1.00 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        228 |   229 |     1 |      0 |   1.00 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         75 |    75 |     0 |      0 |   1.00 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         26 |    26 |     0 |      0 |   1.00 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        200 |   201 |     1 |      0 |   1.00 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |        134 |   135 |     1 |      0 |   1.01 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         90 |    90 |     0 |      0 |   1.00 |
|                                                                     [r (range 1000000)], (last r) |       1 |        166 |   167 |     1 |      0 |   1.01 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        513 |   514 |     1 |      0 |   1.00 |
|                                                                                     [], (str nil) | 1000000 |         13 |    13 |     0 |      0 |   1.00 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        856 |   853 |    -3 |      0 |   1.00 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         53 |    53 |     0 |      0 |   1.00 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1587 |  1588 |     1 |      0 |   1.00 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         28 |    28 |     0 |      0 |   1.00 |
|                                                                               [x 1], (identity x) | 1000000 |         12 |    12 |     0 |      0 |   1.00 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        573 |   568 |    -5 |      0 |   0.99 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         67 |    67 |     0 |      0 |   1.00 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1858 |  1870 |    12 |      0 |   1.01 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         53 |    53 |     0 |      0 |   1.00 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        231 |   231 |     0 |      0 |   1.00 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         29 |    29 |     0 |      0 |   1.00 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        424 |   427 |     3 |      0 |   1.01 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        176 |   178 |     2 |      1 |   1.01 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        268 |   272 |     4 |      1 |   1.01 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        176 |   179 |     3 |      1 |   1.02 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         68 |    70 |     2 |      2 |   1.03 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        139 |   142 |     3 |      2 |   1.02 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         54 |    56 |     2 |      3 |   1.04 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        319 |   330 |    11 |      3 |   1.03 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         25 |    26 |     1 |      4 |   1.04 |
|                                                                                [x 10], (pr-str x) |    1000 |         16 |    17 |     1 |      6 |   1.06 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |        157 |   167 |    10 |      6 |   1.06 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |        119 |   128 |     9 |      7 |   1.08 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |        244 |   264 |    20 |      8 |   1.08 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         55 |    60 |     5 |      9 |   1.09 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         75 |    83 |     8 |     10 |   1.11 |
|                                                                                       [], (str 1) | 1000000 |         49 |    55 |     6 |     12 |   1.12 |
|                                                                                     [], (str "1") | 1000000 |         31 |    35 |     4 |     12 |   1.13 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         24 |    27 |     3 |     12 |   1.13 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        279 |   318 |    39 |     13 |   1.14 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         82 |    93 |    11 |     13 |   1.13 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        111 |   126 |    15 |     13 |   1.14 |
|                                                                                  [], (list 1 2 3) | 1000000 |         29 |    33 |     4 |     13 |   1.14 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        151 |   176 |    25 |     16 |   1.17 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         53 |    62 |     9 |     16 |   1.17 |
|                                                                                        [], (list) | 1000000 |         28 |    33 |     5 |     17 |   1.18 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        155 |   189 |    34 |     21 |   1.22 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        157 |   191 |    34 |     21 |   1.22 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        114 |   140 |    26 |     22 |   1.23 |
|                                                                                            Totals |         |      26730 | 25717 | -1013 |        |   0.96 |

