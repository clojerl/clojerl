
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|
|                                                                                [x 10], (pr-str x) |    1000 |         24 |    15 |    -9 |    -37 |
|                                                                              [x true], (pr-str x) |    1000 |         18 |    15 |    -3 |    -16 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        508 |   433 |   -75 |    -14 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |        164 |   143 |   -21 |    -12 |
|                                                                                            [], [] | 1000000 |          9 |     8 |    -1 |    -11 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         59 |    53 |    -6 |    -10 |
|                                                                                   [r r], (last r) |       1 |        507 |   464 |   -43 |     -8 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        157 |   143 |   -14 |     -8 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        640 |   591 |   -49 |     -7 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         27 |    25 |    -2 |     -7 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1975 |  1852 |  -123 |     -6 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        484 |   459 |   -25 |     -5 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |        127 |   120 |    -7 |     -5 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        299 |   282 |   -17 |     -5 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         58 |    55 |    -3 |     -5 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        181 |   171 |   -10 |     -5 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        185 |   176 |    -9 |     -4 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       2546 |  2430 |  -116 |     -4 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         71 |    68 |    -3 |     -4 |
|                                                                                 [], (str "1" "2") | 1000000 |        589 |   564 |   -25 |     -4 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        892 |   862 |   -30 |     -3 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         27 |    26 |    -1 |     -3 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        174 |   169 |    -5 |     -2 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        455 |   442 |   -13 |     -2 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         83 |    81 |    -2 |     -2 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        379 |   370 |    -9 |     -2 |
|                                                                                       [], (str 1) | 1000000 |         59 |    58 |    -1 |     -1 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        224 |   221 |    -3 |     -1 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        831 |   838 |     7 |      0 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         57 |    57 |     0 |      0 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        157 |   158 |     1 |      0 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        514 |   515 |     1 |      0 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        195 |   194 |    -1 |      0 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |        136 |   135 |    -1 |      0 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         52 |    52 |     0 |      0 |
|                                                                               [x 1], (identity x) | 1000000 |         12 |    12 |     0 |      0 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        383 |   381 |    -2 |      0 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         24 |    24 |     0 |      0 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        397 |   396 |    -1 |      0 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        645 |   641 |    -4 |      0 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         29 |    29 |     0 |      0 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        406 |   408 |     2 |      0 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        133 |   133 |     0 |      0 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         55 |    55 |     0 |      0 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        269 |   274 |     5 |      1 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        224 |   227 |     3 |      1 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        155 |   157 |     2 |      1 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        519 |   526 |     7 |      1 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        790 |   800 |    10 |      1 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         78 |    80 |     2 |      2 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        711 |   729 |    18 |      2 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        346 |   356 |    10 |      2 |
|                                                                     [r (range 1000000)], (last r) |       1 |        179 |   183 |     4 |      2 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1563 |  1596 |    33 |      2 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        364 |   373 |     9 |      2 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         80 |    83 |     3 |      3 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        474 |   491 |    17 |      3 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |        214 |   222 |     8 |      3 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        532 |   552 |    20 |      3 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         46 |    48 |     2 |      4 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         22 |    23 |     1 |      4 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         67 |    70 |     3 |      4 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         24 |    25 |     1 |      4 |
|                                                                                     [], (str "1") | 1000000 |         38 |    40 |     2 |      5 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        474 |   499 |    25 |      5 |
|                                                                                     [], (str nil) | 1000000 |         18 |    19 |     1 |      5 |
|                                                                           [], (simple-multi :foo) | 1000000 |        911 |   973 |    62 |      6 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |        159 |   170 |    11 |      6 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        984 |  1047 |    63 |      6 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         87 |    93 |     6 |      6 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        272 |   292 |    20 |      7 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        114 |   122 |     8 |      7 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        272 |   292 |    20 |      7 |
|                                                                                  [], (list 1 2 3) | 1000000 |         27 |    29 |     2 |      7 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         48 |    52 |     4 |      8 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         68 |    74 |     6 |      8 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |        344 |   373 |    29 |      8 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         83 |    90 |     7 |      8 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        110 |   120 |    10 |      9 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         81 |    89 |     8 |      9 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        164 |   181 |    17 |     10 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        134 |   148 |    14 |     10 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        830 |   914 |    84 |     10 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |        199 |   221 |    22 |     11 |
|                                                                                        [], (list) | 1000000 |         26 |    29 |     3 |     11 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         26 |    29 |     3 |     11 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        794 |   892 |    98 |     12 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        158 |   179 |    21 |     13 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         20 |    23 |     3 |     15 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         33 |    38 |     5 |     15 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        768 |   889 |   121 |     15 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        127 |   162 |    35 |     27 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        230 |   298 |    68 |     29 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |        171 |   237 |    66 |     38 |

