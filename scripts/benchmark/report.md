|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |        432 |   255 |  -177 |    -40 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       3302 |  1980 | -1322 |    -40 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        199 |   125 |   -74 |    -37 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        215 |   154 |   -61 |    -28 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        212 |   156 |   -56 |    -26 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        719 |   536 |  -183 |    -25 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         34 |    26 |    -8 |    -23 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         31 |    26 |    -5 |    -16 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        336 |   284 |   -52 |    -15 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        214 |   184 |   -30 |    -14 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         84 |    76 |    -8 |     -9 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        247 |   226 |   -21 |     -8 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        197 |   180 |   -17 |     -8 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |       1325 |  1227 |   -98 |     -7 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        120 |   111 |    -9 |     -7 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        960 |   888 |   -72 |     -7 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        320 |   300 |   -20 |     -6 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        359 |   338 |   -21 |     -5 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |        159 |   151 |    -8 |     -5 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |       1540 |  1470 |   -70 |     -4 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         46 |    44 |    -2 |     -4 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        189 |   181 |    -8 |     -4 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        624 |   595 |   -29 |     -4 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |        228 |   220 |    -8 |     -3 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |       1677 |  1616 |   -61 |     -3 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |        227 |   222 |    -5 |     -2 |
|                                                                                     [], (str "1") | 1000000 |         36 |    35 |    -1 |     -2 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         76 |    74 |    -2 |     -2 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         90 |    88 |    -2 |     -2 |
|                                                                     [r (range 1000000)], (last r) |       1 |        174 |   169 |    -5 |     -2 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |        376 |   366 |   -10 |     -2 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         90 |    88 |    -2 |     -2 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        570 |   564 |    -6 |     -1 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         67 |    66 |    -1 |     -1 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |       1291 |  1276 |   -15 |     -1 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         52 |    51 |    -1 |     -1 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         75 |    75 |     0 |      0 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |        123 |   123 |     0 |      0 |
|                                                                                            [], [] | 1000000 |          8 |     8 |     0 |      0 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         22 |    22 |     0 |      0 |
|                                                                           [], (simple-multi :foo) | 1000000 |        874 |   879 |     5 |      0 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         45 |    45 |     0 |      0 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        763 |   764 |     1 |      0 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        150 |   150 |     0 |      0 |
|                                                                                     [], (str nil) | 1000000 |         18 |    18 |     0 |      0 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         55 |    55 |     0 |      0 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1590 |  1585 |    -5 |      0 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         28 |    28 |     0 |      0 |
|                                                                               [x 1], (identity x) | 1000000 |         11 |    11 |     0 |      0 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        768 |   763 |    -5 |      0 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |       1019 |  1014 |    -5 |      0 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        594 |   594 |     0 |      0 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |       1268 |  1280 |    12 |      0 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        223 |   223 |     0 |      0 |
|                                                                                       [], (str 1) | 1000000 |         52 |    53 |     1 |      1 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        892 |   901 |     9 |      1 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |        240 |   244 |     4 |      1 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         58 |    59 |     1 |      1 |
|                                                                                 [], (str "1" "2") | 1000000 |        966 |   983 |    17 |      1 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        979 |  1003 |    24 |      2 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        195 |   199 |     4 |      2 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        779 |   796 |    17 |      2 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        281 |   288 |     7 |      2 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        873 |   899 |    26 |      2 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        439 |   453 |    14 |      3 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         56 |    58 |     2 |      3 |
|                                                                                        [], (list) | 1000000 |         28 |    29 |     1 |      3 |
|                                                                                   [r r], (last r) |       1 |        439 |   456 |    17 |      3 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |        136 |   141 |     5 |      3 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        845 |   876 |    31 |      3 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         30 |    31 |     1 |      3 |
|                                                                                  [], (list 1 2 3) | 1000000 |         28 |    29 |     1 |      3 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        176 |   183 |     7 |      3 |
|                                                                             [], (str "1" "2" "3") | 1000000 |       1456 |  1528 |    72 |      4 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        475 |   498 |    23 |      4 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        570 |   602 |    32 |      5 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         54 |    57 |     3 |      5 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        174 |   186 |    12 |      6 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        361 |   385 |    24 |      6 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        573 |   614 |    41 |      7 |
|                                                                              [x true], (pr-str x) |    1000 |         13 |    14 |     1 |      7 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        512 |   549 |    37 |      7 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        139 |   149 |    10 |      7 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        636 |   697 |    61 |      9 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        703 |   781 |    78 |     11 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |        145 |   163 |    18 |     12 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         25 |    28 |     3 |     12 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         24 |    27 |     3 |     12 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         97 |   110 |    13 |     13 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         29 |    33 |     4 |     13 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         86 |    98 |    12 |     13 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       2653 |  3156 |   503 |     18 |
|                                                                                [x 10], (pr-str x) |    1000 |         14 |    17 |     3 |     21 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         10 |    24 |    14 |    140 |
