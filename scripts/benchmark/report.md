
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |        255 |    67 |  -188 |    -73 |
|                                                                             [], (str "1" "2" "3") | 1000000 |       1528 |   819 |  -709 |    -46 |
|                                                                                 [], (str "1" "2") | 1000000 |        983 |   546 |  -437 |    -44 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |       1276 |   867 |  -409 |    -32 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        594 |   412 |  -182 |    -30 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |       1280 |   904 |  -376 |    -29 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        595 |   429 |  -166 |    -27 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       3156 |  2367 |  -789 |    -25 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |       1470 |  1219 |  -251 |    -17 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |       1227 |  1023 |  -204 |    -16 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |       1616 |  1388 |  -228 |    -14 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        764 |   660 |  -104 |    -13 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        899 |   780 |  -119 |    -13 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         33 |    29 |    -4 |    -12 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         27 |    24 |    -3 |    -11 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |        110 |    99 |   -11 |    -10 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |        163 |   147 |   -16 |     -9 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         28 |    26 |    -2 |     -7 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        149 |   138 |   -11 |     -7 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |       1003 |   934 |   -69 |     -6 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         59 |    55 |    -4 |     -6 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         75 |    71 |    -4 |     -5 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         58 |    55 |    -3 |     -5 |
|                                                                                [x 10], (pr-str x) |    1000 |         17 |    16 |    -1 |     -5 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        536 |   507 |   -29 |     -5 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         57 |    54 |    -3 |     -5 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        763 |   720 |   -43 |     -5 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        186 |   177 |    -9 |     -4 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        180 |   172 |    -8 |     -4 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |        141 |   134 |    -7 |     -4 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         24 |    23 |    -1 |     -4 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        125 |   119 |    -6 |     -4 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         31 |    30 |    -1 |     -3 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         26 |    25 |    -1 |     -3 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         26 |    25 |    -1 |     -3 |
|                                                                                  [], (list 1 2 3) | 1000000 |         29 |    28 |    -1 |     -3 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        184 |   177 |    -7 |     -3 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        156 |   152 |    -4 |     -2 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        226 |   221 |    -5 |     -2 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        549 |   534 |   -15 |     -2 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        697 |   680 |   -17 |     -2 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |        244 |   239 |    -5 |     -2 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        453 |   448 |    -5 |     -1 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        888 |   873 |   -15 |     -1 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         55 |    54 |    -1 |     -1 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |        366 |   361 |    -5 |     -1 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |        123 |   123 |     0 |      0 |
|                                                                                            [], [] | 1000000 |          8 |     8 |     0 |      0 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        181 |   182 |     1 |      0 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        284 |   284 |     0 |      0 |
|                                                                                   [r r], (last r) |       1 |        456 |   458 |     2 |      0 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        338 |   339 |     1 |      0 |
|                                                                              [x true], (pr-str x) |    1000 |         14 |    14 |     0 |      0 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        150 |   149 |    -1 |      0 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        154 |   154 |     0 |      0 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         88 |    88 |     0 |      0 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        498 |   502 |     4 |      0 |
|                                                                                     [], (str nil) | 1000000 |         18 |    18 |     0 |      0 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1585 |  1573 |   -12 |      0 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         28 |    28 |     0 |      0 |
|                                                                               [x 1], (identity x) | 1000000 |         11 |    11 |     0 |      0 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         66 |    66 |     0 |      0 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        183 |   183 |     0 |      0 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |        222 |   225 |     3 |      1 |
|                                                                     [r (range 1000000)], (last r) |       1 |        169 |   172 |     3 |      1 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |       1014 |  1028 |    14 |      1 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1980 |  2002 |    22 |      1 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        300 |   304 |     4 |      1 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         98 |    99 |     1 |      1 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |        220 |   226 |     6 |      2 |
|                                                                           [], (simple-multi :foo) | 1000000 |        879 |   902 |    23 |      2 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         45 |    46 |     1 |      2 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        288 |   294 |     6 |      2 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        111 |   115 |     4 |      3 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        199 |   206 |     7 |      3 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |        151 |   157 |     6 |      3 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         76 |    79 |     3 |      3 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        223 |   230 |     7 |      3 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         51 |    53 |     2 |      3 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        781 |   815 |    34 |      4 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         22 |    23 |     1 |      4 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        876 |   919 |    43 |      4 |
|                                                                                       [], (str 1) | 1000000 |         53 |    56 |     3 |      5 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        564 |   595 |    31 |      5 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        385 |   405 |    20 |      5 |
|                                                                                        [], (list) | 1000000 |         29 |    31 |     2 |      6 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         44 |    47 |     3 |      6 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        901 |   962 |    61 |      6 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         74 |    80 |     6 |      8 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        602 |   660 |    58 |      9 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        796 |   869 |    73 |      9 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        614 |   682 |    68 |     11 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         88 |    98 |    10 |     11 |
|                                                                                     [], (str "1") | 1000000 |         35 |    40 |     5 |     14 |

