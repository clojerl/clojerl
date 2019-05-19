
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       2430 |  1416 | -1014 |    -41 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        862 |   610 |  -252 |    -29 |
|                                                                                     [], (str nil) | 1000000 |         19 |    14 |    -5 |    -26 |
|                                                                                 [], (str "1" "2") | 1000000 |        564 |   414 |  -150 |    -26 |
|                                                                                [x 10], (pr-str x) |    1000 |         15 |    13 |    -2 |    -13 |
|                                                                     [r (range 1000000)], (last r) |       1 |        183 |   164 |   -19 |    -10 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        459 |   419 |   -40 |     -8 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |        170 |   156 |   -14 |     -8 |
|                                                                               [x 1], (identity x) | 1000000 |         12 |    11 |    -1 |     -8 |
|                                                                              [x true], (pr-str x) |    1000 |         15 |    14 |    -1 |     -6 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         80 |    76 |    -4 |     -5 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         57 |    54 |    -3 |     -5 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        729 |   689 |   -40 |     -5 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        120 |   114 |    -6 |     -5 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         38 |    36 |    -2 |     -5 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         23 |    22 |    -1 |     -4 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         70 |    67 |    -3 |     -4 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         25 |    24 |    -1 |     -4 |
|                                                                                       [], (str 1) | 1000000 |         58 |    56 |    -2 |     -3 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         52 |    50 |    -2 |     -3 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         26 |    25 |    -1 |     -3 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        499 |   480 |   -19 |     -3 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         55 |    53 |    -2 |     -3 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        889 |   854 |   -35 |     -3 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         83 |    81 |    -2 |     -2 |
|                                                                                   [r r], (last r) |       1 |        464 |   451 |   -13 |     -2 |
|                                                                           [], (simple-multi :foo) | 1000000 |        973 |   950 |   -23 |     -2 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         48 |    47 |    -1 |     -2 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         68 |    66 |    -2 |     -2 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        641 |   625 |   -16 |     -2 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         93 |    91 |    -2 |     -2 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        158 |   156 |    -2 |     -1 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        176 |   174 |    -2 |     -1 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        526 |   518 |    -8 |     -1 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        373 |   367 |    -6 |     -1 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |       1047 |  1029 |   -18 |     -1 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        892 |   880 |   -12 |     -1 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        800 |   792 |    -8 |     -1 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        552 |   544 |    -8 |     -1 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         55 |    54 |    -1 |     -1 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        838 |   839 |     1 |      0 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |        120 |   119 |    -1 |      0 |
|                                                                                            [], [] | 1000000 |          8 |     8 |     0 |      0 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        433 |   434 |     1 |      0 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        491 |   493 |     2 |      0 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        181 |   182 |     1 |      0 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         74 |    74 |     0 |      0 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        179 |   178 |    -1 |      0 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        356 |   353 |    -3 |      0 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |        135 |   134 |    -1 |      0 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         89 |    89 |     0 |      0 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1596 |  1595 |    -1 |      0 |
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         29 |    29 |     0 |      0 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         25 |    25 |     0 |      0 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1852 |  1853 |     1 |      0 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         53 |    53 |     0 |      0 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         29 |    29 |     0 |      0 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         90 |    90 |     0 |      0 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        227 |   231 |     4 |      1 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        148 |   150 |     2 |      1 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        292 |   297 |     5 |      1 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        591 |   600 |     9 |      1 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        381 |   386 |     5 |      1 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        274 |   282 |     8 |      2 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |        143 |   146 |     3 |      2 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        143 |   146 |     3 |      2 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        194 |   199 |     5 |      2 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         81 |    83 |     2 |      2 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        370 |   380 |    10 |      2 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        914 |   933 |    19 |      2 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        292 |   298 |     6 |      2 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |        221 |   228 |     7 |      3 |
|                                                                                        [], (list) | 1000000 |         29 |    30 |     1 |      3 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        122 |   126 |     4 |      3 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |        373 |   385 |    12 |      3 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |        237 |   246 |     9 |      3 |
|                                                                                  [], (list 1 2 3) | 1000000 |         29 |    30 |     1 |      3 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        133 |   138 |     5 |      3 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         23 |    24 |     1 |      4 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         24 |    25 |     1 |      4 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        162 |   170 |     8 |      4 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        221 |   232 |    11 |      4 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        515 |   544 |    29 |      5 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        157 |   166 |     9 |      5 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        171 |   181 |    10 |      5 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        169 |   180 |    11 |      6 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |        222 |   236 |    14 |      6 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        396 |   423 |    27 |      6 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        298 |   318 |    20 |      6 |
|                                                                                     [], (str "1") | 1000000 |         40 |    43 |     3 |      7 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        442 |   474 |    32 |      7 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         52 |    56 |     4 |      7 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        408 |   440 |    32 |      7 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        282 |   310 |    28 |      9 |

