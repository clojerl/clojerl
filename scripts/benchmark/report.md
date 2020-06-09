
|                                                                                             :expr |   :runs | :time-prev | :time | :diff | :diff% | :ratio |
|---------------------------------------------------------------------------------------------------|---------|------------|-------|-------|--------|--------|
|                                                                              [], (list 1 2 3 4 5) | 1000000 |         33 |    29 |    -4 |    -12 |   0.88 |
|                                                         [coll (range 1000000)], (reduce + 0 coll) |       1 |        164 |   147 |   -17 |    -10 |   0.90 |
|                                                 [coll {(quote foo) 1, (quote bar) 2}], (sym coll) | 1000000 |        205 |   188 |   -17 |     -8 |   0.92 |
|                                                                       [coll "foobar"], (seq coll) | 1000000 |         47 |    43 |    -4 |     -8 |   0.91 |
|                                                                                [x 10], (pr-str x) |    1000 |         12 |    11 |    -1 |     -8 |   0.92 |
|                                                         [coll (new Foo 1 2)], (assoc coll :bar 2) | 1000000 |         80 |    73 |    -7 |     -8 |   0.91 |
|                    [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vector a b c)) | 1000000 |        182 |   166 |   -16 |     -8 |   0.91 |
|   [coll (new Foo 1 2)], (loop [i 0 m coll] (if (< i 1000000) (recur (inc i) (assoc m :bar 2)) m)) |       1 |         80 |    73 |    -7 |     -8 |   0.91 |
|                                                         [coll (new Foo 1 2)], (assoc coll :baz 3) | 1000000 |         81 |    76 |    -5 |     -6 |   0.94 |
|                                                         [], (transduce (take 64) + (repeat 48 1)) |   10000 |        232 |   219 |   -13 |     -5 |   0.94 |
|                                                                             [], (str "1" "2" "3") | 1000000 |        449 |   424 |   -25 |     -5 |   0.94 |
|                                                                [f tuple], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         21 |    20 |    -1 |     -4 |   0.95 |
|                                                                     [], (into [] (repeat 1000 1)) |    1000 |        263 |   251 |   -12 |     -4 |   0.95 |
|                                                            [xs (list 1 2 3 4 5)], (apply list xs) | 1000000 |        236 |   228 |    -8 |     -3 |   0.97 |
|                                                           [], (doall (take 1000 (cycle [1 2 3]))) |    1000 |        524 |   505 |   -19 |     -3 |   0.96 |
|                                                                 [coll (new Foo 1 2)], (:bar coll) | 1000000 |         86 |    83 |    -3 |     -3 |   0.97 |
|                                         [coll {(quote foo) 1, (quote bar) 2}], ((quote foo) coll) | 1000000 |        188 |   181 |    -7 |     -3 |   0.96 |
|                [v (into [] (range 1000000))], (loop [[x & xs] v] (if-not (nil? xs) (recur xs) x)) |      10 |       1834 |  1784 |   -50 |     -2 |   0.97 |
|                                                                     [coll "foobar"], (nth coll 2) | 1000000 |        176 |   171 |    -5 |     -2 |   0.97 |
|                                               [coll (into [] (range 1000000))], (reduce + 0 coll) |       1 |        147 |   144 |    -3 |     -2 |   0.98 |
|                                                        [s "a" f clojure.string/capitalize], (f s) | 1000000 |        376 |   367 |    -9 |     -2 |   0.98 |
|                                                            [coll (range 500000)], (reduce + coll) |       1 |         73 |    72 |    -1 |     -1 |   0.99 |
|                                                          [coll {:foo 1, :bar 2}], (get coll :foo) | 1000000 |        114 |   112 |    -2 |     -1 |   0.98 |
|                                                                                 [], (str "1" "2") | 1000000 |        314 |   309 |    -5 |     -1 |   0.98 |
|           [xs (into [] (range 1000000))], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        660 |   649 |   -11 |     -1 |   0.98 |
|                                                                [], (reduce conj [] (range 40000)) |      10 |        113 |   111 |    -2 |     -1 |   0.98 |
|                                                  [coll (into [] (range 1000000))], (apply + coll) |       1 |         53 |    52 |    -1 |     -1 |   0.98 |
|                                                                        [coll [1 2 3]], (seq coll) | 1000000 |         84 |    83 |    -1 |     -1 |   0.99 |
|                                                [xs (range 512)], (last (for [x xs y xs] (+ x y))) |       1 |         69 |    68 |    -1 |     -1 |   0.99 |
|                                                         [], (into [] (take 1000) (iterate inc 0)) |    1000 |        660 |   647 |   -13 |     -1 |   0.98 |
|                                                               [f vector], (f 1 2 3 4 5 6 7 8 9 0) |  100000 |         80 |    80 |     0 |      0 |   1.00 |
|                                                                          [], (symbol (quote foo)) | 1000000 |         31 |    31 |     0 |      0 |   1.00 |
|                                                                     [coll "foobar"], (first coll) | 1000000 |        113 |   114 |     1 |      0 |   1.01 |
|                                                              [coll {:foo 1, :bar 2}], (:foo coll) | 1000000 |        122 |   122 |     0 |      0 |   1.00 |
|                                [a (into [] (range 1000000)) b (into [] (range 1000000))], (= a b) |       1 |        152 |   151 |    -1 |      0 |   0.99 |
|                                                                                  [], (list 1 2 3) | 1000000 |         30 |    30 |     0 |      0 |   1.00 |
|                                                                 [coll (seq [1 2 3])], (rest coll) | 1000000 |         33 |    33 |     0 |      0 |   1.00 |
|                                                         [], (into [] (take 1000) (cycle [1 2 3])) |    1000 |        614 |   608 |    -6 |      0 |   0.99 |
|                                                                [coll (tuple 1 2 3)], (nth coll 2) | 1000000 |         45 |    45 |     0 |      0 |   1.00 |
|                                                                               [x 1], (identity x) | 1000000 |         12 |    12 |     0 |      0 |   1.00 |
|                                                                [coll (seq [1 2 3])], (first coll) | 1000000 |         32 |    32 |     0 |      0 |   1.00 |
|                                                                                     [], (str nil) | 1000000 |         13 |    13 |     0 |      0 |   1.00 |
|                                                                      [coll [1 2 3]], (nth coll 0) | 1000000 |         81 |    81 |     0 |      0 |   1.00 |
|                           [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count [a b c]) | 1000000 |        162 |   163 |     1 |      0 |   1.01 |
|                                     [coll {(quote foo) 1, (quote bar) 2}], (get coll (quote foo)) | 1000000 |        183 |   183 |     0 |      0 |   1.00 |
|                                                                 [xs [1 2 3 4 5]], (apply list xs) | 1000000 |        294 |   292 |    -2 |      0 |   0.99 |
|                                                              [], (into [] (take 1000) (repeat 1)) |    1000 |        556 |   560 |     4 |      0 |   1.01 |
|                                                                       [], (= 1 1 1 1 1 1 1 1 1 0) |  100000 |         65 |    65 |     0 |      0 |   1.00 |
|                                                                          [coll [1 2 3]], (coll 0) | 1000000 |         72 |    72 |     0 |      0 |   1.00 |
|                                               [coll (list 1 2 3)], (satisfies? clojerl.ISeq coll) | 1000000 |         49 |    49 |     0 |      0 |   1.00 |
|                                                                                   [r r], (last r) |       1 |        201 |   199 |    -2 |      0 |   0.99 |
| [coll {:foo 1, :bar 2}], (loop [i 0 m coll] (if (< i 100000) (recur (inc i) (assoc m :foo 2)) m)) |       1 |         18 |    18 |     0 |      0 |   1.00 |
|                                                    [coll [1 2 3]], (satisfies? clojerl.ISeq coll) | 1000000 |         50 |    50 |     0 |      0 |   1.00 |
|                                                                                        [], (list) | 1000000 |         30 |    30 |     0 |      0 |   1.00 |
|                                                                [coll {:foo 1, :bar 2}], (kw coll) | 1000000 |        134 |   135 |     1 |      0 |   1.01 |
|                                                                                            [], [] | 1000000 |          9 |     9 |     0 |      0 |   1.00 |
|                                                                 [coll (seq [1 2 3])], (next coll) | 1000000 |         33 |    33 |     0 |      0 |   1.00 |
|                                       [f (fn [a b c d e f g h i j & more])], (apply f (range 32)) | 1000000 |        415 |   421 |     6 |      1 |   1.01 |
|                                                                 [s big-str-data], (read-string s) |    1000 |       1655 |  1688 |    33 |      1 |   1.02 |
|                                                   [s "aBcDeF" f clojure.string/capitalize], (f s) | 1000000 |       1361 |  1376 |    15 |      1 |   1.01 |
|                                          [xs (vec (range 512))], (last (for [x xs y xs] (+ x y))) |       4 |        258 |   261 |     3 |      1 |   1.01 |
|                                                                  [coll (list 1 2 3)], (rest coll) | 1000000 |         70 |    71 |     1 |      1 |   1.01 |
|                                                                  [coll (tuple 1 2 3)], (seq coll) | 1000000 |         45 |    46 |     1 |      2 |   1.02 |
|                                                            [], (transduce (take 64) + (repeat 1)) |   10000 |        297 |   304 |     7 |      2 |   1.02 |
|                                                                       [], (doall (repeat 1000 1)) |    1000 |        114 |   117 |     3 |      2 |   1.03 |
|                                           [coll (take 100000 (iterate inc 0))], (reduce + 0 coll) |       1 |         69 |    71 |     2 |      2 |   1.03 |
|                     [xs (range 1000000)], (transduce (comp (map inc) (map inc) (map inc)) + 0 xs) |       1 |        681 |   708 |    27 |      3 |   1.04 |
|                                                       [f (fn [a b & more])], (apply f (range 32)) | 1000000 |        402 |   417 |    15 |      3 |   1.04 |
|                               [xs (range 1000000)], (reduce + 0 (map inc (map inc (map inc xs)))) |       1 |        871 |   900 |    29 |      3 |   1.03 |
|                                      [m {:c 3, :b 2, :a 1}], (zipmap (keys m) (map inc (vals m))) |  100000 |        301 |   312 |    11 |      3 |   1.04 |
|                                                [coll {:foo 1} ks [:foo]], (update-in coll ks inc) | 1000000 |        715 |   743 |    28 |      3 |   1.04 |
|                     [[a b c] (take 3 (repeatedly (fn* [] (rand-int 10))))], (count (vec [a b c])) | 1000000 |        322 |   334 |    12 |      3 |   1.04 |
|                                                                  [], (reduce + 0 (repeat 1000 1)) |    1000 |        138 |   143 |     5 |      3 |   1.04 |
|                                                                 [coll (list 1 2 3)], (first coll) | 1000000 |         48 |    50 |     2 |      4 |   1.04 |
|                                                        [coll []], (instance? clojerl.Vector coll) | 1000000 |         25 |    26 |     1 |      4 |   1.04 |
|                                                               [], (reduce + (take 64 (repeat 1))) |   10000 |        330 |   344 |    14 |      4 |   1.04 |
|                                                             [s "{:foo [1 2 3]}"], (read-string s) |    1000 |         25 |    26 |     1 |      4 |   1.04 |
|                                                                [coll (tuple 1 2 3)], (first coll) | 1000000 |         70 |    73 |     3 |      4 |   1.04 |
|                                                                     [r (range 1000000)], (last r) |       1 |         79 |    83 |     4 |      5 |   1.05 |
|                                                            [], (reduce + (take 64 (repeat 48 1))) |   10000 |        254 |   269 |    15 |      5 |   1.06 |
|                                                                                     [], (str "1") | 1000000 |         19 |    20 |     1 |      5 |   1.05 |
|                                                          [], (reduce + (take 64 (cycle [1 2 3]))) |   10000 |        437 |   464 |    27 |      6 |   1.06 |
|                                    [coll (reduce conj [] (range (+ 32768 32)))], (conj coll :foo) |  100000 |         26 |    28 |     2 |      7 |   1.08 |
|                                                                           [], (simple-multi :foo) | 1000000 |        244 |   262 |    18 |      7 |   1.07 |
|                                                                                       [], (str 1) | 1000000 |         42 |    45 |     3 |      7 |   1.07 |
|                                          [coll (reduce conj [] (range (+ 32768 33)))], (pop coll) |  100000 |         23 |    25 |     2 |      8 |   1.09 |
|                                                                     [coll [1 2 3]], (conj coll 4) | 1000000 |        116 |   131 |    15 |     12 |   1.13 |
|                                                           [], (doall (take 1000 (iterate inc 0))) |    1000 |        517 |   599 |    82 |     15 |   1.16 |
|                                                                [], (doall (take 1000 (repeat 1))) |    1000 |        368 |   428 |    60 |     16 |   1.16 |
|                                      [coll (reduce conj [] (range 40000))], (assoc coll 123 :foo) |  100000 |         27 |    32 |     5 |     18 |   1.19 |
|                                                                              [x true], (pr-str x) |    1000 |         11 |    13 |     2 |     18 |   1.18 |
|                                                          [], (reduce + (take 64 (iterate inc 0))) |   10000 |        442 |   523 |    81 |     18 |   1.18 |
|                                                       [], (transduce (take 64) + (iterate inc 0)) |   10000 |        336 |   405 |    69 |     20 |   1.21 |
|                                                       [], (transduce (take 64) + (cycle [1 2 3])) |   10000 |        317 |   410 |    93 |     29 |   1.29 |
|               [xs (into [] (range 1000000))], (r/reduce + (r/map inc (r/map inc (r/map inc xs)))) |       1 |            |   649 |       |        |        |
|                                                                                            Totals |         |      22240 | 23313 |   424 |        |   1.05 |

