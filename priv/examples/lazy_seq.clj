(ns examples.lazy-seq)

(clojure.core/->>
 (clojure.core/map #(clojure.core/prn %) [1 2 3 4])
 clojure.core/seq
 (clojure.core/drop 1)
 (clojure.core/take 2)
 clojure.core/doall
 clojure.core/prn)
