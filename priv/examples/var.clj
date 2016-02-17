(ns examples.var)

(def x 1)

(clojure.core/prn (var x))

(clojure.core/prn #'x)
