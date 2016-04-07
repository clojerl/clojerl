(ns examples.ns
  (:use examples.simple))

(defn ten-ten [] (repeat 10 10))

(prn (seq (ten-ten)))

(prn :a)

(prn x)
