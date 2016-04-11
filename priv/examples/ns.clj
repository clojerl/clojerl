(ns examples.ns
  (:use examples.simple)
  (:require [examples.simple :as simple]))

(defn ten-ten [] (repeat 10 10))

(prn (seq (ten-ten)))

(prn :a)

(prn x)

(prn simple/x)
