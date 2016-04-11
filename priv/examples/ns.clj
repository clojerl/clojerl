(ns examples.ns
  (:use examples.simple)
  (:require [examples.simple :as simple])
  (:use [examples.fn :only [one]]))

(defn ten-ten [] (repeat 10 10))

(prn (seq (ten-ten)))

(prn :a)

(prn x)

(prn simple/x)

(prn 'examples.fn/one " = " one)
