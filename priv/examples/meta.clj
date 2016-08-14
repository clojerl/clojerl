(ns examples.meta)

(def x ^{:hello "world"} [])

(prn (meta x))
