(ns examples.macro)

(clojure.core/defn hello
  [x]
  (clojure.core/prn [:hello x]))

(hello "Joni")
