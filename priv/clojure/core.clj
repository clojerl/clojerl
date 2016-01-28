(ns clojure.core)

(def
  seq (fn* [items] (clj_core/seq items)))

(def
  ^{:arglists '([& items])
    :doc "Creates a new list containing the items."
    :added "1.0"}
  list (fn* [& items] (clojerl.List/new (seq items))))

(def
  ^{:arglists '([x seq])
    :doc "Returns a new seq where x is the first element and seq is
    the rest."
    :added "1.0"
    :static true}
  cons (fn* [x s] (clj_core/cons x s)))

(def str
  (fn* [x] (clj_core/str x)))

(def prn
  (fn* [x]
       (io/format "~s~n" (seq [(str x)]))))

(def =
  (fn* [a b] (erlang/== a b)))

(def not
  (fn* [a] (erlang/not a)))

(def <-
  (fn* [a b] (erlang/=:= a b)))

(def assert ^:macro
  (fn* [v] (if (not v) (throw :assert))))
