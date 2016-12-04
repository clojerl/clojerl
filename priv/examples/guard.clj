(ns examples.guard)

(def f1
  (fn*
   ([x] {:when (erlang/is_tuple.e x)}
    [:tuple x])
   ([x] {:when (erlang/is_binary.e x)}
    [:string x])))
