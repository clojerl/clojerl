(ns examples.guard)

(def assert (fn* [bool] (if bool :ok (throw "Assertion failed"))))

(def simple-guard
  (fn*
   ([x] {:when (erlang/is_tuple.e x)}
    [:tuple x])
   ([x] {:when (erlang/is_binary.e x)}
    [:string x])))

(assert (clj_core/equiv.e (simple-guard #erl []) [:tuple #erl []]))
(assert (clj_core/equiv.e (simple-guard "hello") [:string "hello"]))
(assert (try (simple-guard 1) false (catch _ e true)))

(def simple-and-guard
  (fn*
   ([x] {:when [:and (erlang/is_integer.e x)
                     (erlang/>.e x 2)]}
    :more-than-2)
   ([x] {:when (erlang/is_integer.e x)}
    :less-than-2)))

(assert (clj_core/equiv.e (simple-and-guard 3) :more-than-2))
(assert (clj_core/equiv.e (simple-and-guard -1) :less-than-2))
(assert (try (simple-and-guard "2") false (catch _ e  true)))

(def simple-or-guard
  (fn* [x] {:when [:or (erlang/is_integer.e x)
                       (erlang/is_binary.e x)]}
   :integer-or-binary))

(assert (clj_core/equiv.e (simple-or-guard 3) :integer-or-binary))
(assert (clj_core/equiv.e (simple-or-guard "three") :integer-or-binary))
(assert (try (simple-or-guard :three) false (catch _ e true)))

(def nested-and-or
  (fn* [x] {:when [:and [:or (erlang/is_integer.e x)
                             (erlang/is_float.e x)]
                        (erlang/>.e x 1)]}
   :integer-or-float-more-than-1))

(assert (clj_core/equiv.e (nested-and-or 3) :integer-or-float-more-than-1))
(assert (clj_core/equiv.e (nested-and-or 3.0) :integer-or-float-more-than-1))
(assert (try (nested-and-or 1) false (catch _ e true)))
(assert (try (nested-and-or 1.0) false (catch _ e true)))
(assert (try (nested-and-or "one") false (catch _ e true)))
(assert (try (nested-and-or :one) false (catch _ e true)))
