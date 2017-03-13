(ns examples.pattern)

(def f (fn* ([x x] x) ([x y] y)))

(def g (fn* ([#erl{:x x} x] x)
            ([_ _] :whatever)))

(let* [:foo (g #erl{:x :foo} :foo)])
(let* [:whatever (g #erl{:x :foo} :bar)])
(let* [:whatever (g :foo :bar)])

(let* [#erl{:bar bar :baz bar} #erl{:bar 1 :baz 1 :foo 2}])

(try
  (throw #erl[:invalid :hello])
  (catch :throw #erl[x reason]
    (let* [:invalid x :hello reason])))

(let* [3 (case* #erl[1 2]
                #erl[one two] (erlang/+.e one two)
                2 :two)])
