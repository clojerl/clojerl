(ns examples.pattern)

(def f (fn* ([x x] x) ([x y] y)))

(def g (fn* ([#erl{:x x} x] x)
            ([_ _] :whatever)))

(let* [:foo (g #erl{:x :foo} :foo)
       :whatever (g #erl{:x :foo} :bar)
       :whatever (g :foo :bar)

       #erl{:bar bar :baz bar} #erl{:bar 1 :baz 1 :foo 2}

       3 (case* #erl[1 2]
           #erl[one two] (erlang/+.e one two)
           2 :two)])

(let* [#bin[[h :type :utf8] [ello :type :binary]] "hello"
       104    h
       "ello" ello])

(let* [#erl(a b) #erl(1 2)
       #erl{1 2} #erl{a b}])

(try
  (throw #erl[:invalid :hello])
  (catch :throw #erl[x reason]
    (let* [:invalid x :hello reason])))
