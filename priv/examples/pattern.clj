(ns examples.pattern)

(def f (fn* ([x x] x) ([x y] y)))

(def g (fn* ([#erl{:x x} x] x)
            ([_ _] :whatever)))

(println (f :foo :foo))

(println (f :foo :bar))

(println (g #erl{:x :foo} :foo))
(println (g #erl{:x :foo} :bar))
(println (g :foo :bar))
