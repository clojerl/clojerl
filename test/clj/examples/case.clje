(ns examples.case)

(let* [x :b
       y (case* x
                :a 1
                :b 2
                :c 3)
       z (case* y
                x 2
                #erl [:default :value])]
  (erlang/display x)
  (erlang/display y)
  (erlang/display z))
