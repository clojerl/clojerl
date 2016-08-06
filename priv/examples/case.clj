(ns examples.case)

(let* [x :b
       y (case* x
                :a 1
                :b 2
                :c 3)
       z (case* y
                x 2
                #[:default :value])]
  (erlang/display.e x)
  (erlang/display.e y)
  (erlang/display.e z))
