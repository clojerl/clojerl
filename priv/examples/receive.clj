(ns examples.receive)

(def loop
  (fn* [state]
    (receive*
     x {:when (erlang/=:=.e x :inc)}
     (-> state inc loop)

     x {:when (erlang/=:=.e x :dec)}
     (-> state dec loop)

     x {:when (erlang/=:=.e x :print)}
     (loop (doto state prn))

     _
     (loop state))))

(let [x (erlang/spawn.e :examples.receive :loop '#erl (42))]
  (erlang/send.e x :inc)
  (erlang/send.e x :inc)
  (erlang/send.e x :inc)
  (erlang/send.e x :print)
  (erlang/send.e x :dec)
  (erlang/send.e x :dec)
  (erlang/send.e x :dec)
  (erlang/send.e x :print))
