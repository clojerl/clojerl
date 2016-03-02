(ns examples.macro)

(def cons (fn* [x s] (clj_core/cons.e x s)))
(def seq (fn* [s] (clj_core/seq.e s)))

(def ^:macro defn
  (fn* [form env name args & body]
       (cons 'def
             (cons name
                   [(cons 'fn* (cons args [body]))])
                   )))

(defn hello [name]
  (io/format.e "~s~n" (seq [name])))

(hello "Moto")
