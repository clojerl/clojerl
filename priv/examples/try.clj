(ns examples.try)

(try (clojure.core/prn 1))

(try (erlang/+.e 1 :a)
     (catch :error error
       (clojure.core/prn [:error error])))

(try (throw :hello)
     (catch :error error
       (clojure.core/prn error))
     (catch :exit error
       (clojure.core/prn error))
     (catch :throw error
       (clojure.core/prn [:throw error])))

(try (throw :hello-before-finally)
     (catch :error error
       (clojure.core/prn error))
     (catch :exit error
       (clojure.core/prn error))
     (catch :throw error
       (clojure.core/prn [:throw error]))
     (finally
       (clojure.core/prn :finally)))
