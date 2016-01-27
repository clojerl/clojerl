(ns examples.fn)

(def one 1)

(def fixed-arity (fn* [x y] (clj_core/str [x y])))

(def multiple-fixed-arities
  (fn*
   ([x] (clj_core/str x))
   ([x y] (clj_core/str [x y]))
   ([x y z] (clj_core/str [x y z]))))

(def variadic-arity
  (fn* [& xs] (clj_core/str xs)))

(def multiple-variadic
  (fn*
   ([x] (clj_core/str [1 x]))
   ([x & y] (clj_core/str [x y]))))

;; Resolve var in another ns
(clojure.core/prn {:a 2, #{1 2 2} 1})
(clojure.core/prn clojure.core/prn)
(clojure.core/assert (clojure.core/= (clojure.core/str one) "1"))

(clojure.core/prn (if :test
                    (do
                      (clojure.core/prn ::then)
                      :then)
                    :else))

(clojure.core/prn (fixed-arity ::fixed ::arity))

(clojure.core/prn (multiple-fixed-arities :mult-fixed))
(clojure.core/prn (multiple-fixed-arities :a :b))
;; (clojure.core/prn (multiple-fixed-arities 1 2 3))

;;(clojure.core/prn (variadic-arity 1 2 4))
;; (clojure.core/prn (variadic-arity 1 2 3))
;; (clojure.core/prn (variadic-arity 1 2 3 4))

;; (clojure.core/prn (multiple-variadic 1))
;; (clojure.core/prn (multiple-variadic 1 2 3 4 5))

#_(clojure.core/prn 1)

(clojure.core/prn 'last-one)
