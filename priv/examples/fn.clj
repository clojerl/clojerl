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
   ([x] (clj_core/str [:multiple-variadic-1 x]))
   ([x y] (clj_core/str [:multiple-variadic-2 x y]))
   ([x y z] (clj_core/str [:multiple-variadic-3 x y z]))
   ([x y z & w] (clj_core/str [:multiple-variadic-n x y z w]))))

(def apply
  (fn* [f x] (f x)))

;; Resolve var in another ns
(clojure.core/prn {:a 2, #{1 2 2} one})

;; Provide the value of an fn-var as an argument

(clojure.core/prn clojure.core/prn)

;; Use if

(clojure.core/prn (if :test
                    (do
                      (clojure.core/prn ::then)
                      :then)
                    :else))

;; Assert uses throw

(clojure.core/assert (clojure.core/= (clojure.core/str one) "1"))

;; Call a fn with single fixed arity

(clojure.core/prn (fixed-arity ::fixed ::arity))

;; Call a fn with multiple fixed arities

(clojure.core/prn (multiple-fixed-arities :mult-fixed))
(clojure.core/prn (multiple-fixed-arities :a :b))
(clojure.core/prn (multiple-fixed-arities 1 2 3))

;; Call a fn with a single variadic argument

(clojure.core/prn (variadic-arity 1 2 4))
(clojure.core/prn (variadic-arity 1 2 3))
(clojure.core/prn (variadic-arity 1 2 3 4))
(clojure.core/prn (variadic-arity))

;; Call a  variadic fn that also has fixed arities

(clojure.core/prn (multiple-variadic 1))
(clojure.core/prn (multiple-variadic 1 2))
(clojure.core/prn (multiple-variadic 1 2 3))
(clojure.core/prn (multiple-variadic 1 2 3 4))
(clojure.core/prn (multiple-variadic 1 2 3 4 5 6 7 8 9 :a :b :c))

;; Call an anonymous fn with a single fixed arity

(clojure.core/prn ((fn* [x] x) :anon-fn-fixed))

;; Call an anonymous fn with multiple fixed arities

(clojure.core/prn ((fn* ([] :none) ([x] x)) :anon-fn-mult))

;; Provide a fn var as an argument to be used as a function

;; (apply clojure.core/prn :apply!!!)
