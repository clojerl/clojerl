(ns examples.fn)

(def one 1)

(def fixed-arity (fn* [x y] (clj_core/str [x y])))

(def multiple-fixed-arities
  (fn*
   ([x] (clj_core/str [:multiple-fixed-arities 1 x]))
   ([x y] (clj_core/str [:multiple-fixed-arities 2 x y]))
   ([x y z] (clj_core/str [:multiple-fixed-arities 3 x y z]))))

(def variadic-arity
  (fn* [& xs] (clj_core/str [:variadic-arity xs])))

(def variadic-arity-2
  (fn* [x & xs] (clj_core/str [:variadic-arity-2 x xs])))

(def multiple-variadic
  (fn*
   ([x] (clj_core/str [:multiple-variadic-1 x]))
   ([x y] (clj_core/str [:multiple-variadic-2 x y]))
   ([x y z] (clj_core/str [:multiple-variadic-3 x y z]))
   ([x y z & w] (clj_core/str [:multiple-variadic-n x y z w]))))

(def apply-f
  (fn*
   ([f x] (f x))))

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

(clojure.core/prn (variadic-arity-2 1))
(clojure.core/prn (variadic-arity-2 1 2 3))

;; Call a  variadic fn that also has fixed arities

(clojure.core/prn (multiple-variadic 1))
(clojure.core/prn (multiple-variadic 1 2))
(clojure.core/prn (multiple-variadic 1 2 3))
(clojure.core/prn (multiple-variadic 1 2 3 4))
(clojure.core/prn (multiple-variadic 1 2 3 4 5 6 7 8 9 :a :b :c))

;; Call an anonymous fn with a single fixed arity

(clojure.core/prn ((fn* [x] x) :anon-fn-fixed))

;; Call an anonymous fn with multiple fixed arities

(clojure.core/prn ((fn* ([] :anon-fn-mult-0)
                        ([_x] :anon-fn-mult-1))))
(clojure.core/prn ((fn* ([] :anon-fn-mult-0)
                        ([_x] :anon-fn-mult-1))
                   :arg))

;; Call an anonymous fn with a single variadic arity

;; (clojure.core/prn ((fn* ([& xs] xs) 1 2 3)))

;; Provide an erlang function as an argument to be used as a function

(apply-f io/format.1 "io:format/1 FTW!!!~n")
(clojure.core/apply io/format.2
                    "io:format/2 FTW!!!: ~s~n"
                    [(clojure.core/seq ["lala"])])
;; (apply-f io/format.2 "io:format/1 FTW!!!~n") ;; This should fail

;; Provide a fn var as an argument to be used as a function

(apply-f clojure.core/prn :apply!!!)

;; Provide a fn variadic var as an argument to be used as a function

(clojure.core/prn (apply-f variadic-arity :apply-f-variadic))

(clojure.core/prn (clojure.core/apply variadic-arity []))

(clojure.core/prn
 (clojure.core/apply variadic-arity
                     [:apply-variadic 1 2 3]))

(clojure.core/prn
 (clojure.core/apply variadic-arity-2
                     [:apply-variadic-2 1 2 3]))

(clojure.core/prn
 (clojure.core/apply variadic-arity-2
                     [:apply-variadic-2]))

(clojure.core/prn
 (clojure.core/apply multiple-variadic
                     [:apply-multi-variadic 1]))
(clojure.core/prn
 (clojure.core/apply multiple-variadic
                     [:apply-multi-variadic 1 2]))
(clojure.core/prn
 (clojure.core/apply multiple-variadic
                     [:apply-multi-variadic 1 2 3]))
(clojure.core/prn
 (clojure.core/apply multiple-variadic
                     [:apply-multi-variadic 1 2 3 4 5 6]))

;; Keywords as a function for maps

(clojure.core/prn (:a {:a 1}))
(clojure.core/prn (:a {:b 2} :not-found-a))
(clojure.core/prn (clojure.core/apply :b [{:b 2}]))
(clojure.core/prn (clojure.core/apply :b {:c 3} [:not-found-b]))
;; (clojure.core/prn (:a)) ;; This should fail
