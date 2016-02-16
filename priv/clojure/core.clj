(ns clojure.core)

(def
  seq (fn* [items] (clj_core/seq.e items)))

(def
  ^{:arglists '([& items])
    :doc "Creates a new list containing the items."
    :added "1.0"}
  list (fn* [& items] (clojerl.List/new.e (seq items))))

(def
  ^{:arglists '([x seq])
    :doc "Returns a new seq where x is the first element and seq is
    the rest."
    :added "1.0"
    :static true}
  cons (fn* [x s] (clj_core/cons.e x s)))

(def str
  (fn* [x] (clj_core/str.e x)))

(def prn
  (fn* [x]
       (io/format.e "~s~n" (seq [(str x)]))))

(def =
  (fn* [a b] (erlang/==.2 a b)))

(def not
  (fn* [a] (erlang/not.1 a)))

(def <
  (fn* [a b] (erlang/<.e a b)))

(def assert ^:macro
  (fn* [v] (if (not v) (throw :assert))))

(def apply
  (fn*
   ([f args] (clj_core/invoke.e f (seq args)))
   ([f x args] (clj_core/invoke.e f (cons x (seq args))))
   ([f x y args] (clj_core/invoke.e f (cons x (cons y (seq args)))))
   ([f x y z args] (clj_core/invoke.e f (cons x (cons y (cons z (seq args))))))
   ([f a b c d args] (clj_core/invoke.e f (cons a (cons b (cons c (cons d (seq args)))))))))

(def first
  (fn* [xs] (clj_core/first.e xs)))

(def rest
  (fn* [xs] (clj_core/rest.e xs)))

(def next
  (fn* [xs] (clj_core/next.e xs)))

(def reverse
  (fn* [s] (lists/reverse.e (seq s))))

(def concat
  (fn*
   ([] (list))
   ([x] (apply list x))
   ([x y]
    (if (seq x)
      (cons (first (seq x)) (concat (rest (seq x)) y))
      y))
   ([x y & zs]
    (if (seq zs)
      (apply concat (concat x y) (first zs) (next zs))
      (concat x y)))))

(def with-meta
  (fn* [x meta] (clj_core/with_meta.e x meta)))

(def meta
  (fn* [x] (clj_core/meta.e x)))

(def vector
  (fn* [& xs] (clj_core/vector.e (seq xs))))

(def hash-map
  (fn* [& xs] (clj_core/hash_map.e (seq xs))))

(def hash-set
  (fn* [& xs] (clj_core/hash_set.e (seq xs))))

(def ^:macro defn
  (fn* [_form _env name args & body]
       `(def ~name (fn* ~args ~@body))))

(defn reduce
  ([f coll]
   (reduce f (first coll) (rest coll)))
  ([f val coll]
   (if (seq coll)
     (reduce f (f val (first coll)) (rest coll))
     val)))

(defn +
  ([] 0)
  ([x] x)
  ([x y] (erlang/+.e x y))
  ([x y & more]
   (reduce + (+ x y) more)))

(defn -
  ([] 0)
  ([x] x)
  ([x y] (erlang/-.e x y))
  ([x y & more]
   (reduce - (- x y) more)))

(defn comp [& fs]
  (let* [fs (reverse fs)]
    (fn* [& xs]
      (reduce #(%2 %1)
              (apply (first fs) xs)
              (rest fs)))))
