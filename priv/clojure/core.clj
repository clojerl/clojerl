(ns clojure.core)

(def
  seq (fn* [items] (clj_core/seq items)))

(def
  ^{:arglists '([& items])
    :doc "Creates a new list containing the items."
    :added "1.0"}
  list (fn* [& items] (clojerl.List/new (seq items))))

(def
  ^{:arglists '([x seq])
    :doc "Returns a new seq where x is the first element and seq is
    the rest."
    :added "1.0"
    :static true}
  cons (fn* [x s] (clj_core/cons x s)))

(def str
  (fn* [x] (clj_core/str x)))

(def prn
  (fn* [x]
       (io/format "~s~n" (seq [(str x)]))))

(def =
  (fn* [a b] (erlang/== a b)))

(def not
  (fn* [a] (erlang/not a)))

(def <-
  (fn* [a b] (erlang/=:= a b)))

(def assert ^:macro
  (fn* [v] (if (not v) (throw :assert))))

(def apply
  (fn*
   ([f args] (clj_core/invoke f (seq args)))
   ([f x args] (clj_core/invoke f (cons x (seq args))))
   ([f x y args] (clj_core/invoke f (cons x (cons y (seq args)))))
   ([f x y z args] (clj_core/invoke f (cons x (cons y (cons z (seq args))))))
   ([f a b c d args] (clj_core/invoke f (cons a (cons b (cons c (cons d (seq args)))))))))

(def first
  (fn* [xs] (clj_core/first xs)))

(def rest
  (fn* [xs] (clj_core/rest xs)))

(def next
  (fn* [xs] (clj_core/next xs)))

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
  (fn* [x meta] (clj_core/with_meta x meta)))

(def meta
  (fn* [x] (clj_core/meta x)))

(def ^:macro defn
  (fn* [_form _env name args & body]
       `(def ~name (fn* ~args ~@body))))
