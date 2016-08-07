(ns examples.deftype)

(deftype SomeType [value1 value2]
  clojerl.IHash
  (hash [_] 42)
  clojerl.Stringable
  (str [_]
    (str "#SomeType[value1 = " value1 ", value2 = " value2 "]")))

(let [x (new SomeType 1 2)]
  (prn x)
  (prn (clojerl.IHash/hash.e x)))

(defrecord SomeRecord [x y]
  clojerl.IHash
  (hash [this] (+ x y)))

(let [x (new SomeRecord 1 2)]
  (prn x)
  (prn (clojerl.IHash/hash.e x))
  (prn (:x x))
  (prn (assoc x :x 42 :z *out*))
  (prn (merge x {:y 1984 :z :keyword}))
  (prn (merge {:y 1984 :z :keyword} x))
  (prn (= {:x 1 :y 2} x)))
