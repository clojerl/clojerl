;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.core)

(def
  ^{:arglists '([& items])
    :doc "Creates a new list containing the items."
    :added "1.0"}
  list (fn* [& items] (new clojerl.List items)))

(def
  ^{:arglists '([x seq])
    :doc "Returns a new seq where x is the first element and seq is
    the rest."
    :added "1.0"
    :static true}
  cons (fn* [x s] (clj_core/cons.e x s)))

(def
  ^{:macro true
    :added "1.0"}
  let (fn* let [_&form _&env & decl] (cons 'let* decl)))

(def
  ^{:macro true
    :added "1.0"}
  loop (fn* loop [_&form _&env & decl] (cons 'loop* decl)))

(def
  ^{:macro true
    :added "1.0"}
  fn (fn* fn [&form _&env & decl]
         (let [x (cons 'fn* (new clojerl.List decl))]
           (clj_core/with_meta.e x (clj_core/meta.e &form)))))

(def
  ^{:arglists '([coll])
    :doc "Returns the first item in the collection. Calls seq on its
    argument. If coll is nil, returns nil."
    :added "1.0"
    :static true}
  first (fn ^:static first [coll] (clj_core/first.e coll)))

(def
  ^{:arglists '([coll])
    :tag clojerl.ISeq
    :doc "Returns a seq of the items after the first. Calls seq on its
  argument.  If there are no more items, returns nil."
    :added "1.0"
    :static true}
  next (fn ^:static next [x] (clj_core/next.e x)))

(def
  ^{:arglists '([coll])
    :tag clojerl.ISeq
    :doc "Returns a possibly empty seq of the items after the first. Calls seq on its
  argument."
    :added "1.0"
    :static true}
  rest (fn ^:static rest [x] (clj_core/rest.e x)))

(def
  ^{:arglists '([coll x] [coll x & xs])
    :doc "conj[oin]. Returns a new collection with the xs
    'added'. (conj nil item) returns (item).  The 'addition' may
    happen at different 'places' depending on the concrete type."
    :added "1.0"
    :static true}
  conj (fn ^:static conj
         ([] [])
         ([coll] coll)
         ([coll x] (clj_core/conj.e coll x))
         ([coll x & xs]
          (if xs
            (recur (conj coll x) (first xs) (next xs))
            (conj coll x)))))

(def
  ^{:doc "Same as (first (next x))"
    :arglists '([x])
    :added "1.0"
    :static true}
  second (fn ^:static second [x] (first (next x))))

(def
  ^{:doc "Same as (first (first x))"
    :arglists '([x])
    :added "1.0"
    :static true}
  ffirst (fn ^:static ffirst [x] (first (first x))))

(def
  ^{:doc "Same as (next (first x))"
    :arglists '([x])
    :added "1.0"
    :static true}
  nfirst (fn ^:static nfirst [x] (next (first x))))

(def
  ^{:doc "Same as (first (next x))"
    :arglists '([x])
    :added "1.0"
    :static true}
  fnext (fn ^:static fnext [x] (first (next x))))

(def
  ^{:doc "Same as (next (next x))"
    :arglists '([x])
    :added "1.0"
    :static true}
  nnext (fn ^:static nnext [x] (next (next x))))

(def
  ^{:arglists '([coll])
    :doc "Returns a seq on the collection. If the collection is
    empty, returns nil.  (seq nil) returns nil. seq also works on
    Strings, native Java arrays (of reference types) and any objects
    that implement Iterable. Note that seqs cache values, thus seq
    should not be used on any Iterable whose iterator repeatedly
    returns the same mutable object."
    :tag clojerl.ISeq
    :added "1.0"
    :static true}
  seq (fn ^:static seq [coll] (clj_core/seq.e coll)))

(def
  ^{:arglists '([protocol x])
    :doc "Returns true if x satisfies the protocol"
    :added "1.2"}
  satisfies? (fn satisfies? [protocol x]
               (clj_core/satisfies?.e protocol (clj_core/type.e x))))

(def
  ^{:arglists '([atype x])
    :doc "Returns true if x's type is atype"
    :added "1.0"}
  instance? (fn instance? [atype x] (erlang/==.e atype (clj_core/type.e x))))

(def
  ^{:arglists '([x])
    :doc "Return true if x implements ISeq"
    :added "1.0"
    :static true}
  seq? (fn ^:static seq? [x] (satisfies? clojerl.ISeq x)))

(def
  ^{:arglists '([x])
    :doc "Return true if x implements ISeq"
    :added "1.0"
    :static true}
  lazy-seq? (fn ^:static seq? [x] (instance? clojerl.LazySeq x)))

(def
  ^{:arglists '([x])
    :doc "Return true if x implements IMeta"
    :added "1.0"
    :static true}
  meta? (fn ^:static meta? [x] (satisfies? clojerl.IMeta x)))

(def
  ^{:arglists '([x])
    :doc "Return true if x is a String"
    :added "1.0"
    :static true}
  string? (fn ^:static string? [x] (erlang/is_binary.e x)))

(def
  ^{:arglists '([x])
    :doc "Return true if x is a Character"
    :added "1.0"
    :static true}
  char? (fn ^:static char? [x]
          (erlang/and.e (erlang/is_binary.e x)
                        (erlang/=:=.e 1 (erlang/size.e x)))))

(def
  ^{:arglists '([x])
    :doc "Return true if x implements IMap"
    :added "1.0"
    :static true}
  map? (fn ^:static map? [x] (satisfies? clojerl.IMap x)))

(def
  ^{:arglists '([x])
    :doc "Return true if x is a Vector"
    :added "1.0"
    :static true}
  vector? (fn ^:static vector? [x] (instance? clojerl.Vector x)))

(def
  ^{:arglists '([x])
    :doc "Return true if x is a Symbol"
    :added "1.0"
    :static true}
  symbol? (fn ^:static symbol? [x] (instance? clojerl.Symbol x)))

(def
  ^{:arglists '([x])
    :doc "Returns true if x is an Erlang tuple"
    :added "1.0"
    :static true}
  tuple? (fn [x] (erlang/is_tuple.e x)))

(def
  ^{:arglists '([map key val] [map key val & kvs])
    :doc "assoc[iate]. When applied to a map, returns a new map of the
    same (hashed/sorted) type, that contains the mapping of key(s) to
    val(s). When applied to a vector, returns a new vector that
    contains val at index. Note - index must be <= (count vector)."
    :added "1.0"
    :static true}
  assoc
  (fn ^:static assoc
    ([map key val] (clj_core/assoc.e map key val))
    ([map key val & kvs]
     (let [ret (assoc map key val)]
       (if kvs
         (if (next kvs)
           (recur ret (first kvs) (second kvs) (nnext kvs))
           (throw "assoc expects even number of arguments after map/vector, found odd number"))
         ret)))))

;;;;;;;;;;;;;;;;; metadata ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def
  ^{:arglists '([obj])
    :doc "Returns the metadata of obj, returns nil if there is no metadata."
    :added "1.0"
    :static true}
  meta (fn ^:static meta [x]
         (if (meta? x)
           (clj_core/meta.e x))))

(def
  ^{:arglists '([^clojure.lang.IObj obj m])
    :doc "Returns an object of the same type and value as obj, with
    map m as its metadata."
    :added "1.0"
    :static true}
  with-meta (fn ^:static with-meta [x m]
              (clj_core/with_meta.e x m)))

(def ^{:private true :dynamic true}
  assert-valid-fdecl (fn [fdecl]))

(def
  ^{:private true}
  sigs
  (fn [fdecl]
    (assert-valid-fdecl fdecl)
    (let [asig
          (fn [fdecl]
            (let [arglist (first fdecl)
                  ;elide implicit macro args
                  arglist (if (clj_core/equiv.e '&form (first arglist))
                            (clj_core/subvec.e arglist 2 (clj_core/count.e arglist))
                            arglist)
                  body (next fdecl)]
              (if (map? (first body))
                (if (next body)
                  (with-meta arglist (conj (if (meta arglist) (meta arglist) {}) (first body)))
                  arglist)
                arglist)))]
      (if (seq? (first fdecl))
        (loop [ret [] fdecls fdecl]
          (if fdecls
            (recur (conj ret (asig (first fdecls))) (next fdecls))
            (seq ret)))
        (list (asig fdecl))))))

(def
  ^{:arglists '([coll])
    :doc "Return the last item in coll, in linear time"
    :added "1.0"
    :static true}
  last (fn ^:static last [s]
         (if (next s)
           (recur (next s))
           (first s))))

(def
  ^{:arglists '([coll])
    :doc "Return a seq of all but the last item in coll, in linear time"
    :added "1.0"
    :static true}
  butlast (fn ^:static butlast [s]
            (loop [ret [] s s]
              (if (next s)
                (recur (conj ret (first s)) (next s))
                (seq ret)))))

(def
  ^{:doc "Same as (def name (fn [params* ] exprs*)) or (def
    name (fn ([params* ] exprs*)+)) with any doc-string or attrs added
    to the var metadata. prepost-map defines a map with optional keys
    :pre and :post that contain collections of pre or post conditions."
    :arglists '([name doc-string? attr-map? [params*] prepost-map? body]
                [name doc-string? attr-map? ([params*] prepost-map? body)+ attr-map?])
    :added "1.0"
    :macro true}
  defn
  (fn* [&form &env name & fdecl]
       (if (symbol? name)
         nil
         (throw "First argument to defn must be a symbol"))
       (let [m     (if (string? (first fdecl))
                     {:doc (first fdecl)}
                     {})
             fdecl (if (string? (first fdecl))
                     (next fdecl)
                     fdecl)
             m     (if (map? (first fdecl))
                     (conj m (first fdecl))
                     m)
             fdecl (if (map? (first fdecl))
                     (next fdecl)
                     fdecl)
             fdecl (if (vector? (first fdecl))
                     (list fdecl)
                     fdecl)
             m     (if (map? (last fdecl))
                     (conj m (last fdecl))
                     m)
             fdecl (if (map? (last fdecl))
                     (butlast fdecl)
                     fdecl)
             m     (conj {:arglists (list 'quote (sigs fdecl))} m)
             m     (conj (if (meta name) (meta name) {}) m)]
         (list 'def (with-meta name m)
               (cons 'clojure.core/fn ;; can't use syntax-quote here yet because
                     ;; we haven't defined all the necessary functions
                     (new clojerl.List (seq fdecl)))))))

(defn vector
  "Creates a new vector containing the args."
  {:added "1.0"
   :static true}
  ([] [])
  ([a] [a])
  ([a b] [a b])
  ([a b c] [a b c])
  ([a b c d] [a b c d])
  ([a b c d & args]
   (clj_core/vector.e (cons a (cons b (cons c (cons d args)))))))

(defn vec
  "Creates a new vector containing the contents of coll. Java arrays
  will be aliased and should not be modified."
  {:added "1.0"
   :static true}
  ([coll]
   (if (vector? coll)
     (if (satisfies? clojerl.IMeta coll)
       (with-meta coll nil)
       (clj_core/vector.e coll))
     (clj_core/vector.e coll))))

(defn hash-map
  "keyval => key val
  Returns a new hash map with supplied mappings.  If any keys are
  equal, they are handled as if by repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([] {})
  ([& keyvals]
   (new clojerl.Map keyvals)))

(defn hash-set
  "Returns a new hash set with supplied keys.  Any equal keys are
  handled as if by repeated uses of conj."
  {:added "1.0"
   :static true}
  ([] #{})
  ([& keys]
   (new clojerl.Set keys)))

(defn sorted-map
  "keyval => key val
  Returns a new sorted map with supplied mappings.  If any keys are
  equal, they are handled as if by repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([& keyvals]
   (new clojerl.SortedMap keyvals)))

(defn sorted-map-by
  "keyval => key val
  Returns a new sorted map with supplied mappings, using the supplied
  comparator.  If any keys are equal, they are handled as if by
  repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([comparator & keyvals]
   (throw "unsupported sorted-map")
   #_(clojure.lang.PersistentTreeMap/create comparator keyvals)))

(defn sorted-set
  "Returns a new sorted set with supplied keys.  Any equal keys are
  handled as if by repeated uses of conj."
  {:added "1.0"
   :static true}
  ([& keys]
   (new clojerl.SortedSet keys)))

(defn sorted-set-by
  "Returns a new sorted set with supplied keys, using the supplied
  comparator.  Any equal keys are handled as if by repeated uses of
  conj."
  {:added "1.1"
   :static true}
  ([comparator & keys]
   (throw "unsupported sorted-set")
   #_(clojure.lang.PersistentTreeSet/create comparator keys)))

;;;;;;;;;;;;;;;;;;;;

(defn nil?
  "Returns true if x is nil, false otherwise."
  {:tag clojerl.Boolean
   :added "1.0"
   :static true
   :inline (fn [x] (list 'erlang/=:=.e x nil))}
  [x] (erlang/=:=.e x nil))

(def
  ^{:macro true
    :doc "Like defn, but the resulting function name is declared as a
  macro and will be used as a macro by the compiler when it is
  called."
    :arglists '([name doc-string? attr-map? [params*] body]
                [name doc-string? attr-map? ([params*] body)+ attr-map?])
    :added "1.0"}
  defmacro (fn [&form &env name & args]
             (let [name   (with-meta name (assoc (meta name) :macro true))
                   prefix (loop [p (list name) args args]
                            (let [f (first args)]
                              (if (string? f)
                                (recur (cons f p) (next args))
                                (if (map? f)
                                  (recur (cons f p) (next args))
                                  p))))
                   fdecl (loop [fd args]
                           (if (string? (first fd))
                             (recur (next fd))
                             (if (map? (first fd))
                               (recur (next fd))
                               fd)))
                   fdecl (if (vector? (first fdecl))
                           (list fdecl)
                           fdecl)
                   add-implicit-args (fn [fd]
                                       (let [args (first fd)]
                                         (cons (vec (cons '&form (cons '&env args)))
                                               (next fd))))
                   add-args (fn [acc ds]
                              (if (nil? ds)
                                acc
                                (let [d (first ds)]
                                  (if (map? d)
                                    (conj acc d)
                                    (recur (conj acc (add-implicit-args d))
                                           (next ds))))))
                   fdecl (seq (add-args [] fdecl))
                   decl (loop [p prefix d fdecl]
                          (if p
                            (recur (next p) (cons (first p) d))
                            d))]
               (cons 'clojure.core/defn decl))))

(defmacro when
  "Evaluates test. If logical true, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test (cons 'do body)))

(defmacro when-not
  "Evaluates test. If logical false, evaluates body in an implicit do."
  {:added "1.0"}
  [test & body]
  (list 'if test nil (cons 'do body)))

(defn false?
  "Returns true if x is the value false, false otherwise."
  {:tag clojerl.Boolean
   :added "1.0"
   :static true}
  [x] (erlang/=:=.e x false))

(defn true?
  "Returns true if x is the value true, false otherwise."
  {:tag clojerl.Boolean
   :added "1.0"
   :static true}
  [x] (erlang/=:=.e x true))

(defn not
  "Returns true if x is logical false, false otherwise."
  {:tag clojerl.Boolean
   :added "1.0"
   :static true}
  [x] (if x false true))

(defn some?
  "Returns true if x is not nil, false otherwise."
  {:tag clojerl.Boolean
   :added "1.6"
   :static true}
  [x] (not (nil? x)))

(defn str
  "With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args."
  {:tag clojerl.String
   :added "1.0"
   :static true}
  (^clojerl.String [] "")
  (^clojerl.String [^Object x]
                    (if (nil? x) "" (clj_core/str.e x)))
  (^clojerl.String [x & ys]
                    ((fn [acc more]
                       (if more
                         (recur (clojerl.String/append.e acc (str (first more)))
                                (next more))
                         acc))
                     (clj_core/str.e x) ys)))

(defn keyword?
  "Return true if x is a Keyword"
  {:added "1.0"
   :static true}
  [x] (instance? clojerl.Keyword x))

(defn symbol
  "Returns a Symbol with the given namespace and name."
  {:tag clojerl.Symbol
   :added "1.0"
   :static true}
  ([name] (if (symbol? name) name (clj_core/symbol.e name)))
  ([ns name] (clj_core/symbol.e ns name)))

(defn gensym
  "Returns a new symbol with a unique name. If a prefix string is
  supplied, the name is prefix# where # is some unique number. If
  prefix is not supplied, the prefix is 'G__'."
  {:added "1.0"
   :static true}
  ([] (gensym "G__"))
  ([prefix-string] (clj_core/gensym.e prefix-string)))

(defmacro cond
  "Takes a set of test/expr pairs. It evaluates each test one at a
  time.  If a test returns logical true, cond evaluates and returns
  the value of the corresponding expr and doesn't evaluate any of the
  other tests or exprs. (cond) returns nil."
  {:added "1.0"}
  [& clauses]
  (when clauses
    (list 'if (first clauses)
          (if (next clauses)
            (second clauses)
            (throw "cond requires an even number of forms"))
          (cons 'clojure.core/cond (nnext clauses)))))

(defn keyword
  "Returns a Keyword with the given namespace and name.  Do not use :
  in the keyword strings, it will be added automatically."
  {:tag clojerl.Keyword
   :added "1.0"
   :static true}
  ([name] (cond (keyword? name) name
                (symbol? name) (new clojerl.Keyword (clj_core/namespace.e name)
                                                      (clj_core/name.e name))
                (string? name) (new clojerl.Keyword name)))
  ([ns name] (new clojerl.Keyword ns name)))

(defn find-keyword
  "Returns a Keyword with the given namespace and name if one already
  exists.  This function will not intern a new keyword. If the keyword
  has not already been interned, it will return nil.  Do not use :
  in the keyword strings, it will be added automatically."
  {:tag clojerl.Keyword
   :added "1.3"
   :static true}
  ([name] (cond (keyword? name) name
                (symbol? name) (clojerl.Keyword/find.e (clj_core/namespace.e name)
                                                       (clj_core/name.e name))
                (string? name) (clojerl.Keyword/find.e name)))
  ([ns name] (clojerl.Keyword/find.e ns name)))

(defn spread
  {:private true
   :static true}
  [arglist]
  (cond
    (nil? arglist) nil
    (nil? (next arglist)) (seq (first arglist))
    :else (cons (first arglist) (spread (next arglist)))))

(defn list*
  "Creates a new list containing the items prepended to the rest, the
  last of which will be treated as a sequence."
  {:added "1.0"
   :static true}
  ([args] (seq args))
  ([a args] (cons a args))
  ([a b args] (cons a (cons b args)))
  ([a b c args] (cons a (cons b (cons c args))))
  ([a b c d & more]
   (cons a (cons b (cons c (cons d (spread more)))))))

(defn apply
  "Applies fn f to the argument list formed by prepending intervening arguments to args."
  {:added "1.0"
   :static true}
  ([f args]
   (clojerl.IFn/apply.e f (seq args)))
  ([f x args]
   (clojerl.IFn/apply.e f (list* x args)))
  ([f x y args]
   (clojerl.IFn/apply.e f (list* x y args)))
  ([f x y z args]
   (clojerl.IFn/apply.e f (list* x y z args)))
  ([f a b c d & args]
   (clojerl.IFn/apply.e f (cons a (cons b (cons c (cons d (spread args))))))))

(defn vary-meta
  "Returns an object of the same type and value as obj, with
  (apply f (meta obj) args) as its metadata."
  {:added "1.0"
   :static true}
  [obj f & args]
  (with-meta obj (apply f (meta obj) args)))

(defmacro lazy-seq
  "Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls. See also - realized?"
  {:added "1.0"}
  [& body]
  (list 'new 'clojerl.LazySeq (list* '^{:once true} fn* [] body)))

#_((defn ^:static ^clojure.lang.ChunkBuffer chunk-buffer ^clojure.lang.ChunkBuffer [capacity]
     (throw "unimplemented chunked seq")
     #_(clojure.lang.ChunkBuffer. capacity))

   (defn ^:static chunk-append [^clojure.lang.ChunkBuffer b x]
     (throw "unimplemented chunked seq")
     #_(.add b x))

   (defn ^:static ^clojure.lang.IChunk chunk [^clojure.lang.ChunkBuffer b]
     (throw "unimplemented chunked seq")
     #_(.chunk b))

   (defn ^:static  ^clojure.lang.IChunk chunk-first ^clojure.lang.IChunk [^clojure.lang.IChunkedSeq s]
     (throw "unimplemented chunked seq")
     #_(.chunkedFirst s))

   #(defn ^:static ^clojerl.ISeq chunk-rest ^clojerl.ISeq [^clojure.lang.IChunkedSeq s]
      (throw "unimplemented chunked seq")
      #_(.chunkedMore s))

   (defn ^:static ^clojerl.ISeq chunk-next ^clojerl.ISeq [^clojure.lang.IChunkedSeq s]
     (throw "unimplemented chunked seq")
     #_(.chunkedNext s))

   (defn ^:static chunk-cons [chunk rest]
     (throw "unimplemented chunked seq")
     #_(if (clojure.lang.Numbers/isZero (clojure.lang.RT/count chunk))
         rest
         (clojure.lang.ChunkedCons. chunk rest)))

   (defn ^:static chunked-seq? [s]
     (throw "unimplemented chunked seq")
     #_(instance? clojure.lang.IChunkedSeq s)))

(defn concat
    "Returns a lazy seq representing the concatenation of the elements in the supplied colls."
    {:added "1.0"
     :static true}
    ([] (lazy-seq nil))
    ([x] (lazy-seq x))
    ([x y]
     (lazy-seq
      (let [s (seq x)]
        (if s
          (cons (first s) (concat (rest s) y))
          y))))
    ([x y & zs]
     (let [cat (fn cat [xys zs]
                 (lazy-seq
                  (let [xys (seq xys)]
                    (if xys
                      (cons (first xys) (cat (rest xys) zs))
                      (when zs
                        (cat (first zs) (next zs)))))))]
       (cat (concat x y) zs))))

;;;;;;;;;;;;;;;;at this point all the support for syntax-quote exists;;;;;;;;;;;;;;;;;;;;;;
(defmacro delay
  "Takes a body of expressions and yields a Delay object that will
  invoke the body only the first time it is forced (with force or deref/@), and
  will cache the result and return it on all subsequent force
  calls. See also - realized?"
  {:added "1.0"}
  [& body]
  (throw "unimplemented delay")
  #_(list 'new 'clojerl.Delay (list* `^{:once true} fn* [] body)))

(defn delay?
  "returns true if x is a Delay created with delay"
  {:added "1.0"
   :static true}
  [x]
  (throw "unimplemented delay")
  #_(instance? clojerl.Delay x))

(defn force
  "If x is a Delay, returns the (possibly cached) value of its expression, else returns x"
  {:added "1.0"
   :static true}
  [x]
  (throw "unimplemented delay")
  #_(clojerl.Delay/force.e x))

(defmacro if-not
  "Evaluates test. If logical false, evaluates and returns then expr,
  otherwise else expr, if supplied, else nil."
  {:added "1.0"}
  ([test then] `(if-not ~test ~then nil))
  ([test then else]
   `(if (not ~test) ~then ~else)))

(defn identical?
  "Tests if 2 arguments are the same object"
  {:inline (fn [x y] `(erlang/=:=.e ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x y] (erlang/=:=.e x y)))

;equiv-based
(defn =
  "Equality. Returns true if x equals y, false if not. Same as
  Java x.equals(y) except it also works for nil, and compares
  numbers and collections in a type-independent manner.  Clojure's immutable data
  structures define equals() (and thus =) as a value, not an identity,
  comparison."
  {:inline (fn [x y] `(clj_core/equiv.e ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (clj_core/equiv.e x y))
  ([x y & more]
   (if (clj_core/equiv.e x y)
     (if (next more)
       (recur y (first more) (next more))
       (clj_core/equiv.e y (first more)))
     false)))

;equals-based
#_(defn =
  "Equality. Returns true if x equals y, false if not. Same as Java
  x.equals(y) except it also works for nil. Boxed numbers must have
  same type. Clojure's immutable data structures define equals() (and
  thus =) as a value, not an identity, comparison."
  {:inline (fn [x y] `(. clojure.lang.Util equals ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (clojure.lang.Util/equals x y))
  ([x y & more]
   (if (= x y)
     (if (next more)
       (recur y (first more) (next more))
       (= y (first more)))
     false)))

(defn not=
  "Same as (not (= obj1 obj2))"
  {:tag clojerl.Boolean
   :added "1.0"
   :static true}
  ([x] false)
  ([x y] (not (= x y)))
  ([x y & more]
   (not (apply = x y more))))

(defn compare
  "Comparator. Returns a negative number, zero, or a positive number
  when x is logically 'less than', 'equal to', or 'greater than'
  y. Same as Java x.compareTo(y) except it also works for nil, and
  compares numbers and collections in a type-independent manner. x
  must implement Comparable"
  {
   :inline (fn [x y] `(. clojure.lang.Util compare ~x ~y))
   :added "1.0"}
  [x y]
  (clj_utils/compare.e x y))

(defmacro and
  "Evaluates exprs one at a time, from left to right. If a form
  returns logical false (nil or false), and returns that value and
  doesn't evaluate any of the other expressions, otherwise it returns
  the value of the last expr. (and) returns true."
  {:added "1.0"}
  ([] true)
  ([x] x)
  ([x & next]
   `(let [and# ~x]
      (if and# (and ~@next) and#))))

(defmacro or
  "Evaluates exprs one at a time, from left to right. If a form
  returns a logical true value, or returns that value and doesn't
  evaluate any of the other expressions, otherwise it returns the
  value of the last expression. (or) returns nil."
  {:added "1.0"}
  ([] nil)
  ([x] x)
  ([x & next]
      `(let [or# ~x]
         (if or# or# (or ~@next)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;
(defn zero?
  "Returns true if num is zero, else false"
  {:inline (fn [x] `(erlang/==.e ~x 0))
   :added "1.0"}
  [x] (erlang/==.e x 0))

(defn count
  "Returns the number of items in the collection. (count nil) returns
  0.  Also works on strings, arrays, and Java Collections and Maps"
  {:inline (fn  [x] `(clj_core/count.e ~x))
   :added "1.0"}
  [coll] (clj_core/count.e coll))

(defn int
  "Coerce to int"
  {:inline (fn  [x] `(erlang/trunc.e ~x))
   :added "1.0"}
  [x] (erlang/trunc.e x))

(defn nth
  "Returns the value at the index. get returns nil if index out of
  bounds, nth throws an exception unless not-found is supplied.  nth
  also works for strings, Java arrays, regex Matchers and Lists, and,
  in O(n) time, for sequences."
  {:inline (fn  [c i & nf] `(clj_core/nth.e ~c ~i ~@nf))
   :inline-arities #{2 3}
   :added "1.0"}
  ([coll index] (clj_core/nth.e coll index))
  ([coll index not-found] (clj_core/nth.e coll index not-found)))

(defn <
  "Returns non-nil if nums are in monotonically increasing order,
  otherwise false."
  {:inline (fn [x y] `(erlang/<.e ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (erlang/<.e x y))
  ([x y & more]
   (if (< x y)
     (if (next more)
       (recur y (first more) (next more))
       (< y (first more)))
     false)))

(defn inc'
  "Returns a number one greater than num. Supports arbitrary precision.
  See also: inc"
  {:inline (fn [x] `(erlang/+.e ~x 1))
   :added "1.0"}
  [x] (erlang/+.e x 1))

(defn inc
  "Returns a number one greater than num. Does not auto-promote
  longs, will throw on overflow. See also: inc'"
  {:inline (fn [x] `(inc' ~x))
   :added "1.2"}
  [x] (inc' x))

;; reduce is defined again later after InternalReduce loads
(defn ^:private ^:static
  reduce1
  ([f coll]
   (let [s (seq coll)]
     (if s
       (reduce1 f (first s) (next s))
       (f))))
  ([f val coll]
   (let [s (seq coll)]
     (if s
       (recur f (f val (first s)) (next s))
       val))))

(defn reverse
  "Returns a seq of the items in coll in reverse order. Not lazy."
  {:added "1.0"
   :static true}
  [coll]
  (reduce1 conj '() coll))

;;math stuff
(defn ^:private nary-inline
  ([op]
   (fn
     ([x] `(~op ~x))
     ([x y] `(~op ~x ~y))
     ([x y & more]
      (reduce1
         (fn [a b] `(~op ~a ~b))
         `(~op ~x ~y)) more))))

(defn ^:private >1? [n] (erlang/>.e n 1))
(defn ^:private >0? [n] (erlang/>.e n 0))

(defn +'
  "Returns the sum of nums. (+) returns 0. Supports arbitrary precision.
  See also: +"
  {:inline (nary-inline 'erlang/+.e)
   :inline-arities >1?
   :added "1.0"}
  ([] 0)
  ([x] x)
  ([x y] (erlang/+.e x y))
  ([x y & more]
   (reduce1 +' (+' x y) more)))

(defn +
  "Returns the sum of nums. (+) returns 0. Does not auto-promote
  longs, will throw on overflow. See also: +'"
  {:inline (nary-inline 'add)
   :inline-arities >1?
   :added "1.2"}
  ([] 0)
  ([x] x)
  ([x y] (erlang/+.e x y))
  ([x y & more]
   (reduce1 erlang/+.2 (+ x y) more)))

(defn *'
  "Returns the product of nums. (*) returns 1. Supports arbitrary precision.
  See also: *"
  {:inline (nary-inline 'erlang/*.e)
   :inline-arities >1?
   :added "1.0"}
  ([] 1)
  ([x] x)
  ([x y] (erlang/*.e x y))
  ([x y & more]
   (reduce1 *' (*' x y) more)))

(defn *
  "Returns the product of nums. (*) returns 1. Does not auto-promote
  longs, will throw on overflow. See also: *'"
  {:inline (nary-inline 'multiply)
   :inline-arities >1?
   :added "1.2"}
  ([] 1)
  ([x] x)
  ([x y] (*' x y))
  ([x y & more]
   (reduce1 * (* x y) more)))

(defn /
  "If no denominators are supplied, returns 1/numerator,
  else returns numerator divided by all of the denominators."
  {:inline (nary-inline 'divide)
   :inline-arities >1?
   :added "1.0"}
  ([x] (/ 1 x))
  ([x y] (erlang//.e x y))
  ([x y & more]
   (reduce1 / (/ x y) more)))

(defn -'
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result. Supports arbitrary precision.
  See also: -"
  {:inline (nary-inline 'erlang/-.e)
   :inline-arities >0?
   :added "1.0"}
  ([x] (erlang/-.e x))
  ([x y] (erlang/-.e x y))
  ([x y & more]
   (reduce1 -' (-' x y) more)))

(defn -
  "If no ys are supplied, returns the negation of x, else subtracts
  the ys from x and returns the result. Does not auto-promote
  longs, will throw on overflow. See also: -'"
  {:inline (nary-inline 'minus)
   :inline-arities >0?
   :added "1.2"}
  ([x] (-' x))
  ([x y] (-' x y))
  ([x y & more]
   (reduce1 - (- x y) more)))

(defn <=
  "Returns non-nil if nums are in monotonically non-decreasing order,
  otherwise false."
  {:inline (fn [x y] `(erlang/=<.e ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (erlang/=<.e x y))
  ([x y & more]
   (if (<= x y)
     (if (next more)
       (recur y (first more) (next more))
       (<= y (first more)))
     false)))

(defn >
  "Returns non-nil if nums are in monotonically decreasing order,
  otherwise false."
  {:inline (fn [x y] `(erlang/>.e ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (erlang/>.e x y))
  ([x y & more]
   (if (> x y)
     (if (next more)
       (recur y (first more) (next more))
       (> y (first more)))
     false)))

(defn >=
  "Returns non-nil if nums are in monotonically non-increasing order,
  otherwise false."
  {:inline (fn [x y] `(erlang/>=.e ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (erlang/>=.e x y))
  ([x y & more]
   (if (>= x y)
     (if (next more)
       (recur y (first more) (next more))
       (>= y (first more)))
     false)))

(defn ==
  "Returns non-nil if nums all have the equivalent
  value (type-independent), otherwise false"
  {:inline (fn [x y] `(erlang/==.e ~x ~y))
   :inline-arities #{2}
   :added "1.0"}
  ([x] true)
  ([x y] (erlang/==.e x y))
  ([x y & more]
   (if (== x y)
     (if (next more)
       (recur y (first more) (next more))
       (== y (first more)))
     false)))

(defn max
  "Returns the greatest of the nums."
  {:added "1.0"
   :inline-arities >1?
   :inline (nary-inline 'max)}
  ([x] x)
  ([x y] (if (< x y) y x))
  ([x y & more]
   (reduce1 max (max x y) more)))

(defn min
  "Returns the least of the nums."
  {:added "1.0"
   :inline-arities >1?
   :inline (nary-inline 'min)}
  ([x] x)
  ([x y] (if (> x y) y x))
  ([x y & more]
   (reduce1 min (min x y) more)))

(defn dec'
  "Returns a number one less than num. Supports arbitrary precision.
  See also: dec"
  {:inline (fn [x] `(erlang/-.e ~x 1))
   :added "1.0"}
  [x] (erlang/-.e x 1))

(defn dec
  "Returns a number one less than num. Does not auto-promote
  longs, will throw on overflow. See also: dec'"
  {:inline (fn [x] `(erlang/-.e ~x 1))
   :added "1.2"}
  [x] (erlang/-.e x 1))

(defn pos?
  "Returns true if num is greater than zero, else false"
  {:inline (fn [x] `(> ~x 0))
   :added "1.0"}
  [x] (> x 0))

(defn neg?
  "Returns true if num is less than zero, else false"
  {:inline (fn [x] `(< ~x 0))
   :added "1.0"}
  [x] (< x 0))

(defn quot
  "quot[ient] of dividing numerator by denominator."
  {:added "1.0"
   :static true
   :inline (fn [x y] `(erlang/trunc.e (/ ~x ~y)))}
  [num div]
  (erlang/trunc.e (/ num div)))

(defn rem
  "remainder of dividing numerator by denominator."
  {:added "1.0"
   :static true
   :inline (fn [x y] `(erlang/rem.e ~x ~y))}
  [num div]
  (erlang/rem.e num div))

;;Bit ops

(defn bit-not
  "Bitwise complement"
  {:inline (fn [x] `(erlang/bnot.e ~x))
   :added "1.0"}
  [x] (erlang/bnot.e x))


(defn bit-and
  "Bitwise and"
   {:inline (nary-inline 'and)
    :inline-arities >1?
    :added "1.0"}
   ([x y] (erlang/band.e x y))
   ([x y & more]
      (reduce1 bit-and (bit-and x y) more)))

(defn bit-or
  "Bitwise or"
  {:inline (nary-inline 'or)
   :inline-arities >1?
   :added "1.0"}
  ([x y] (erlang/bor.e x y))
  ([x y & more]
    (reduce1 bit-or (bit-or x y) more)))

(defn bit-xor
  "Bitwise exclusive or"
  {:inline (nary-inline 'xor)
   :inline-arities >1?
   :added "1.0"}
  ([x y] (erlang/bxor.e x y))
  ([x y & more]
    (reduce1 bit-xor (bit-xor x y) more)))

(defn bit-and-not
  "Bitwise and with complement"
  {:inline (nary-inline 'andNot)
   :inline-arities >1?
   :added "1.0"
   :static true}
  ([x y] (bit-not (bit-and x y)))
  ([x y & more]
    (reduce1 bit-and-not (bit-and-not x y) more)))


(defn bit-clear
  "Clear bit at index n"
  {:added "1.0"
   :static true}
  [x n]
  (erlang/band.e x (erlang/bnot.e (erlang/bsl.e 1 n))))

(defn bit-set
  "Set bit at index n"
  {:added "1.0"
   :static true}
  [x n]
  (erlang/bor.e x (erlang/bsl.e 1 n)))

(defn bit-flip
  "Flip bit at index n"
  {:added "1.0"
   :static true}
  [x n]
  (erlang/bxor.e x (erlang/bsl.e 1 n)))

(defn bit-test
  "Test bit at index n"
  {:added "1.0"
   :static true}
  [x n]
  (erlang/==.e 0 (erlang/band.e x (erlang/bsl.e 1 n))))


(defn bit-shift-left
  "Bitwise shift left"
  {:inline (fn [x n] `(erlang/bsl.e ~x ~n))
   :added "1.0"}
  [x n] (erlang/bsl.e x n))

(defn bit-shift-right
  "Bitwise shift right"
  {:inline (fn [x n] `(erlang/bsr.e ~x ~n))
   :added "1.0"}
  [x n] (erlang/bsr.e x n))

(defn unsigned-bit-shift-right
  "Bitwise shift right, without sign-extension."
  {:inline (fn [x n] `(erlang/bsr.e ~x ~n))
   :added "1.6"}
  [x n]
  (throw "unsupported unsigned operation"))

(defn integer?
  "Returns true if n is an integer"
  {:added "1.0"
   :static true}
  [n]
  (erlang/is_integer.e n))

(defn even?
  "Returns true if n is even, throws an exception if n is not an integer"
  {:added "1.0"
   :static true}
   [n] (if (integer? n)
        (zero? (bit-and n 1))
        (throw (str "Argument must be an integer: " n))))

(defn odd?
  "Returns true if n is odd, throws an exception if n is not an integer"
  {:added "1.0"
   :static true}
  [n] (not (even? n)))


;;

(defn complement
  "Takes a fn f and returns a fn that takes the same arguments as f,
  has the same effects, if any, and returns the opposite truth value."
  {:added "1.0"
   :static true}
  [f]
  (fn
    ([] (not (f)))
    ([x] (not (f x)))
    ([x y] (not (f x y)))
    ([x y & zs] (not (apply f x y zs)))))

(defn constantly
  "Returns a function that takes any number of arguments and returns x."
  {:added "1.0"
   :static true}
  [x] (fn [& args] x))

(defn identity
  "Returns its argument."
  {:added "1.0"
   :static true}
  [x] x)

;;Collection stuff

;;list stuff
(defn peek
  "For a list or queue, same as first, for a vector, same as, but much
  more efficient than, last. If the collection is empty, returns nil."
  {:added "1.0"
   :static true}
  [coll] (clj_core/peek.e coll))

(defn pop
  "For a list or queue, returns a new list/queue without the first
  item, for a vector, returns a new vector without the last item. If
  the collection is empty, throws an exception.  Note - not the same
  as next/butlast."
  {:added "1.0"
   :static true}
  [coll] (clj_core/pop.e coll))

;;map stuff

(defn map-entry?
  "Return true if x is a map entry"
  {:added "1.8"}
  [x]
  (and (vector? x) (= 2 (count x))))

(defn contains?
  "Returns true if key is present in the given collection, otherwise
  returns false.  Note that for numerically indexed collections like
  vectors and Java arrays, this tests if the numeric key is within the
  range of indexes. 'contains?' operates constant or logarithmic time;
  it will not perform a linear search for a value.  See also 'some'."
  {:added "1.0"
   :static true}
  [coll key] (clj_core/contains?.e coll key))

(defn get
  "Returns the value mapped to key, not-found or nil if key not present."
  {:inline (fn  [m k & nf] `(clj_core/get.e ~m ~k ~@nf))
   :inline-arities #{2 3}
   :added "1.0"}
  ([map key]
   (clj_core/get.e map key))
  ([map key not-found]
   (clj_core/get.e map key not-found)))

(defn dissoc
  "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,
  that does not contain a mapping for key(s)."
  {:added "1.0"
   :static true}
  ([map] map)
  ([map key]
   (clj_core/dissoc.e map key))
  ([map key & ks]
   (let [ret (dissoc map key)]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

(defn disj
  "disj[oin]. Returns a new set of the same (hashed/sorted) type, that
  does not contain key(s)."
  {:added "1.0"
   :static true}
  ([set] set)
  ([^clojure.lang.IPersistentSet set key]
   (when set
     (clj_core/disj.e set key)))
  ([set key & ks]
   (when set
     (let [ret (disj set key)]
       (if ks
         (recur ret (first ks) (next ks))
         ret)))))

(defn find
  "Returns the map entry for key, or nil if key not present."
  {:added "1.0"
   :static true}
  [map key] (clj_core/find.e map key))

(defn select-keys
  "Returns a map containing only those entries in map whose key is in keys"
  {:added "1.0"
   :static true}
  [map keyseq]
    (loop [ret {} keys (seq keyseq)]
      (if keys
        (let [entry (clj_core/find.e map (first keys))]
          (recur
           (if entry
             (conj ret entry)
             ret)
           (next keys)))
        (with-meta ret (meta map)))))

(defn keys
  "Returns a sequence of the map's keys, in the same order as (seq map)."
  {:added "1.0"
   :static true}
  [map] (clj_core/keys.e map))

(defn vals
  "Returns a sequence of the map's values, in the same order as (seq map)."
  {:added "1.0"
   :static true}
  [map] (clj_core/vals.e map))

(defn key
  "Returns the key of the map entry."
  {:added "1.0"
   :static true}
  [e]
  (first e))

(defn val
  "Returns the value in the map entry."
  {:added "1.0"
   :static true}
  [e]
  (second e))

(defn rseq
  "Returns, in constant time, a seq of the items in rev (which
  can be a vector or sorted-map), in reverse order. If rev is empty returns nil"
  {:added "1.0"
   :static true}
  [rev]
    (clj_core/rseq.e rev))

(defn name
  "Returns the name String of a string, symbol or keyword."
  {:tag clojerl.String
   :added "1.0"
   :static true}
  [x]
  (if (string? x) x (clj_core/name.e x)))

(defn namespace
  "Returns the namespace String of a symbol or keyword, or nil if not present."
  {:tag clojerl.String
   :added "1.0"
   :static true}
  [^clojure.lang.Named x]
  (clj_core/namespace.e x))

(defmacro ..
  "form => fieldName-symbol or (instanceMethodName-symbol args*)
  Expands into a member access (.) of the first member on the first
  argument, followed by the next member on the result, etc. For
  instance:
  (.. System (getProperties) (get \"os.name\"))
  expands to:
  (. (. System (getProperties)) (get \"os.name\"))
  but is easier to write, read, and understand."
  {:added "1.0"}
  ([x form] `(. ~x ~form))
  ([x form & more] `(.. (. ~x ~form) ~@more)))

(defmacro ->
  "Threads the expr through the forms. Inserts x as the
  second item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  second item in second form, etc."
  {:added "1.0"}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(~(first form) ~x ~@(next form)) (meta form))
                       (list form x))]
        (recur threaded (next forms)))
      x)))

(defmacro ->>
  "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  {:added "1.1"}
  [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
              (with-meta `(~(first form) ~@(next form)  ~x) (meta form))
              (list form x))]
        (recur threaded (next forms)))
      x)))

(def map)

(defn ^:private check-valid-options
  "Throws an exception if the given option map contains keys not listed
  as valid, else returns nil."
  [options & valid-keys]
  (when (seq (apply disj (apply hash-set (keys options)) valid-keys))
    (throw
      (apply str "Only these options are valid: "
             (first valid-keys)
             (map #(str ", " %) (rest valid-keys))))))

;;multimethods
(def global-hierarchy)

(defmacro defmulti
  "Creates a new multimethod with the associated dispatch function.
  The docstring and attr-map are optional.

  Options are key-value pairs and may be one of:

  :default

  The default dispatch value, defaults to :default

  :hierarchy

  The value used for hierarchical dispatch (e.g. ::square is-a ::shape)

  Hierarchies are type-like relationships that do not depend upon type
  inheritance. By default Clojure's multimethods dispatch off of a
  global hierarchy map.  However, a hierarchy relationship can be
  created with the derive function used to augment the root ancestor
  created with make-hierarchy.

  Multimethods expect the value of the hierarchy option to be supplied as
  a reference type e.g. a var (i.e. via the Var-quote dispatch macro #'
  or the var special form)."
  {:arglists '([name docstring? attr-map? dispatch-fn & options])
   :added "1.0"}
  [mm-name & options]
  (let [docstring   (if (string? (first options))
                      (first options)
                      nil)
        options     (if (string? (first options))
                      (next options)
                      options)
        m           (if (map? (first options))
                      (first options)
                      {})
        options     (if (map? (first options))
                      (next options)
                      options)
        dispatch-fn (first options)
        options     (next options)
        m           (if docstring
                      (assoc m :doc docstring)
                      m)
        m           (if (meta mm-name)
                      (conj (meta mm-name) m)
                      m)]
    (when (= (count options) 1)
      (throw "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)"))
    (let [options   (apply hash-map options)
          default   (get options :default :default)
          hierarchy (get options :hierarchy #'global-hierarchy)]
      (check-valid-options options :default :hierarchy)
      `(let [v# (def ~mm-name)]
         (when-not (and (clojerl.Var/has_root.e v#)
                        (instance? clojerl.MultiFn (deref v#)))
           (defn ~(with-meta mm-name m)
             [& args#]
             (let [val# (apply ~dispatch-fn args#)
                   f#    (clojerl.MultiFn/get_method.e ~(name mm-name) val# ~default ~hierarchy)]
               (when (nil? f#)
                 (throw (str "No multimethod defined for dispatch value " val#
                             " in " '~mm-name
                             )))
               (apply f# args#))))))))

(defmacro defmethod
  "Creates and installs a new method of multimethod associated with dispatch-value. "
  {:added "1.0"}
  [multifn dispatch-val & fn-tail]
  (let [fn-name (symbol (str (name multifn) "_method_" dispatch-val))]
    `(do
       (defn- ~fn-name ~@fn-tail)
       (erl-on-load*
        (clojerl.MultiFn/add_method.e ~(name multifn)
                                      ~dispatch-val
                                      (var ~fn-name))))))

(defn remove-all-methods
  "Removes all of the methods of multimethod."
  {:added "1.2"
   :static true}
  [^Var multifn]
  (clojerl.MultiFn/remove_all.e (name multifn)))

(defn remove-method
  "Removes the method of multimethod associated with dispatch-value."
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn dispatch-val]
  (clojerl.MultiFn/remove_method.e multifn dispatch-val))

(defn prefer-method
  "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y
   when there is a conflict"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn dispatch-val-x dispatch-val-y]
  (throw "unimplemented hierarchy"))

(defn methods
  "Given a multimethod, returns a map of dispatch values -> dispatch fns"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn]
  (clojerl.MultiFn/get_method_table.e multifn))

(defn get-method
  "Given a multimethod and a dispatch value, returns the dispatch fn
  that would apply to that value, or nil if none apply and no default"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn dispatch-val]
  (clojerl.MultiFn/get_method.e multifn dispatch-val))

(defn prefers
  "Given a multimethod, returns a map of preferred value -> set of other values"
  {:added "1.0"
   :static true}
  [^clojure.lang.MultiFn multifn]
  (throw "unimplemented hierarchy"))

;;;;;;;;; var stuff

(defmacro ^{:private true} assert-args
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form)))))
     ~(let [more (nnext pairs)]
        (when more
          (list* `assert-args more)))))

(defmacro set!
  [x val]
  (assert-args (symbol? x) "a symbol as its first argument")
  `(clj_core/set!.e (var ~x) ~val))

(defmacro if-let
  "bindings => binding-form test
  If test is true, evaluates then with binding-form bound to the value of
  test, if not, yields else"
  {:added "1.0"}
  ([bindings then]
   `(if-let ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert-args
     (vector? bindings) "a vector for its binding"
     (nil? oldform) "1 or 2 forms after binding vector"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if temp#
          (let [~form temp#]
            ~then)
          ~else)))))

(defmacro when-let
  "bindings => binding-form test
  When test is true, evaluates body with binding-form bound to the value of test"
  {:added "1.0"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (when temp#
         (let [~form temp#]
           ~@body)))))

(defmacro if-some
  "bindings => binding-form test
   If test is not nil, evaluates then with binding-form bound to the
   value of test, if not, yields else"
  {:added "1.6"}
  ([bindings then]
   `(if-some ~bindings ~then nil))
  ([bindings then else & oldform]
   (assert-args
     (vector? bindings) "a vector for its binding"
     (nil? oldform) "1 or 2 forms after binding vector"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
     `(let [temp# ~tst]
        (if (nil? temp#)
          ~else
          (let [~form temp#]
            ~then))))))

(defmacro when-some
  "bindings => binding-form test
   When test is not nil, evaluates body with binding-form bound to the
   value of test"
  {:added "1.6"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
   (let [form (bindings 0) tst (bindings 1)]
    `(let [temp# ~tst]
       (if (nil? temp#)
         nil
         (let [~form temp#]
           ~@body)))))

(defn push-thread-bindings
  "WARNING: This is a low-level function. Prefer high-level macros like
  binding where ever possible.

  Takes a map of Var/value pairs. Binds each Var to the associated value for
  the current thread. Each call *MUST* be accompanied by a matching call to
  pop-thread-bindings wrapped in a try-finally!

      (push-thread-bindings bindings)
      (try
        ...
        (finally
          (pop-thread-bindings)))"
  {:added "1.1"
   :static true}
  [bindings]
  (clojerl.Var/push_bindings.e bindings))

(defn pop-thread-bindings
  "Pop one set of bindings pushed with push-binding before. It is an error to
  pop bindings without pushing before."
  {:added "1.1"
   :static true}
  []
  (clojerl.Var/pop_bindings.e))

(defn get-thread-bindings
  "Get a map with the Var/value pairs which is currently in effect for the
  current thread."
  {:added "1.1"
   :static true}
  []
  (clojerl.Var/get_bindings_map.e))

(defmacro binding
  "binding => var-symbol init-expr

  Creates new bindings for the (already-existing) vars, with the
  supplied initial values, executes the exprs in an implicit do, then
  re-establishes the bindings that existed before.  The new bindings
  are made in parallel (unlike let); all init-exprs are evaluated
  before the vars are bound to their new values."
  {:added "1.0"}
  [bindings & body]
  (assert-args
    (vector? bindings) "a vector for its binding"
    (even? (count bindings)) "an even number of forms in binding vector")
  (let [var-ize (fn [var-vals]
                  (loop [ret [] vvs (seq var-vals)]
                    (if vvs
                      (recur  (conj (conj ret `(var ~(first vvs))) (second vvs))
                             (next (next vvs)))
                      (seq ret))))]
    `(let []
       (push-thread-bindings (hash-map ~@(var-ize bindings)))
       (try
         ~@body
         (finally
           (pop-thread-bindings))))))

(defn with-bindings*
  "Takes a map of Var/value pairs. Installs for the given Vars the associated
  values as thread-local bindings. Then calls f with the supplied arguments.
  Pops the installed bindings after f returned. Returns whatever f returns."
  {:added "1.1"
   :static true}
  [binding-map f & args]
  (push-thread-bindings binding-map)
  (try
    (apply f args)
    (finally
      (pop-thread-bindings))))

(defmacro with-bindings
  "Takes a map of Var/value pairs. Installs for the given Vars the associated
  values as thread-local bindings. Then executes body. Pops the installed
  bindings after body was evaluated. Returns the value of body."
  {:added "1.1"}
  [binding-map & body]
  `(with-bindings* ~binding-map (fn [] ~@body)))

(defn bound-fn*
  "Returns a function, which will install the same bindings in effect as in
  the thread at the time bound-fn* was called and then call f with any given
  arguments. This may be used to define a helper function which runs on a
  different thread, but needs the same bindings in place."
  {:added "1.1"
   :static true}
  [f]
  (let [bindings (get-thread-bindings)]
    (fn [& args]
      (apply with-bindings* bindings f args))))

(defmacro bound-fn
  "Returns a function defined by the given fntail, which will install the
  same bindings in effect as in the thread at the time bound-fn was called.
  This may be used to define a helper function which runs on a different
  thread, but needs the same bindings in place."
  {:added "1.1"}
  [& fntail]
  `(bound-fn* (fn ~@fntail)))

(defn find-var
  "Returns the global var named by the namespace-qualified symbol, or
  nil if no var with that name."
  {:added "1.0"
   :static true}
  [sym] (clj_namespace/find_var.e sym))

(defn binding-conveyor-fn
  {:private true
   :added "1.3"}
  [f]
  (let [frame (clojerl.Var/get_bindings.e)]
    (fn
      ([]
         (clojerl.Var/reset_bindings.e frame)
         (f))
      ([x]
         (clojerl.Var/reset_bindings.e frame)
         (f x))
      ([x y]
         (clojerl.Var/reset_bindings.e frame)
         (f x y))
      ([x y z]
         (clojerl.Var/reset_bindings.e frame)
         (f x y z))
      ([x y z & args]
         (clojerl.Var/reset_bindings.e frame)
         (apply f x y z args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Refs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ^{:private true}
  setup-reference [^clojure.lang.ARef r options]
  (throw "unimplemented ref")
  #_(let [opts (apply hash-map options)]
    (when (:meta opts)
      (.resetMeta r (:meta opts)))
    (when (:validator opts)
      (.setValidator r (:validator opts)))
    r))

(defn agent
  "Creates and returns an agent with an initial value of state and
  zero or more options (in any order):

  :meta metadata-map

  :validator validate-fn

  :error-handler handler-fn

  :error-mode mode-keyword

  If metadata-map is supplied, it will become the metadata on the
  agent. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception.  handler-fn is called if an
  action throws an exception or if validate-fn rejects a new state --
  see set-error-handler! for details.  The mode-keyword may be either
  :continue (the default if an error-handler is given) or :fail (the
  default if no error-handler is given) -- see set-error-mode! for
  details."
  {:added "1.0"
   :static true
   }
  ([state & options]
   (throw "unimplemented agent")
   #_(let [a (new clojure.lang.Agent state)
         opts (apply hash-map options)]
     (setup-reference a options)
     (when (:error-handler opts)
       (.setErrorHandler a (:error-handler opts)))
     (.setErrorMode a (or (:error-mode opts)
                          (if (:error-handler opts) :continue :fail)))
     a)))

(defn set-agent-send-executor!
  "Sets the ExecutorService to be used by send"
  {:added "1.5"}
  [executor]
  (throw "unimplemented agent")
  #_(set! clojure.lang.Agent/pooledExecutor executor))

(defn set-agent-send-off-executor!
  "Sets the ExecutorService to be used by send-off"
  {:added "1.5"}
  [executor]
  (throw "unimplemented agent")
  #_(set! clojure.lang.Agent/soloExecutor executor))

(defn send-via
  "Dispatch an action to an agent. Returns the agent immediately.
  Subsequently, in a thread supplied by executor, the state of the agent
  will be set to the value of:

  (apply action-fn state-of-agent args)"
  {:added "1.5"}
  [executor ^clojure.lang.Agent a f & args]
  (throw "unimplemented agent")
  #_(.dispatch a (binding [*agent* a] (binding-conveyor-fn f)) args executor))

(defn send
  "Dispatch an action to an agent. Returns the agent immediately.
  Subsequently, in a thread from a thread pool, the state of the agent
  will be set to the value of:

  (apply action-fn state-of-agent args)"
  {:added "1.0"
   :static true}
  [^clojure.lang.Agent a f & args]
  (throw "unimplemented agent")
  #_(apply send-via clojure.lang.Agent/pooledExecutor a f args))

(defn send-off
  "Dispatch a potentially blocking action to an agent. Returns the
  agent immediately. Subsequently, in a separate thread, the state of
  the agent will be set to the value of:

  (apply action-fn state-of-agent args)"
  {:added "1.0"
   :static true}
  [^clojure.lang.Agent a f & args]
  (throw "unimplemented agent")
  #_(apply send-via clojure.lang.Agent/soloExecutor a f args))

(defn release-pending-sends
  "Normally, actions sent directly or indirectly during another action
  are held until the action completes (changes the agent's
  state). This function can be used to dispatch any pending sent
  actions immediately. This has no impact on actions sent during a
  transaction, which are still held until commit. If no action is
  occurring, does nothing. Returns the number of actions dispatched."
  {:added "1.0"
   :static true}
  []
  (throw "unimplemented agent")
  #_(clojure.lang.Agent/releasePendingSends))

(defn add-watch
  "Adds a watch function to an agent/atom/var/ref reference. The watch
  fn must be a fn of 4 args: a key, the reference, its old-state, its
  new-state. Whenever the reference's state might have been changed,
  any registered watches will have their functions called. The watch fn
  will be called synchronously, on the agent's thread if an agent,
  before any pending sends if agent or ref. Note that an atom's or
  ref's state may have changed again prior to the fn call, so use
  old/new-state rather than derefing the reference. Note also that watch
  fns may be called from multiple threads simultaneously. Var watchers
  are triggered only by root binding changes, not thread-local
  set!s. Keys must be unique per reference, and can be used to remove
  the watch with remove-watch, but are otherwise considered opaque by
  the watch mechanism."
  {:added "1.0"
   :static true}
  [^clojure.lang.IRef reference key fn]
  (throw "unimplemented agent/atom/var/ref")
  #_(.addWatch reference key fn))

(defn remove-watch
  "Removes a watch (set by add-watch) from a reference"
  {:added "1.0"
   :static true}
  [^clojure.lang.IRef reference key]
  (throw "unimplemented agent/atom/var/ref")
  #_(.removeWatch reference key))

(defn agent-error
  "Returns the exception thrown during an asynchronous action of the
  agent if the agent is failed.  Returns nil if the agent is not
  failed."
  {:added "1.2"
   :static true}
  [^clojure.lang.Agent a]
  (throw "unimplemented agent")
  #_(.getError a))

(defn restart-agent
  "When an agent is failed, changes the agent state to new-state and
  then un-fails the agent so that sends are allowed again.  If
  a :clear-actions true option is given, any actions queued on the
  agent that were being held while it was failed will be discarded,
  otherwise those held actions will proceed.  The new-state must pass
  the validator if any, or restart will throw an exception and the
  agent will remain failed with its old state and error.  Watchers, if
  any, will NOT be notified of the new state.  Throws an exception if
  the agent is not failed."
  {:added "1.2"
   :static true
   }
  [^clojure.lang.Agent a, new-state & options]
  (throw "unimplemented agent")
  #_(let [opts (apply hash-map options)]
    (.restart a new-state (if (:clear-actions opts) true false))))

(defn set-error-handler!
  "Sets the error-handler of agent a to handler-fn.  If an action
  being run by the agent throws an exception or doesn't pass the
  validator fn, handler-fn will be called with two arguments: the
  agent and the exception."
  {:added "1.2"
   :static true}
  [^clojure.lang.Agent a, handler-fn]
  (throw "unimplemented agent")
  #_(.setErrorHandler a handler-fn))

(defn error-handler
  "Returns the error-handler of agent a, or nil if there is none.
  See set-error-handler!"
  {:added "1.2"
   :static true}
  [^clojure.lang.Agent a]
  (throw "unimplemented agent")
  #_(.getErrorHandler a))

(defn set-error-mode!
  "Sets the error-mode of agent a to mode-keyword, which must be
  either :fail or :continue.  If an action being run by the agent
  throws an exception or doesn't pass the validator fn, an
  error-handler may be called (see set-error-handler!), after which,
  if the mode is :continue, the agent will continue as if neither the
  action that caused the error nor the error itself ever happened.

  If the mode is :fail, the agent will become failed and will stop
  accepting new 'send' and 'send-off' actions, and any previously
  queued actions will be held until a 'restart-agent'.  Deref will
  still work, returning the state of the agent before the error."
  {:added "1.2"
   :static true}
  [^clojure.lang.Agent a, mode-keyword]
  (throw "unimplemented agent")
  #_(.setErrorMode a mode-keyword))

(defn error-mode
  "Returns the error-mode of agent a.  See set-error-mode!"
  {:added "1.2"
   :static true}
  [^clojure.lang.Agent a]
  (throw "unimplemented agent")
  #_(.getErrorMode a))

(defn agent-errors
  "DEPRECATED: Use 'agent-error' instead.
  Returns a sequence of the exceptions thrown during asynchronous
  actions of the agent."
  {:added "1.0"
   :deprecated "1.2"}
  [a]
  (when-let [e (agent-error a)]
    (list e)))

(defn clear-agent-errors
  "DEPRECATED: Use 'restart-agent' instead.
  Clears any exceptions thrown during asynchronous actions of the
  agent, allowing subsequent actions to occur."
  {:added "1.0"
   :deprecated "1.2"}
  [^clojure.lang.Agent a]
  (throw "unimplemented agent")
  #_(restart-agent a (.deref a)))

(defn shutdown-agents
  "Initiates a shutdown of the thread pools that back the agent
  system. Running actions will complete, but no new actions will be
  accepted"
  {:added "1.0"
   :static true}
  []
  (throw "unimplemented agent")
  #_(. clojure.lang.Agent shutdown))

(defn ref
  "Creates and returns a Ref with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  :min-history (default 0)
  :max-history (default 10)

  If metadata-map is supplied, it will become the metadata on the
  ref. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception. validate-fn will be called on
  transaction commit, when all refs have their final values.

  Normally refs accumulate history dynamically as needed to deal with
  read demands. If you know in advance you will need history you can
  set :min-history to ensure it will be available when first needed (instead
  of after a read fault). History is limited, and the limit can be set
  with :max-history."
  {:added "1.0"
   :static true
   }
  ([x]
   (throw "unimplemented ref")
   #_(new clojure.lang.Ref x))
  ([x & options]
   (throw "unimplemented ref")
   #_(let [r  ^clojure.lang.Ref (setup-reference (ref x) options)
         opts (apply hash-map options)]
    (when (:max-history opts)
      (.setMaxHistory r (:max-history opts)))
    (when (:min-history opts)
      (.setMinHistory r (:min-history opts)))
    r)))

(defn ^:private deref-future
  ([^java.util.concurrent.Future fut]
   (throw "unimplemented ref")
   #_(.get fut))
  ([^java.util.concurrent.Future fut timeout-ms timeout-val]
   (throw "unimplemented ref")
   #_(try (.get fut timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
        (catch java.util.concurrent.TimeoutException e
          timeout-val))))

(defn deref
  "Also reader macro: @ref/@agent/@var/@atom/@delay/@future/@promise. Within a transaction,
  returns the in-transaction-value of ref, else returns the
  most-recently-committed value of ref. When applied to a var, agent
  or atom, returns its current state. When applied to a delay, forces
  it if not already forced. When applied to a future, will block if
  computation not complete. When applied to a promise, will block
  until a value is delivered.  The variant taking a timeout can be
  used for blocking references (futures and promises), and will return
  timeout-val if the timeout (in milliseconds) is reached before a
  value is available. See also - realized?."
  {:added "1.0"
   :static true}
  ([ref]
   (if (satisfies? clojerl.IDeref ref)
     (clj_core/deref.e ref)
     (throw (str "unimplemented deref for type " (clj_core/type.e ref)))
     #_(deref-future ref)))
  ([ref timeout-ms timeout-val]
   (throw "unimplemented timeout for deref")
   #_(if (instance? clojure.lang.IBlockingDeref ref)
       (.deref ^clojure.lang.IBlockingDeref ref timeout-ms timeout-val)
       (deref-future ref timeout-ms timeout-val))))

(defn atom
  "Creates and returns an Atom with an initial value of x and zero or
  more options (in any order):

  :meta metadata-map

  :validator validate-fn

  If metadata-map is supplied, it will become the metadata on the
  atom. validate-fn must be nil or a side-effect-free fn of one
  argument, which will be passed the intended new state on any state
  change. If the new state is unacceptable, the validate-fn should
  return false or throw an exception."
  {:added "1.0"
   :static true}
  ([x]
   (throw "unimplemented atom")
   #_(new clojure.lang.Atom x))
  ([x & options] (setup-reference (atom x) options)))

(defn swap!
  "Atomically swaps the value of atom to be:
  (apply f current-value-of-atom args). Note that f may be called
  multiple times, and thus should be free of side effects.  Returns
  the value that was swapped in."
  {:added "1.0"
   :static true}
  ([^clojure.lang.IAtom atom f]
   (throw "unimplemented atom")
   #_(.swap atom f))
  ([^clojure.lang.IAtom atom f x]
   (throw "unimplemented atom")
   #_(.swap atom f x))
  ([^clojure.lang.IAtom atom f x y]
   (throw "unimplemented atom")
   #_(.swap atom f x y))
  ([^clojure.lang.IAtom atom f x y & args]
   (throw "unimplemented atom")
   #_(.swap atom f x y args)))

(defn compare-and-set!
  "Atomically sets the value of atom to newval if and only if the
  current value of the atom is identical to oldval. Returns true if
  set happened, else false"
  {:added "1.0"
   :static true}
  [^clojure.lang.IAtom atom oldval newval]
  (throw "unimplemented atom")
  #_(.compareAndSet atom oldval newval))

(defn reset!
  "Sets the value of atom to newval without regard for the
  current value. Returns newval."
  {:added "1.0"
   :static true}
  [^clojure.lang.IAtom atom newval]
  (throw "unimplemented atom")
  #_(.reset atom newval))

(defn set-validator!
  "Sets the validator-fn for a var/ref/agent/atom. validator-fn must be nil or a
  side-effect-free fn of one argument, which will be passed the intended
  new state on any state change. If the new state is unacceptable, the
  validator-fn should return false or throw an exception. If the current state (root
  value if var) is not acceptable to the new validator, an exception
  will be thrown and the validator will not be changed."
  {:added "1.0"
   :static true}
  [^clojure.lang.IRef iref validator-fn]
  (throw "unimplemented var/ref/agent/atom")
  #_(. iref (setValidator validator-fn)))

(defn get-validator
  "Gets the validator-fn for a var/ref/agent/atom."
  {:added "1.0"
   :static true}
  [^clojure.lang.IRef iref]
  (throw "unimplemented var/ref/agent/atom")
  #_(. iref (getValidator)))

(defn alter-meta!
  "Atomically sets the metadata for a namespace/var/ref/agent/atom to be:

  (apply f its-current-meta args)

  f must be free of side-effects"
  {:added "1.0"
   :static true}
  [^clojure.lang.IReference iref f & args]
  (throw "unimplemented var/ref/agent/atom")
  #_(.alterMeta iref f args))

(defn reset-meta!
  "Atomically resets the metadata for a namespace/var/ref/agent/atom"
  {:added "1.0"
   :static true}
  [^clojure.lang.IReference iref metadata-map]
  (throw "unimplemented var/ref/agent/atom")
  #_(.resetMeta iref metadata-map))

(defn commute
  "Must be called in a transaction. Sets the in-transaction-value of
  ref to:

  (apply fun in-transaction-value-of-ref args)

  and returns the in-transaction-value of ref.

  At the commit point of the transaction, sets the value of ref to be:

  (apply fun most-recently-committed-value-of-ref args)

  Thus fun should be commutative, or, failing that, you must accept
  last-one-in-wins behavior.  commute allows for more concurrency than
  ref-set."
  {:added "1.0"
   :static true}

  [^clojure.lang.Ref ref fun & args]
  (throw "unimplemented ref")
  #_(. ref (commute fun args)))

(defn alter
  "Must be called in a transaction. Sets the in-transaction-value of
  ref to:

  (apply fun in-transaction-value-of-ref args)

  and returns the in-transaction-value of ref."
  {:added "1.0"
   :static true}
  [^clojure.lang.Ref ref fun & args]
  (throw "unimplemented ref")
  #_(. ref (alter fun args)))

(defn ref-set
  "Must be called in a transaction. Sets the value of ref.
  Returns val."
  {:added "1.0"
   :static true}
  [^clojure.lang.Ref ref val]
  (throw "unimplemented ref")
  #_(. ref (set val)))

(defn ref-history-count
  "Returns the history count of a ref"
  {:added "1.1"
   :static true}
  [^clojure.lang.Ref ref]
  (throw "unimplemented")
  #_(.getHistoryCount ref))

(defn ref-min-history
  "Gets the min-history of a ref, or sets it and returns the ref"
  {:added "1.1"
   :static true}
  ([^clojure.lang.Ref ref]
   (throw "unimplemented ref")
   #_(.getMinHistory ref))
  ([^clojure.lang.Ref ref n]
   (throw "unimplemented ref")
   #_(.setMinHistory ref n)))

(defn ref-max-history
  "Gets the max-history of a ref, or sets it and returns the ref"
  {:added "1.1"
   :static true}
  ([^clojure.lang.Ref ref]
   (throw "unimplemented ref")
   #_(.getMaxHistory ref))
  ([^clojure.lang.Ref ref n]
   (throw "unimplemented ref")
   #_(.setMaxHistory ref n)))

(defn ensure
  "Must be called in a transaction. Protects the ref from modification
  by other transactions.  Returns the in-transaction-value of
  ref. Allows for more concurrency than (ref-set ref @ref)"
  {:added "1.0"
   :static true}
  [^clojure.lang.Ref ref]
  (throw "unimplemented ref")
  #_(. ref (touch))
  #_(. ref (deref)))

(defmacro sync
  "transaction-flags => TBD, pass nil for now

  Runs the exprs (in an implicit do) in a transaction that encompasses
  exprs and any nested calls.  Starts a transaction if none is already
  running on this thread. Any uncaught exception will abort the
  transaction and flow out of sync. The exprs may be run more than
  once, but any effects on Refs will be atomic."
  {:added "1.0"}
  [flags-ignored-for-now & body]
  (throw "unimplemented ref")
  #_`(. clojure.lang.LockingTransaction
      (runInTransaction (fn [] ~@body))))


(defmacro io!
  "If an io! block occurs in a transaction, throws an
  error, else runs body in an implicit do. If the
  first expression in body is a literal string, will use that as the
  exception message."
  {:added "1.0"}
  [& body]
  (throw "unimplemented ref")
  #_(let [message (when (string? (first body)) (first body))
        body (if message (next body) body)]
    `(if (clojure.lang.LockingTransaction/isRunning)
       (throw (new IllegalStateException ~(or message "I/O in transaction")))
       (do ~@body))))

#_(defn volatile!
  "Creates and returns a Volatile with an initial value of val."
  {:added "1.7"
   :tag clojure.lang.Volatile}
  [val]
  (throw "unimplemented volatile")
  #_(clojure.lang.Volatile. val))

#_(defn vreset!
  "Sets the value of volatile to newval without regard for the
   current value. Returns newval."
  {:added "1.7"}
  [^clojure.lang.Volatile vol newval]
  (throw "unimplemented volatile")
  #_(.reset vol newval))

#_(defmacro vswap!
  "Non-atomically swaps the value of the volatile as if:
   (apply f current-value-of-vol args). Returns the value that
   was swapped in."
  {:added "1.7"}
  [vol f & args]
  (throw "unimplemented volatile")
  #_(let [v (with-meta vol {:tag 'clojure.lang.Volatile})]
    `(.reset ~v (~f (.deref ~v) ~@args))))

#_(defn volatile?
  "Returns true if x is a volatile."
  {:added "1.7"}
  [x]
  (throw "unimplemented volatile")
  #_(instance? clojure.lang.Volatile x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; fn stuff ;;;;;;;;;;;;;;;;


(defn comp
  "Takes a set of functions and returns a fn that is the composition
  of those fns.  The returned fn takes a variable number of args,
  applies the rightmost of fns to the args, the next
  fn (right-to-left) to the result, etc."
  {:added "1.0"
   :static true}
  ([] identity)
  ([f] f)
  ([f g]
     (fn
       ([] (f (g)))
       ([x] (f (g x)))
       ([x y] (f (g x y)))
       ([x y z] (f (g x y z)))
       ([x y z & args] (f (apply g x y z args)))))
  ([f g & fs]
     (reduce1 comp (list* f g fs))))

(defn juxt
  "Takes a set of functions and returns a fn that is the juxtaposition
  of those fns.  The returned fn takes a variable number of args, and
  returns a vector containing the result of applying each fn to the
  args (left-to-right).
  ((juxt a b c) x) => [(a x) (b x) (c x)]"
  {:added "1.1"
   :static true}
  ([f]
     (fn
       ([] [(f)])
       ([x] [(f x)])
       ([x y] [(f x y)])
       ([x y z] [(f x y z)])
       ([x y z & args] [(apply f x y z args)])))
  ([f g]
     (fn
       ([] [(f) (g)])
       ([x] [(f x) (g x)])
       ([x y] [(f x y) (g x y)])
       ([x y z] [(f x y z) (g x y z)])
       ([x y z & args] [(apply f x y z args) (apply g x y z args)])))
  ([f g h]
     (fn
       ([] [(f) (g) (h)])
       ([x] [(f x) (g x) (h x)])
       ([x y] [(f x y) (g x y) (h x y)])
       ([x y z] [(f x y z) (g x y z) (h x y z)])
       ([x y z & args] [(apply f x y z args) (apply g x y z args) (apply h x y z args)])))
  ([f g h & fs]
     (let [fs (list* f g h fs)]
       (fn
         ([] (reduce1 #(conj %1 (%2)) [] fs))
         ([x] (reduce1 #(conj %1 (%2 x)) [] fs))
         ([x y] (reduce1 #(conj %1 (%2 x y)) [] fs))
         ([x y z] (reduce1 #(conj %1 (%2 x y z)) [] fs))
         ([x y z & args] (reduce1 #(conj %1 (apply %2 x y z args)) [] fs))))))

(defn partial
  "Takes a function f and fewer than the normal arguments to f, and
  returns a fn that takes a variable number of additional args. When
  called, the returned function calls f with args + additional args."
  {:added "1.0"
   :static true}
  ([f] f)
  ([f arg1]
   (fn
     ([] (f arg1))
     ([x] (f arg1 x))
     ([x y] (f arg1 x y))
     ([x y z] (f arg1 x y z))
     ([x y z & args] (apply f arg1 x y z args))))
  ([f arg1 arg2]
   (fn
     ([] (f arg1 arg2))
     ([x] (f arg1 arg2 x))
     ([x y] (f arg1 arg2 x y))
     ([x y z] (f arg1 arg2 x y z))
     ([x y z & args] (apply f arg1 arg2 x y z args))))
  ([f arg1 arg2 arg3]
   (fn
     ([] (f arg1 arg2 arg3))
     ([x] (f arg1 arg2 arg3 x))
     ([x y] (f arg1 arg2 arg3 x y))
     ([x y z] (f arg1 arg2 arg3 x y z))
     ([x y z & args] (apply f arg1 arg2 arg3 x y z args))))
  ([f arg1 arg2 arg3 & more]
   (fn [& args] (apply f arg1 arg2 arg3 (concat more args)))))

;;;;;;;;;;;;;;;;;;; sequence fns  ;;;;;;;;;;;;;;;;;;;;;;;

(defn sequence
  "Coerces coll to a (possibly empty) sequence, if it is not already
  one. Will not force a lazy seq. (sequence nil) yields (), When a
  transducer is supplied, returns a lazy sequence of applications of
  the transform to the items in coll(s), i.e. to the set of first
  items of each coll, followed by the set of second
  items in each coll, until any one of the colls is exhausted.  Any
  remaining items in other colls are ignored. The transform should accept
  number-of-colls arguments"
  {:added "1.0"
   :static true}
  ([coll]
     (if (seq? coll) coll
         (or (seq coll) ())))
  #_ (([xform coll]
       (or (clojure.lang.RT/chunkIteratorSeq
            (clojure.lang.TransformerIterator/create xform (clojure.lang.RT/iter coll)))
           ()))
      ([xform coll & colls]
       (or (clojure.lang.RT/chunkIteratorSeq
            (clojure.lang.TransformerIterator/createMulti
             xform
             (map #(clojure.lang.RT/iter %) (cons coll colls))))
           ()))))

(defn every?
  "Returns true if (pred x) is logical true for every x in coll, else
  false."
  {:tag clojerl.Boolean
   :added "1.0"
   :static true}
  [pred coll]
  (cond
   (nil? (seq coll)) true
   (pred (first coll)) (recur pred (next coll))
   :else false))

(def
 ^{:tag clojerl.Boolean
   :doc "Returns false if (pred x) is logical true for every x in
  coll, else true."
   :arglists '([pred coll])
   :added "1.0"}
 not-every? (comp not every?))

(defn some
  "Returns the first logical true value of (pred x) for any x in coll,
  else nil.  One common idiom is to use a set as pred, for example
  this will return :fred if :fred is in the sequence, otherwise nil:
  (some #{:fred} coll)"
  {:added "1.0"
   :static true}
  [pred coll]
    (when (seq coll)
      (or (pred (first coll)) (recur pred (next coll)))))

(def
 ^{:tag clojerl.Boolean
   :doc "Returns false if (pred x) is logical true for any x in coll,
  else true."
   :arglists '([pred coll])
   :added "1.0"}
 not-any? (comp not some))

;will be redefed later with arg checks
(defmacro dotimes
  "bindings => name n
  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  {:added "1.0"}
  [bindings & body]
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# ~n]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (inc ~i)))))))

(defn map
  "Returns a lazy sequence consisting of the result of applying f to
  the set of first items of each coll, followed by applying f to the
  set of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments. Returns a transducer when
  no collection is provided."
  {:added "1.0"
   :static true}
  ([f]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
           (rf result (f input)))
        ([result input & inputs]
           (rf result (apply f input inputs))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (cons (f (first s)) (map f (rest s))))))
  ([f c1 c2]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2)]
      (when (and s1 s2)
        (cons (f (first s1) (first s2))
              (map f (rest s1) (rest s2)))))))
  ([f c1 c2 c3]
   (lazy-seq
    (let [s1 (seq c1) s2 (seq c2) s3 (seq c3)]
      (when (and  s1 s2 s3)
        (cons (f (first s1) (first s2) (first s3))
              (map f (rest s1) (rest s2) (rest s3)))))))
  ([f c1 c2 c3 & colls]
   (let [step (fn step [cs]
                 (lazy-seq
                  (let [ss (map seq cs)]
                    (when (every? identity ss)
                      (cons (map first ss) (step (map rest ss)))))))]
     (map #(apply f %) (step (conj colls c3 c2 c1))))))

(defmacro declare
  "defs the supplied var names with no bindings, useful for making forward declarations."
  {:added "1.0"}
  [& names] `(do ~@(map #(list 'def (vary-meta % assoc :declared true)) names)))

(declare cat)

(defn mapcat
  "Returns the result of applying concat to the result of applying map
  to f and colls.  Thus function f should return a collection. Returns
  a transducer when no collections are provided"
  {:added "1.0"
   :static true}
  ([f] (comp (map f) cat))
  ([f & colls]
     (apply concat (apply map f colls))))

(defn filter
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns true. pred must be free of side-effects.
  Returns a transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([pred]
    (fn [rf]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result input]
           (if (pred input)
             (rf result input)
             result)))))
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (if true #_(chunked-seq? s)
        #_(let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
              (let [v (.nth c i)]
                (when (pred v)
                  (chunk-append b v))))
          (chunk-cons (chunk b) (filter pred (chunk-rest s))))
        (let [f (first s) r (rest s)]
          (if (pred f)
            (cons f (filter pred r))
            (filter pred r))))))))

(defn remove
  "Returns a lazy sequence of the items in coll for which
  (pred item) returns false. pred must be free of side-effects.
  Returns a transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([pred] (filter (complement pred)))
  ([pred coll]
     (filter (complement pred) coll)))

(defn reduced
  "Wraps x in a way such that a reduce will terminate with the value x"
  {:added "1.5"}
  [x]
  (new clojerl.Reduced x))

(defn reduced?
  "Returns true if x is the result of a call to reduced"
  {:inline (fn [x] `(clj_core/is_reduced.e ~x ))
   :inline-arities #{1}
   :added "1.5"}
  [x]
  (clojerl.Reduced/is_reduced.e x))

(defn ensure-reduced
  "If x is already reduced?, returns it, else returns (reduced x)"
  {:added "1.7"}
  [x]
  (if (reduced? x) x (reduced x)))

(defn unreduced
  "If x is reduced?, returns (deref x), else returns x"
  {:added "1.7"}
  [x]
  (if (reduced? x) (deref x) x))

(defn take
  "Returns a lazy sequence of the first n items in coll, or all items if
  there are fewer than n.  Returns a stateful transducer when
  no collection is provided."
  {:added "1.0"
   :static true}
  #_([n]
     (fn [rf]
       (let [nv (volatile! n)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [n @nv
                    nn (vswap! nv dec)
                    result (if (pos? n)
                             (rf result input)
                             result)]
                (if (not (pos? nn))
                  (ensure-reduced result)
                  result)))))))
  ([n coll]
     (lazy-seq
      (when (pos? n)
        (when-let [s (seq coll)]
          (cons (first s) (take (dec n) (rest s))))))))

(defn take-while
  "Returns a lazy sequence of successive items from coll while
  (pred item) returns true. pred must be free of side-effects.
  Returns a transducer when no collection is provided."
  {:added "1.0"
   :static true}
  ([pred]
     (fn [rf]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (if (pred input)
              (rf result input)
              (reduced result))))))
  ([pred coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (when (pred (first s))
          (cons (first s) (take-while pred (rest s))))))))

(defn drop
  "Returns a lazy sequence of all but the first n items in coll.
  Returns a stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  #_([n]
     (fn [rf]
       (let [nv (volatile! n)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [n @nv]
                (vswap! nv dec)
                (if (pos? n)
                  result
                  (rf result input))))))))
  ([n coll]
     (let [step (fn [n coll]
                  (let [s (seq coll)]
                    (if (and (pos? n) s)
                      (recur (dec n) (rest s))
                      s)))]
       (lazy-seq (step n coll)))))

(defn drop-last
  "Return a lazy sequence of all but the last n (default 1) items in coll"
  {:added "1.0"
   :static true}
  ([s] (drop-last 1 s))
  ([n s] (map (fn [x _] x) s (drop n s))))

(defn take-last
  "Returns a seq of the last n items in coll.  Depending on the type
  of coll may be no better than linear time.  For vectors, see also subvec."
  {:added "1.1"
   :static true}
  [n coll]
  (loop [s (seq coll), lead (seq (drop n coll))]
    (if lead
      (recur (next s) (next lead))
      s)))

(defn drop-while
  "Returns a lazy sequence of the items in coll starting from the
  first item for which (pred item) returns logical false.  Returns a
  stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  #_([pred]
     (fn [rf]
       (let [dv (volatile! true)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [drop? @dv]
                (if (and drop? (pred input))
                  result
                  (do
                    (vreset! dv nil)
                    (rf result input)))))))))
  ([pred coll]
     (let [step (fn [pred coll]
                  (let [s (seq coll)]
                    (if (and s (pred (first s)))
                      (recur pred (rest s))
                      s)))]
       (lazy-seq (step pred coll)))))

(defn cycle
  "Returns a lazy (infinite!) sequence of repetitions of the items in coll."
  {:added "1.0"
   :static true}
  [coll] (lazy-seq
          (when-let [s (seq coll)]
            (concat s (cycle s)))))

(defn split-at
  "Returns a vector of [(take n coll) (drop n coll)]"
  {:added "1.0"
   :static true}
  [n coll]
    [(take n coll) (drop n coll)])

(defn split-with
  "Returns a vector of [(take-while pred coll) (drop-while pred coll)]"
  {:added "1.0"
   :static true}
  [pred coll]
    [(take-while pred coll) (drop-while pred coll)])

(defn repeat
  "Returns a lazy (infinite!, or length n if supplied) sequence of xs."
  {:added "1.0"
   :static true}
  ([x] (lazy-seq (cons x (repeat x))))
  ([n x] (take n (repeat x))))

(defn replicate
  "DEPRECATED: Use 'repeat' instead.
   Returns a lazy seq of n xs."
  {:added "1.0"
   :deprecated "1.3"}
  [n x] (take n (repeat x)))

(defn iterate
  "Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects"
  {:added "1.0"
   :static true}
  [f x] (cons x (lazy-seq (iterate f (f x)))))

(defn range
  "Returns a lazy seq of nums from start (inclusive) to end
  (exclusive), by step, where start defaults to 0, step to 1, and end to
  infinity. When step is equal to 0, returns an infinite sequence of
  start. When start is equal to end, returns empty list."
  {:added "1.0"
   :static true}
  ([] (iterate inc 0))
  ([end] (range 0 end 1))
  ([start end] (range start end 1))
  ([start end step]
   (new clojerl.Range start end step)))

(defn merge
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping from
  the latter (left-to-right) will be the mapping in the result."
  {:added "1.0"
   :static true}
  [& maps]
  (when (some identity maps)
    (reduce1 #(conj (or %1 {}) %2) maps)))

(defn merge-with
  "Returns a map that consists of the rest of the maps conj-ed onto
  the first.  If a key occurs in more than one map, the mapping(s)
  from the latter (left-to-right) will be combined with the mapping in
  the result by calling (f val-in-result val-in-latter)."
  {:added "1.0"
   :static true}
  [f & maps]
  (when (some identity maps)
    (let [merge-entry (fn [m e]
                        (let [k (key e) v (val e)]
                          (if (contains? m k)
                            (assoc m k (f (get m k) v))
                            (assoc m k v))))
          merge2 (fn [m1 m2]
                   (reduce1 merge-entry (or m1 {}) (seq m2)))]
      (reduce1 merge2 maps))))



(defn zipmap
  "Returns a map with the keys mapped to the corresponding vals."
  {:added "1.0"
   :static true}
  [keys vals]
    (loop [map {}
           ks (seq keys)
           vs (seq vals)]
      (if (and ks vs)
        (recur (assoc map (first ks) (first vs))
               (next ks)
               (next vs))
        map)))

(defn line-seq
  "Returns the lines of text from rdr as a lazy sequence of strings.
  rdr must implement java.io.BufferedReader."
  {:added "1.0"
   :static true}
  [rdr]
  (let [line (erlang.io.IReader/read_line.e rdr)]
    (when (string? line)
      (cons line (lazy-seq (line-seq rdr))))))

(defn comparator
  "Returns an implementation of java.util.Comparator based upon pred."
  {:added "1.0"
   :static true}
  [pred]
  (fn [x y]
    (cond (pred x y) -1 (pred y x) 1 :else 0)))

(defn sort
  "Returns a sorted sequence of the items in coll. If no comparator is
  supplied, uses compare.  comparator must implement
  java.util.Comparator.  Guaranteed to be stable: equal elements will
  not be reordered.  If coll is a Java array, it will be modified.  To
  avoid this, sort a copy of the array."
  {:added "1.0"
   :static true}
  ([coll]
   (sort compare coll))
  ([comp coll]
   (if (seq coll)
     (lists/sort.e (clj_core/to_list.e coll))
     ())))

(defn sort-by
  "Returns a sorted sequence of the items in coll, where the sort
  order is determined by comparing (keyfn item).  If no comparator is
  supplied, uses compare.  comparator must implement
  java.util.Comparator.  Guaranteed to be stable: equal elements will
  not be reordered.  If coll is a Java array, it will be modified.  To
  avoid this, sort a copy of the array."
  {:added "1.0"
   :static true}
  ([keyfn coll]
   (sort-by keyfn compare coll))
  ([keyfn comp coll]
   (sort (fn [x y] (comp (keyfn x) (keyfn y))) coll)))

(defn dorun
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. dorun can
  be used to force any effects. Walks through the successive nexts of
  the seq, does not retain the head and returns nil."
  {:added "1.0"
   :static true}
  ([coll]
   (when-let [s (seq coll)]
     (recur (next s))))
  ([n coll]
   (when (and (seq coll) (pos? n))
     (recur (dec n) (next coll)))))

(defn doall
  "When lazy sequences are produced via functions that have side
  effects, any effects other than those needed to produce the first
  element in the seq do not occur until the seq is consumed. doall can
  be used to force any effects. Walks through the successive nexts of
  the seq, retains the head and returns it, thus causing the entire
  seq to reside in memory at one time."
  {:added "1.0"
   :static true}
  ([coll]
   (dorun coll)
   coll)
  ([n coll]
   (dorun n coll)
   coll))

(defn nthnext
  "Returns the nth next of coll, (seq coll) when n is 0."
  {:added "1.0"
   :static true}
  [coll n]
    (loop [n n xs (seq coll)]
      (if (and xs (pos? n))
        (recur (dec n) (next xs))
        xs)))

(defn nthrest
  "Returns the nth rest of coll, coll when n is 0."
  {:added "1.3"
   :static true}
  [coll n]
    (loop [n n xs coll]
      (if-let [xs (and (pos? n) (seq xs))]
        (recur (dec n) (rest xs))
        xs)))

(defn partition
  "Returns a lazy sequence of lists of n items each, at offsets step
  apart. If step is not supplied, defaults to n, i.e. the partitions
  do not overlap. If a pad collection is supplied, use its elements as
  necessary to complete last partition upto n items. In case there are
  not enough padding elements, return a partition with less than n items."
  {:added "1.0"
   :static true}
  ([n coll]
     (partition n n coll))
  ([n step coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (let [p (doall (take n s))]
           (when (= n (count p))
             (cons p (partition n step (nthrest s step))))))))
  ([n step pad coll]
     (lazy-seq
       (when-let [s (seq coll)]
         (let [p (doall (take n s))]
           (if (= n (count p))
             (cons p (partition n step pad (nthrest s step)))
             (list (take n (concat p pad)))))))))

;; evaluation

(defn eval
  "Evaluates the form data structure (not text!) and returns the result."
  {:added "1.0"
   :static true}
  [form]
  (first (clj_compiler/eval.e form)))

(defmacro doseq
  "Repeatedly executes body (presumably for side-effects) with
  bindings and filtering as provided by \"for\".  Does not retain
  the head of the sequence. Returns nil."
  {:added "1.0"}
  [seq-exprs & body]
  (assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [step (fn step [recform exprs]
               (if-not exprs
                 [true `(do ~@body)]
                 (let [k (first exprs)
                       v (second exprs)]
                   (if (keyword? k)
                     (let [steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)]
                       (cond
                         (= k :let) [needrec `(let ~v ~subform)]
                         (= k :while) [false `(when ~v
                                                ~subform
                                                ~@(when needrec [recform]))]
                         (= k :when) [false `(if ~v
                                               (do
                                                 ~subform
                                                 ~@(when needrec [recform]))
                                               ~recform)]))
                     (let [seq- (gensym "seq_")
                           chunk- (with-meta (gensym "chunk_")
                                             {:tag 'clojure.lang.IChunk})
                           count- (gensym "count_")
                           i- (gensym "i_")
                           recform `(recur (next ~seq-) nil 0 0)
                           steppair (step recform (nnext exprs))
                           needrec (steppair 0)
                           subform (steppair 1)
                           recform-chunk
                             `(recur ~seq- ~chunk- ~count- (inc ~i-))
                           steppair-chunk (step recform-chunk (nnext exprs))
                           subform-chunk (steppair-chunk 1)]
                       [true
                        `(loop [~seq- (seq ~v), ~chunk- nil,
                                ~count- 0, ~i- 0]
                           (if (< ~i- ~count-)
                             (let [~k (clj_core/nth.e ~chunk- ~i-)]
                               ~subform-chunk
                               ~@(when needrec [recform-chunk]))
                             (when-let [~seq- (seq ~seq-)]
                               (if true #_(chunked-seq? ~seq-)
                                 #_(let [c# (chunk-first ~seq-)]
                                   (recur (chunk-rest ~seq-) c#
                                          (int (count c#)) (int 0)))
                                 (let [~k (first ~seq-)]
                                   ~subform
                                   ~@(when needrec [recform]))))))])))))]
    (nth (step nil (seq seq-exprs)) 1)))


(defn await
  "Blocks the current thread (indefinitely!) until all actions
  dispatched thus far, from this thread or agent, to the agent(s) have
  occurred.  Will block on failed agents.  Will never return if
  a failed agent is restarted with :clear-actions true."
  {:added "1.0"
   :static true}
  [& agents]
  (throw "unimplemented agent")
  #_(io! "await in transaction"
    (when *agent*
      (throw (new Exception "Can't await in agent action")))
    (let [latch (new java.util.concurrent.CountDownLatch (count agents))
          count-down (fn [agent] (. latch (countDown)) agent)]
      (doseq [agent agents]
        (send agent count-down))
      (. latch (await)))))

(defn ^:static await1 [^clojure.lang.Agent a]
  (throw "unimplemented agent")
  #_(when (pos? (.getQueueCount a))
    (await a))
    a)

(defn await-for
  "Blocks the current thread until all actions dispatched thus
  far (from this thread or agent) to the agents have occurred, or the
  timeout (in milliseconds) has elapsed. Returns logical false if
  returning due to timeout, logical true otherwise."
  {:added "1.0"
   :static true}
  [timeout-ms & agents]
  (throw "unimplemented agent")
    #_(io! "await-for in transaction"
     (when *agent*
       (throw (new Exception "Can't await in agent action")))
     (let [latch (new java.util.concurrent.CountDownLatch (count agents))
           count-down (fn [agent] (. latch (countDown)) agent)]
       (doseq [agent agents]
           (send agent count-down))
       (. latch (await  timeout-ms (. java.util.concurrent.TimeUnit MILLISECONDS))))))

(defmacro dotimes
  "bindings => name n

  Repeatedly executes body (presumably for side-effects) with name
  bound to integers from 0 through n-1."
  {:added "1.0"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [i (first bindings)
        n (second bindings)]
    `(let [n# (long ~n)]
       (loop [~i 0]
         (when (< ~i n#)
           ~@body
           (recur (inc ~i)))))))

#_(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  {:added "1.0"}
  [to from]
    (let [ret to items (seq from)]
      (if items
        (recur (conj ret (first items)) (next items))
        ret)))

;;;;;;;;;;;;;;;;;;;;; editable collections ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn transient
  "Returns a new, transient version of the collection, in constant time."
  {:added "1.1"
   :static true}
  [^clojure.lang.IEditableCollection coll]
  (throw "unimplemented transient")
  #_(.asTransient coll))

(defn persistent!
  "Returns a new, persistent version of the transient collection, in
  constant time. The transient collection cannot be used after this
  call, any such use will throw an exception."
  {:added "1.1"
   :static true}
  [^clojure.lang.ITransientCollection coll]
  (throw "unimplemented transient")
  #_(.persistent coll))

(defn conj!
  "Adds x to the transient collection, and return coll. The 'addition'
  may happen at different 'places' depending on the concrete type."
  {:added "1.1"
   :static true}
  ([] (transient []))
  ([coll] coll)
  ([^clojure.lang.ITransientCollection coll x]
   (throw "unimplemented transient")
   #_(.conj coll x)))

(defn assoc!
  "When applied to a transient map, adds mapping of key(s) to
  val(s). When applied to a transient vector, sets the val at index.
  Note - index must be <= (count vector). Returns coll."
  {:added "1.1"
   :static true}
  ([^clojure.lang.ITransientAssociative coll key val]
   (throw "unimplemented transient")
   #_(.assoc coll key val))
  ([^clojure.lang.ITransientAssociative coll key val & kvs]
   (throw "unimplemented transient")
   #_(let [ret (.assoc coll key val)]
       (if kvs
         (recur ret (first kvs) (second kvs) (nnext kvs))
         ret))))

(defn dissoc!
  "Returns a transient map that doesn't contain a mapping for key(s)."
  {:added "1.1"
   :static true}
  ([^clojure.lang.ITransientMap map key]
   (throw "unimplemented transient")
   #_(.without map key))
  ([^clojure.lang.ITransientMap map key & ks]
   (throw "unimplemented transient")
   #_(let [ret (.without map key)]
       (if ks
         (recur ret (first ks) (next ks))
         ret))))

(defn pop!
  "Removes the last item from a transient vector. If
  the collection is empty, throws an exception. Returns coll"
  {:added "1.1"
   :static true}
  [^clojure.lang.ITransientVector coll]
  (throw "unimplemented transient")
  #_(.pop coll))

(defn disj!
  "disj[oin]. Returns a transient set of the same (hashed/sorted) type, that
  does not contain key(s)."
  {:added "1.1"
   :static true}
  ([set] set)
  ([^clojure.lang.ITransientSet set key]
   (throw "unimplemented transient")
   #_(. set (disjoin key)))
  ([^clojure.lang.ITransientSet set key & ks]
   (throw "unimplemented transient")
   #_(let [ret (. set (disjoin key))]
     (if ks
       (recur ret (first ks) (next ks))
       ret))))

;redef into with batch support
(defn ^:private into1
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined."
  {:added "1.0"
   :static true}
  [to from]
  (reduce1 conj to from))

(defmacro import
  "import-list => (package-symbol class-name-symbols*)

  For each name in class-name-symbols, adds a mapping from name to the
  class named by package.name to the current namespace. Use :import in the ns
  macro in preference to calling this directly."
  {:added "1.0"}
  [& import-symbols-or-lists]
  (let [specs (map #(if (and (seq? %) (= 'quote (first %))) (second %) %)
                   import-symbols-or-lists)]
    `(do ~@(map #(list 'clojure.core/import* %)
                (reduce1 (fn [v spec]
                           (if (symbol? spec)
                             (conj v (name spec))
                             (let [p (first spec) cs (rest spec)]
                               (into1 v (map #(str p "." %) cs)))))
                         [] specs)))))

(defn into-tuple
  "Returns a tuple with components set to the values in aseq."
  {:added "1.0"
   :static true}
  ([aseq]
   (-> (seq aseq) clj_core/to_list.1 erlang/list_to_tuple.1))
  ([type aseq]
   (-> (seq aseq) clj_core/to_list.1 erlang/list_to_tuple.1)))

(defn tuple
  "Returns a tuple with items."
  {:added "1.8"}
  [& items]
  (into-tuple items))

(defn class
  "Returns the Class of x"
  {:added "1.0"
   :static true}
  ^clojerl.Keyword [^Object x]
  (if (nil? x) x (clj_core/type.e x)))

(defn type
  "Returns the :type metadata of x, or its Class if none"
  {:added "1.0"
   :static true}
  [x]
  (or (get (meta x) :type) (class x)))

(defn num
  "Coerce to Number"
  {:tag clojerl.Integer
   :inline (fn  [x] `(if (erlang/is_number.e x) x (throw "Not a number")))
   :added "1.0"}
  [x]
  (if (erlang/is_number.e x)
    x
    (throw "Not a number")))

(defn long
  "Coerce to long"
  {:inline (fn  [x] `(erlang/trunc.e ~x))
   :added "1.0"}
  [x]
  (erlang/trunc.e x))

(defn float
  "Coerce to float"
  {:inline (fn  [x] `(erlang/float.e ~x))
   :added "1.0"}
  [x]
  (erlang/float.e x))

(defn double
  "Coerce to double"
  {:inline (fn  [x] `(erlang/float.e ~x))
   :added "1.0"}
  [^Number x]
  (erlang/float.e x))

(defn short
  "Coerce to short"
  {:inline (fn  [x] `(clj_core/short.e x))
   :added "1.0"}
  [^Number x]
  (clj_core/short.e x))

(defn byte
  "Coerce to byte"
  {:inline (fn  [x] `(clj_core/byte.e x))
   :added "1.0"}
  [^Number x]
  (clj_core/byte.e x))

(defn char
  "Coerce to char"
  {:inline (fn  [x] `(clj_core/to_char.e x))
   :added "1.1"}
  [x]
  (clj_core/char.e x))

(defn boolean
  "Coerce to boolean"
  {:inline (fn  [x] `(clj_core/boolean.e ~x))
   :added "1.0"}
  [x] (clj_core/boolean.e x))

(defn number?
  "Returns true if x is a Number"
  {:added "1.0"
   :static true}
  [x]
  (erlang/is_number.e x))

(defn mod
  "Modulus of num and div. Truncates toward negative infinity."
  {:added "1.0"
   :static true}
  [num div]
  (let [m (rem num div)]
    (if (or (zero? m) (= (pos? num) (pos? div)))
      m
      (+ m div))))

(defn float?
  "Returns true if n is a floating point number"
  {:added "1.0"
   :static true}
  [n]
  (erlang/is_float.e n))

(def ^:dynamic ^{:private true} print-initialized false)

(defmulti print-method (fn [x writer]
                         (let [t (get (meta x) :type)]
                           (if (keyword? t) t (class x)))))

(defmulti print-dup (fn [x writer] (class x)))

(defn pr-on
  {:private true
   :static true}
  [x w]
  (if *print-dup*
    (print-dup x w)
    (print-method x w))
  nil)

(defn pr
  "Prints the object(s) to the output stream that is the current value
  of *out*.  Prints the object(s), separated by spaces if there is
  more than one.  By default, pr and prn print in a way that objects
  can be read by the reader"
  {:dynamic true
   :added "1.0"}
  ([] nil)
  ([x]
   (pr-on x *out*))
  ([x & more]
   (pr x)
   (io/put_chars.e *out* " ")
   (if-let [nmore (next more)]
     (recur (first more) nmore)
     (apply pr more))))

(def ^:private ^clojerl.String system-newline
  (let [os-family (first (os/type.e))]
    (if (= os-family :win32) "\r\n" "\n")))

(defn newline
  "Writes a platform-specific newline to *out*"
  {:added "1.0"
   :static true}
  []
  (io/nl.e *out*) nil)

(defn flush
  "Flushes the output stream that is the current value of
  *out*"
  {:added "1.0"
   :static true}
  []
  (io/put_chars.e *out* "") nil)

(defn prn
  "Same as pr followed by (newline). Observes *flush-on-newline*"
  {:added "1.0"
   :static true}
  [& more]
  (apply pr more)
  (newline)
  (when *flush-on-newline*
    (flush)))

(defn print
  "Prints the object(s) to the output stream that is the current value
  of *out*.  print and println produce output for human consumption."
  {:added "1.0"
   :static true}
  [& more]
  (binding [*print-readably* nil]
    (apply pr more)))

(defn println
  "Same as print followed by (newline)"
  {:added "1.0"
   :static true}
  [& more]
  (binding [*print-readably* nil]
    (apply prn more)))

(defn read
  "Reads the next object from stream, which must be an instance of
  java.io.PushbackReader or some derivee.  stream defaults to the
  current value of *in*.

  Opts is a persistent map with valid keys:
    :read-cond - :allow to process reader conditionals, or
                 :preserve to keep all branches
    :features - persistent set of feature keywords for reader conditionals
    :eof - on eof, return value unless :eofthrow, then throw.
           if not specified, will throw

  Note that read can execute code (controlled by *read-eval*),
  and as such should be used only with trusted sources.

  For data structure interop use clojure.edn/read"
  {:added "1.0"
   :static true}
  ([]
   (read *in*))
  ([stream]
   (read stream true nil))
  ([stream eof-error? eof-value]
   (read {:eof (if eof-error? :eofthrow eof-value)}
         stream))
  ([opts stream]
   (clj_reader/read.e ""
                      (clojerl.Map/to_erl_map.e (assoc opts :io-reader stream)))))

(defn read-line
  "Reads the next line from stream that is the current value of *in* ."
  {:added "1.0"
   :static true}
  []
  (erlang.io.IReader/read_line.e *in*))

(defn read-string
  "Reads one object from the string s. Optionally include reader
  options, as specified in read.

  Note that read-string can execute code (controlled by *read-eval*),
  and as such should be used only with trusted sources.

  For data structure interop use clojure.edn/read-string"
  {:added "1.0"
   :static true}
  ([s] (clj_reader/read.e s))
  ([opts s] (clj_reader/read.e s (clojerl.Map/to_erl_map.e opts))))

(defn subvec
  "Returns a persistent vector of the items in vector from
  start (inclusive) to end (exclusive).  If end is not supplied,
  defaults to (count vector). This operation is O(1) and very fast, as
  the resulting vector shares structure with the original and no
  trimming is done."
  {:added "1.0"
   :static true}
  ([v start]
   (subvec v start (count v)))
  ([v start end]
   (clj_core/subvec.e v start end)))

(defmacro with-open
  "bindings => [name init ...]

  Evaluates body in a try expression with names bound to the values
  of the inits, and a finally clause that calls (.close name) on each
  name in reverse order."
  {:added "1.0"}
  [bindings & body]
  (assert-args
     (vector? bindings) "a vector for its binding"
     (even? (count bindings)) "an even number of forms in binding vector")
  (cond
    (= (count bindings) 0) `(do ~@body)
    (symbol? (bindings 0)) `(let ~(subvec bindings 0 2)
                              (try
                                (with-open ~(subvec bindings 2) ~@body)
                                (finally
                                  (erlang.io.Closeable/close.e ~(bindings 0)))))
    :else (throw "with-open only allows Symbols in bindings")))

(defmacro doto
  "Evaluates x then calls all of the methods and functions with the
  value of x supplied at the front of the given arguments.  The forms
  are evaluated in order.  Returns x.

  (doto (new java.util.HashMap) (.put \"a\" 1) (.put \"b\" 2))"
  {:added "1.0"}
  [x & forms]
    (let [gx (gensym)]
      `(let [~gx ~x]
         ~@(map (fn [f]
                  (if (seq? f)
                    `(~(first f) ~gx ~@(next f))
                    `(~f ~gx)))
                forms)
         ~gx)))

(defmacro memfn
  "Expands into code that creates a fn that expects to be passed an
  object and any args and calls the named instance method on the
  object passing the args. Use when you want to treat a Java method as
  a first-class fn. name may be type-hinted with the method receiver's
  type in order to avoid reflective calls."
  {:added "1.0"}
  [name & args]
  (let [t (with-meta (gensym "target")
            (meta name))]
    `(fn [~t ~@args]
       (. ~t (~name ~@args)))))

(defmacro time
  "Evaluates expr and prints the time it took.  Returns the value of
 expr."
  {:added "1.0"}
  [expr]
  `(let [start# (erlang/monotonic_time.e :nano_seconds)
         ret#   ~expr
         stop#  (erlang/monotonic_time.e :nano_seconds)]
     (prn (str "Elapsed time: "
               (/ (- stop# start#) 1000000.0)
               " msecs"))
     ret#))


#_(
   (import '(java.lang.reflect Array))

  (defn alength
    "Returns the length of the Java array. Works on arrays of all
  types."
    {:inline (fn [a] `(. clojure.lang.RT (alength ~a)))
     :added "1.0"}
    [array] (. clojure.lang.RT (alength array)))

  (defn aclone
    "Returns a clone of the Java array. Works on arrays of known
  types."
    {:inline (fn [a] `(. clojure.lang.RT (aclone ~a)))
     :added "1.0"}
    [array] (. clojure.lang.RT (aclone array)))

  (defn aget
    "Returns the value at the index/indices. Works on Java arrays of all
  types."
    {:inline (fn [a i] `(. clojure.lang.RT (aget ~a (int ~i))))
     :inline-arities #{2}
     :added "1.0"}
    ([array idx]
     (clojure.lang.Reflector/prepRet (.getComponentType (class array)) (. Array (get array idx))))
    ([array idx & idxs]
     (apply aget (aget array idx) idxs)))

  (defn aset
    "Sets the value at the index/indices. Works on Java arrays of
  reference types. Returns val."
    {:inline (fn [a i v] `(. clojure.lang.RT (aset ~a (int ~i) ~v)))
     :inline-arities #{3}
     :added "1.0"}
    ([array idx val]
     (. Array (set array idx val))
     val)
    ([array idx idx2 & idxv]
     (apply aset (aget array idx) idx2 idxv)))

  (defmacro
    ^{:private true}
    def-aset [name method coerce]
    `(defn ~name
       {:arglists '([~'array ~'idx ~'val] [~'array ~'idx ~'idx2 & ~'idxv])}
       ([array# idx# val#]
        (. Array (~method array# idx# (~coerce val#)))
        val#)
       ([array# idx# idx2# & idxv#]
        (apply ~name (aget array# idx#) idx2# idxv#))))

  (def-aset
    ^{:doc "Sets the value at the index/indices. Works on arrays of int. Returns val."
      :added "1.0"}
    aset-int setInt int)

  (def-aset
    ^{:doc "Sets the value at the index/indices. Works on arrays of long. Returns val."
      :added "1.0"}
    aset-long setLong long)

  (def-aset
    ^{:doc "Sets the value at the index/indices. Works on arrays of boolean. Returns val."
      :added "1.0"}
    aset-boolean setBoolean boolean)

  (def-aset
    ^{:doc "Sets the value at the index/indices. Works on arrays of float. Returns val."
      :added "1.0"}
    aset-float setFloat float)

  (def-aset
    ^{:doc "Sets the value at the index/indices. Works on arrays of double. Returns val."
      :added "1.0"}
    aset-double setDouble double)

  (def-aset
    ^{:doc "Sets the value at the index/indices. Works on arrays of short. Returns val."
      :added "1.0"}
    aset-short setShort short)

  (def-aset
    ^{:doc "Sets the value at the index/indices. Works on arrays of byte. Returns val."
      :added "1.0"}
    aset-byte setByte byte)

  (def-aset
    ^{:doc "Sets the value at the index/indices. Works on arrays of char. Returns val."
      :added "1.0"}
    aset-char setChar char)

  (defn make-array
    "Creates and returns an array of instances of the specified class of
  the specified dimension(s).  Note that a class object is required.
  Class objects can be obtained by using their imported or
  fully-qualified name.  Class objects for the primitive types can be
  obtained using, e.g., Integer/TYPE."
    {:added "1.0"
     :static true}
    ([^Class type len]
     (. Array (newInstance type (int len))))
    ([^Class type dim & more-dims]
     (let [dims (cons dim more-dims)
           ^"[I" dimarray (make-array (. Integer TYPE)  (count dims))]
       (dotimes [i (alength dimarray)]
         (aset-int dimarray i (nth dims i)))
       (. Array (newInstance type dimarray)))))

  (defn to-array-2d
    "Returns a (potentially-ragged) 2-dimensional array of Objects
  containing the contents of coll, which can be any Collection of any
  Collection."
    {:tag "[[Ljava.lang.Object;"
     :added "1.0"
     :static true}
    [^java.util.Collection coll]
    (let [ret (make-array (. Class (forName "[Ljava.lang.Object;")) (. coll (size)))]
      (loop [i 0 xs (seq coll)]
        (when xs
          (aset ret i (to-array (first xs)))
          (recur (inc i) (next xs))))
      ret)))

(defn macroexpand-1
  "If form represents a macro form, returns its expansion,
  else returns form."
  {:added "1.0"
   :static true}
  [form]
  (clj_analyzer/macroexpand_1.e nil form))

(defn macroexpand
  "Repeatedly calls macroexpand-1 on form until it no longer
  represents a macro form, then returns it.  Note neither
  macroexpand-1 nor macroexpand expand macros in subforms."
  {:added "1.0"
   :static true}
  [form]
    (let [ex (macroexpand-1 form)]
      (if (identical? ex form)
        form
        (macroexpand ex))))

(defn load-reader
  "Sequentially read and evaluate the set of forms contained in the
  stream/file"
  {:added "1.0"
   :static true}
  [rdr]
  (throw "unimplemented reader")
  #_(. clojure.lang.Compiler (load rdr)))

(defn load-string
  "Sequentially read and evaluate the set of forms contained in the
  string"
  {:added "1.0"
   :static true}
  [s]
  (clj_compiler/compile.e str))

(defn set?
  "Returns true if x implements IPersistentSet"
  {:added "1.0"
   :static true}
  [x] (satisfies? clojerl.ISet x))

(defn set
  "Returns a set of the distinct elements of coll."
  {:added "1.0"
   :static true}
  [coll]
  (if (set? coll)
    (with-meta coll nil)
    (clj_core/hash_set.e (clj_core/to_list.e coll))))

(defn ^{:private true
        :static true}
  filter-key [keyfn pred amap]
    (loop [ret {} es (seq amap)]
      (if es
        (if (pred (keyfn (first es)))
          (recur (assoc ret (key (first es)) (val (first es))) (next es))
          (recur ret (next es)))
        ret)))

(defn find-ns
  "Returns the namespace named by the symbol or nil if it doesn't exist."
  {:added "1.0"
   :static true}
  [sym] (clj_namespace/find.e sym))

(defn create-ns
  "Create a new namespace named by the symbol if one doesn't already
  exist, returns it or the already-existing namespace of the same
  name."
  {:added "1.0"
   :static true}
  [sym]
  (clj_namespace/find_or_create.e sym))

(defn remove-ns
  "Removes the namespace named by the symbol. Use with caution.
  Cannot be used to remove the clojure namespace."
  {:added "1.0"
   :static true}
  [sym]
  (clj_namespace/remove.e sym))

(defn all-ns
  "Returns a sequence of all namespaces."
  {:added "1.0"
   :static true}
  [] (clj_namespace/all.e))

(defn the-ns
  "If passed a namespace, returns it. Else, when passed a symbol,
  returns the namespace named by it, throwing an exception if not
  found."
  {:added "1.0"
   :static true}
  [x]
  (if-not (symbol? x)
    x
    (or (find-ns x) (throw (str "No namespace: " x " found")))))

(defn ns-name
  "Returns the name of the namespace, a symbol."
  {:added "1.0"
   :static true}
  [ns]
  (clj_namespace/name.e (the-ns ns)))

(defn ns-map
  "Returns a map of all the mappings for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (clj_namespace/get_mappings.e (the-ns ns)))

(defn ns-unmap
  "Removes the mappings for the symbol from the namespace."
  {:added "1.0"
   :static true}
  [ns sym]
  (clj_namespace/unmap.e (the-ns ns) sym))

;(defn export [syms]
;  (doseq [sym syms]
;   (.. *ns* (intern sym) (setExported true))))

(defn ns-publics
  "Returns a map of the public intern mappings for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (let [ns (the-ns ns)]
    (filter-key val (fn [v] (and (instance? clojerl.Var v)
                                (identical? ns (find-ns (symbol (namespace v))))
                                (clojerl.Var/is_public.e v)))
                (ns-map ns))))

(defn ns-imports
  "Returns a map of the import mappings for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (throw "unsupported import"))

(defn ns-interns
  "Returns a map of the intern mappings for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (let [ns (the-ns ns)]
    (filter-key val (fn [v]
                      (and (instance? clojerl.Var v)
                           (= ns (find-ns (symbol (namespace v))))))
                (ns-map ns))))

(defn refer
  "refers to all public vars of ns, subject to filters.
  filters can include at most one each of:

  :exclude list-of-symbols
  :only list-of-symbols
  :rename map-of-fromsymbol-tosymbol

  For each public interned var in the namespace named by the symbol,
  adds a mapping from the name of the var to the var to the current
  namespace.  Throws an exception if name is already mapped to
  something else in the current namespace. Filters can be used to
  select a subset, via inclusion or exclusion, or to provide a mapping
  to a symbol different from the var's name, in order to prevent
  clashes. Use :use in the ns macro in preference to calling this directly."
  {:added "1.0"}
  [ns-sym & filters]
  (let [ns (or (find-ns ns-sym) (throw (str "No namespace: " ns-sym)))
        fs (apply hash-map filters)
        nspublics (ns-publics ns)
        rename (or (:rename fs) {})
        exclude (set (:exclude fs))
        to-do (if (= :all (:refer fs))
                (keys nspublics)
                (or (:refer fs) (:only fs) (keys nspublics)))]
    (when (and to-do (not (satisfies? clojerl.ISequential to-do)))
      (throw ":only/:refer value must be a sequential collection of symbols"))
    ;; keys in maps are strings to avoid be able to use the underlying map
    ;; implementation. We convert all to-do's to strings since some filters
    ;; could contains symbols
    (doseq [name (map name to-do)]
      (when-not (exclude name)
        (let [v   (nspublics name)
              sym (symbol name)]
          (when-not v
            (throw (if (get (ns-interns ns) name)
                     (str name " is not public")
                     (str name " does not exist"))))
          (clj_namespace/refer.e *ns* (or (rename sym) sym) v))))))

(defn ns-refers
  "Returns a map of the refer mappings for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (let [ns (the-ns ns)]
    (filter-key val (fn [v] (and (instance? clojerl.Var v)
                                (not= ns (find-ns (symbol (namespace v))))))
                (ns-map ns))))

(defn alias
  "Add an alias in the current namespace to another
  namespace. Arguments are two symbols: the alias to be used, and
  the symbolic name of the target namespace. Use :as in the ns macro in preference
  to calling this directly."
  {:added "1.0"
   :static true}
  [alias namespace-sym]
  (clj_namespace/add_alias.e *ns* alias (the-ns namespace-sym)))

(defn ns-aliases
  "Returns a map of the aliases for the namespace."
  {:added "1.0"
   :static true}
  [ns]
  (clj_namespace/get_aliases.e (the-ns ns)))

(defn ns-unalias
  "Removes the alias for the symbol from the namespace."
  {:added "1.0"
   :static true}
  [ns sym]
  (clj_namespace/remove_alias.e (the-ns ns) sym))

(defn take-nth
  "Returns a lazy seq of every nth item in coll.  Returns a stateful
  transducer when no collection is provided."
  {:added "1.0"
   :static true}
  #_([n]
     (fn [rf]
       (let [iv (volatile! -1)]
         (fn
           ([] (rf))
           ([result] (rf result))
           ([result input]
              (let [i (vswap! iv inc)]
                (if (zero? (rem i n))
                  (rf result input)
                  result)))))))
  ([n coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (cons (first s) (take-nth n (drop n s)))))))

(defn interleave
  "Returns a lazy seq of the first item in each coll, then the second etc."
  {:added "1.0"
   :static true}
  ([] ())
  ([c1] (lazy-seq c1))
  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (when (and s1 s2)
          (cons (first s1) (cons (first s2)
                                 (interleave (rest s1) (rest s2))))))))
  ([c1 c2 & colls]
     (lazy-seq
      (let [ss (map seq (conj colls c2 c1))]
        (when (every? identity ss)
          (concat (map first ss) (apply interleave (map rest ss))))))))

(defn var-get
  "Gets the value in the var object"
  {:added "1.0"
   :static true}
  [x] (clojerl.Var/get.e x))

(defn var-set
  "Sets the value in the var object to val. The var must be
  thread-locally bound."
  {:added "1.0"
   :static true}
  [x val] (clojerl.Var/dynamic_binding.e x val))

(defmacro with-local-vars
  "varbinding=> symbol init-expr

  Executes the exprs in a context in which the symbols are bound to
  vars with per-thread bindings to the init-exprs.  The symbols refer
  to the var objects themselves, and must be accessed with var-get and
  var-set"
  {:added "1.0"}
  [name-vals-vec & body]
  (assert-args
   (vector? name-vals-vec) "a vector for its binding"
   (even? (count name-vals-vec)) "an even number of forms in binding vector")
  `(let [~@(interleave (take-nth 2 name-vals-vec)
                       (repeat '(.. clojure.lang.Var create setDynamic)))]
     (clojerl.Var/push_bindings.e (hash-map ~@name-vals-vec))
     (try
       ~@body
       (finally (clojerl.Var/pop_bindings.e)))))

(defn ns-resolve
  "Returns the var or Class to which a symbol will be resolved in the
  namespace (unless found in the environment), else nil.  Note that
  if the symbol is fully qualified, the var/Class to which it resolves
  need not be present in the namespace."
  {:added "1.0"
   :static true}
  ([ns sym]
   (ns-resolve ns nil sym))
  ([ns env sym]
   (when-not (contains? env sym)
     (clj_namespace/find_var.e (the-ns ns) sym))))

(defn resolve
  "same as (ns-resolve *ns* symbol) or (ns-resolve *ns* &env symbol)"
  {:added "1.0"
   :static true}
  ([sym] (ns-resolve *ns* sym))
  ([env sym] (ns-resolve *ns* env sym)))

#_((defn array-map
     "Constructs an array-map. If any keys are equal, they are handled as
  if by repeated uses of assoc."
     {:added "1.0"
      :static true}
     ([] (. clojure.lang.PersistentArrayMap EMPTY))
     ([& keyvals]
      (clojure.lang.PersistentArrayMap/createAsIfByAssoc (to-array keyvals)))))

;redefine let and loop  with destructuring
(defn destructure [bindings]
  (let [bents (partition 2 bindings)
        pb (fn pb [bvec b v]
             (let [pvec
                   (fn [bvec b val]
                     (let [gvec (gensym "vec__")]
                       (loop [ret (-> bvec (conj gvec) (conj val))
                              n 0
                              bs b
                              seen-rest? false]
                         (if (seq bs)
                           (let [firstb (first bs)]
                             (cond
                               (= firstb '&) (recur (pb ret (second bs) (list `nthnext gvec n))
                                                    n
                                                    (nnext bs)
                                                    true)
                               (= firstb :as) (pb ret (second bs) gvec)
                               :else (if seen-rest?
                                       (throw "Unsupported binding form, only :as can follow & parameter")
                                       (recur (pb ret firstb  (list `nth gvec n nil))
                                              (inc n)
                                              (next bs)
                                              seen-rest?))))
                           ret))))
                   pmap
                   (fn [bvec b v]
                     (let [gmap (gensym "map__")
                           gmapseq (with-meta gmap {:tag 'clojerl.ISeq})
                           defaults (:or b)]
                       (loop [ret (-> bvec (conj gmap) (conj v)
                                      (conj gmap) (conj `(if (seq? ~gmap)
                                                           (new clojerl.Map (clj_core/to_list.e ~gmapseq))
                                                           ~gmap))
                                      ((fn [ret]
                                         (if (:as b)
                                           (conj ret (:as b) gmap)
                                           ret))))
                              bes (reduce1
                                   (fn [bes entry]
                                     (reduce1 #(assoc %1 %2 ((val entry) %2))
                                              (dissoc bes (key entry))
                                              ((key entry) bes)))
                                   (dissoc b :as :or)
                                   {:keys #(if (keyword? %) % (keyword (str %))),
                                    :strs str, :syms #(list `quote %)})]
                         (if (seq bes)
                           (let [bb (key (first bes))
                                 bk (val (first bes))
                                 bv (if (contains? defaults bb)
                                      (list `get gmap bk (defaults bb))
                                      (list `get gmap bk))]
                             (recur (cond
                                      (symbol? bb) (-> ret (conj (if (namespace bb) (symbol (name bb)) bb)) (conj bv))
                                      (keyword? bb) (-> ret (conj (symbol (name bb)) bv))
                                      :else (pb ret bb bv))
                                    (next bes)))
                           ret))))]
               (cond
                 (symbol? b) (-> bvec (conj b) (conj v))
                 (vector? b) (pvec bvec b v)
                 (map? b) (pmap bvec b v)
                 :else (throw (str "Unsupported binding form: " b)))))
        process-entry (fn [bvec b] (pb bvec (first b) (second b)))]
    (if (every? symbol? (map first bents))
      bindings
      (reduce1 process-entry [] bents))))

(defmacro let
  "binding => binding-form init-expr

  Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein."
  {:added "1.0", :special-form true, :forms '[(let [bindings*] exprs*)]}
  [bindings & body]
  (assert-args
   (vector? bindings) "a vector for its binding"
   (even? (count bindings)) "an even number of forms in binding vector")
  `(let* ~(destructure bindings) ~@body))

(defn ^{:private true}
  maybe-destructured
  [params body]
  (if (every? symbol? params)
    (cons params body)
    (loop [params params
           new-params (with-meta [] (meta params))
           lets []]
      (if params
        (if (symbol? (first params))
          (recur (next params) (conj new-params (first params)) lets)
          (let [gparam (gensym "p__")]
            (recur (next params) (conj new-params gparam)
                   (-> lets (conj (first params)) (conj gparam)))))
        `(~new-params
          (let ~lets
            ~@body))))))

;redefine fn with destructuring and pre/post conditions
(defmacro fn
  "params => positional-params* , or positional-params* & next-param
  positional-param => binding-form
  next-param => binding-form
  name => symbol

  Defines a function"
  {:added "1.0", :special-form true,
   :forms '[(fn name? [params* ] exprs*) (fn name? ([params* ] exprs*)+)]}
  [& sigs]
  (let [name (if (symbol? (first sigs)) (first sigs) nil)
        sigs (if name (next sigs) sigs)
        sigs (if (vector? (first sigs))
               (list sigs)
               (if (seq? (first sigs))
                 sigs
                 ;; Assume single arity syntax
                 (throw (if (seq sigs)
                          (str "Parameter declaration "
                               (first sigs)
                               " should be a vector")
                          (str "Parameter declaration missing")))))
        psig (fn* [sig]
                  ;; Ensure correct type before destructuring sig
                  (when (not (seq? sig))
                    (throw (str "Invalid signature \"" sig
                                "\" should be a list")))
                  (let [[params & body] sig
                        _ (when (not (vector? params))
                            (throw (if (seq? (first sigs))
                                     (str "Parameter declaration " params
                                          " should be a vector")
                                     (str "Invalid signature \"" sig
                                          "\" should be a list"))))
                        conds (when (and (next body) (map? (first body)))
                                (first body))
                        body (if conds (next body) body)
                        conds (or conds (meta params))
                        pre (:pre conds)
                        post (:post conds)
                        body (if post
                               `((let [~'% ~(if (< 1 (count body))
                                              `(do ~@body)
                                              (first body))]
                                   ~@(map (fn* [c] `(assert ~c)) post)
                                   ~'%))
                               body)
                        body (if pre
                               (concat (map (fn* [c] `(assert ~c)) pre)
                                       body)
                               body)]
                    (maybe-destructured params body)))
        new-sigs (map psig sigs)]
    (with-meta
      (if name
        (list* 'fn* name new-sigs)
        (cons 'fn* new-sigs))
      (meta &form))))

(defmacro loop
  "Evaluates the exprs in a lexical context in which the symbols in
  the binding-forms are bound to their respective init-exprs or parts
  therein. Acts as a recur target."
  {:added "1.0", :special-form true, :forms '[(loop [bindings*] exprs*)]}
  [bindings & body]
  (assert-args
   (vector? bindings) "a vector for its binding"
   (even? (count bindings)) "an even number of forms in binding vector")
  (let [db (destructure bindings)]
    (if (= db bindings)
      `(loop* ~bindings ~@body)
      (let [vs (take-nth 2 (drop 1 bindings))
            bs (take-nth 2 bindings)
            gs (vec (map (fn [b] (if (symbol? b) b (gensym))) bs))
            bfs (reduce1 (fn [ret [b v g]]
                           (if (symbol? b)
                             (conj ret g v)
                             (conj ret g v b g)))
                         [] (map vector bs vs gs))]
        `(let ~bfs
           (loop* ~(vec (interleave gs gs))
                  (let ~(vec (interleave bs gs))
                    ~@body)))))))

(defmacro when-first
  "bindings => x xs

  Roughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once"
  {:added "1.0"}
  [bindings & body]
  (assert-args
   (vector? bindings) "a vector for its binding"
   (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (let [[x xs] bindings]
    `(when-let [xs# (seq ~xs)]
       (let [~x (first xs#)]
         ~@body))))

(defmacro lazy-cat
  "Expands to code which yields a lazy sequence of the concatenation
  of the supplied colls.  Each coll expr is not evaluated until it is
  needed.

  (lazy-cat xs ys zs) === (concat (lazy-seq xs) (lazy-seq ys) (lazy-seq zs))"
  {:added "1.0"}
  [& colls]
  `(concat ~@(map #(list `lazy-seq %) colls)))

(defmacro for
  "List comprehension. Takes a vector of one or more
   binding-form/collection-expr pairs, each followed by zero or more
   modifiers, and yields a lazy sequence of evaluations of expr.
   Collections are iterated in a nested fashion, rightmost fastest,
   and nested coll-exprs can refer to bindings created in prior
   binding-forms.  Supported modifiers are: :let [binding-form expr ...],
   :while test, :when test.

  (take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))"
  {:added "1.0"}
  [seq-exprs body-expr]
  (assert-args
     (vector? seq-exprs) "a vector for its binding"
     (even? (count seq-exprs)) "an even number of forms in binding vector")
  (let [to-groups (fn [seq-exprs]
                    (reduce1 (fn [groups [k v]]
                              (if (keyword? k)
                                (conj (pop groups) (conj (peek groups) [k v]))
                                (conj groups [k v])))
                            [] (partition 2 seq-exprs)))
        err (fn [& msg] (throw (apply str msg)))
        emit-bind (fn emit-bind [[[bind expr & mod-pairs]
                                  & [[_ next-expr] :as next-groups]]]
                    (let [giter (gensym "iter__")
                          gxs (gensym "s__")
                          do-mod (fn do-mod [[[k v :as pair] & etc]]
                                   (cond
                                     (= k :let) `(let ~v ~(do-mod etc))
                                     (= k :while) `(when ~v ~(do-mod etc))
                                     (= k :when) `(if ~v
                                                    ~(do-mod etc)
                                                    (recur (rest ~gxs)))
                                     (keyword? k) (err "Invalid 'for' keyword " k)
                                     next-groups
                                      `(let [iterys# ~(emit-bind next-groups)
                                             fs# (seq (iterys# ~next-expr))]
                                         (if fs#
                                           (concat fs# (~giter (rest ~gxs)))
                                           (recur (rest ~gxs))))
                                     :else `(cons ~body-expr
                                                  (~giter (rest ~gxs)))))]
                      (if next-groups
                        #_"not the inner-most loop"
                        `(fn ~giter [~gxs]
                           (lazy-seq
                             (loop [~gxs ~gxs]
                               (when-first [~bind ~gxs]
                                 ~(do-mod mod-pairs)))))
                        #_"inner-most loop"
                        (let [gi (gensym "i__")
                              gb (gensym "b__")
                              do-cmod (fn do-cmod [[[k v :as pair] & etc]]
                                        (cond
                                          (= k :let) `(let ~v ~(do-cmod etc))
                                          (= k :while) `(when ~v ~(do-cmod etc))
                                          (= k :when) `(if ~v
                                                         ~(do-cmod etc)
                                                         (recur
                                                           (inc ~gi)))
                                          (keyword? k)
                                            (err "Invalid 'for' keyword " k)
                                          :else
                                            `(do (chunk-append ~gb ~body-expr)
                                                 (recur (inc ~gi)))))]
                          `(fn ~giter [~gxs]
                             (lazy-seq
                               (loop [~gxs ~gxs]
                                 (when-let [~gxs (seq ~gxs)]
                                   #_(if false (chunked-seq? ~gxs)
                                     (let [c# (chunk-first ~gxs)
                                           size# (int (count c#))
                                           ~gb (chunk-buffer size#)]
                                       (if (loop [~gi (int 0)]
                                             (if (< ~gi size#)
                                               (let [~bind (clj_core/nth.e c# ~gi)]
                                                 ~(do-cmod mod-pairs))
                                               true))
                                         (chunk-cons
                                           (chunk ~gb)
                                           (~giter (chunk-rest ~gxs)))
                                         (chunk-cons (chunk ~gb) nil))))
                                   (let [~bind (first ~gxs)]
                                     ~(do-mod mod-pairs))))))))))]
    `(let [iter# ~(emit-bind (to-groups seq-exprs))]
        (iter# ~(second seq-exprs)))))

(defmacro comment
  "Ignores body, yields nil"
  {:added "1.0"}
  [& body])

(defmacro with-out-str
  "Evaluates exprs in a context in which *out* is bound to a fresh
  StringWriter.  Returns the string created by any nested printing
  calls."
  {:added "1.0"}
  [& body]
  `(with-open [s# (new erlang.io.StringWriter)]
     (binding [*out* s#]
       ~@body
       (str s#))))

(defmacro with-in-str
  "Evaluates body in a context in which *in* is bound to a fresh
  StringReader initialized with the string s."
  {:added "1.0"}
  [s & body]
  `(with-open [s# (new erlang.io.StringReader ~s)]
     (binding [*in* s#]
       ~@body)))

(defn pr-str
  "pr to a string, returning it"
  {:tag clojerl.String
   :added "1.0"
   :static true}
  [& xs]
  (with-out-str
    (apply pr xs)))

(defn prn-str
  "prn to a string, returning it"
  {:tag clojerl.String
   :added "1.0"
   :static true}
  [& xs]
  (with-out-str
   (apply prn xs)))

(defn print-str
  "print to a string, returning it"
  {:tag clojerl.String
   :added "1.0"
   :static true}
  [& xs]
  (with-out-str
    (apply print xs)))

(defn println-str
  "println to a string, returning it"
  {:tag clojerl.String
   :added "1.0"
   :static true}
  [& xs]
  (with-out-str
    (apply println xs)))

#_(import clojure.lang.ExceptionInfo clojure.lang.IExceptionInfo)
(defn ex-info
  "Create an instance of ExceptionInfo, a RuntimeException subclass
   that carries a map of additional data."
  {:added "1.4"}
  ([msg map]
   (throw "unimplemented ex-info")
   #_(ExceptionInfo. msg map))
  ([msg map cause]
   (throw "unimplemented ex-info")
   #_(ExceptionInfo. msg map cause)))

(defn ex-data
  "Returns exception data (a map) if ex is an IExceptionInfo.
   Otherwise returns nil."
  {:added "1.4"}
  [ex]
  (throw "unimplemented ex-info")
  #_(when (instance? IExceptionInfo ex)
    (.getData ^IExceptionInfo ex)))

(defmacro assert
  "Evaluates expr and throws an exception if it does not evaluate to
  logical true."
  {:added "1.0"}
  ([x]
   (when *assert*
     `(when-not ~x
        (throw (str "Assert failed: " (pr-str '~x))))))
  ([x message]
   (when *assert*
     `(when-not ~x
        (throw (str "Assert failed: " ~message "\n" (pr-str '~x)))))))

(defn test
  "test [v] finds fn at key :test in var metadata and calls it,
  presuming failure will throw exception"
  {:added "1.0"}
  [v]
    (let [f (:test (meta v))]
      (if f
        (do (f) :ok)
        :no-test)))

(defn rand
  "Returns a random floating point number between 0 (inclusive) and
  n (default 1) (exclusive)."
  {:added "1.0"
   :static true}
  ([] (rand/uniform.e))
  ([n] (* n (rand))))

(defn rand-int
  "Returns a random integer between 0 (inclusive) and n (exclusive)."
  {:added "1.0"
   :static true}
  [n] (int (rand n)))

(defmacro defn-
  "same as defn, yielding non-public def"
  {:added "1.0"}
  [name & decls]
    (list* `defn (with-meta name (assoc (meta name) :private true)) decls))

(defn tree-seq
  "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree."
  {:added "1.0"
   :static true}
   [branch? children root]
   (let [walk (fn walk [node]
                (lazy-seq
                 (cons node
                  (when (branch? node)
                    (mapcat walk (children node))))))]
     (walk root)))

(defn file-seq
  "A tree seq on all files in a directory structure"
  {:added "1.0"
   :static true}
  [dir]
  (tree-seq
   (fn [^erlang.io.File f] (filelib/is_dir.e f))
   (fn [^erlang.io.File d] (->> (file/list_dir.e d)
                             second
                             (map #(filename/join.e d %))
                             seq))
   dir))

(defn xml-seq
  "A tree seq on the xml elements as per xml/parse"
  {:added "1.0"
   :static true}
  [root]
    (tree-seq
     (complement string?)
     (comp seq :content)
     root))

(defn special-symbol?
  "Returns true if s names a special form"
  {:added "1.0"
   :static true}
  [s]
    (clj_analyzer/is_special.e s))

(defn var?
  "Returns true if v is of type clojure.lang.Var"
  {:added "1.0"
   :static true}
  [v] (instance? clojerl.Var v))

(defn subs
  "Returns the substring of s beginning at start inclusive, and ending
  at end (defaults to length of string), exclusive."
  {:added "1.0"
   :static true}
  ([s start] (subs s start (count s)))
  ([s start end] (clojerl.String/substring.e s start end)))

(defn regex? [x]
  (clj_core/regex?.e x))

(defn re-run
  "Runs the matching of the pattern over the string using the provided
  options."
  {:added "1.0"
   :static true}
  [re s & opts]
  (let [opts (clj_core/to_list.e opts)
        res  (erlang.util.Regex/run.e re s opts)]
    (when (and (tuple? res) (= (first res) :match))
      (vec (second res)))))

(defn re-pattern
  "Returns a compiled Erlang regular expression unless s is already a
  compiled pattern."
  {:tag :erlang.util.Regex
   :added "1.0"
   :static true}
  [s]
  (if (regex? s)
    s
    (new erlang.util.Regex s)))

(defn re-find
  "Returns the next regex match, if any, of string to pattern.
  Uses re-groups to return the groups."
  {:added "1.0"
   :static true}
  [re s]
  (let [matches (re-run re s #[:capture :all :binary])]
    (if (= (count matches) 1)
      (first matches)
      matches)))

(defn re-seq
  "Returns a lazy sequence of all matches of pattern in string."
  {:added "1.0"
   :static true}
  [re s]
  (let [match-data (re-find re s)
        [idx len]  (first (re-run re s #[:capture :first :index]))
        match-str  (if (seq? match-data) (first match-data) match-data)
        len        (if (and (zero? len) (not (seq s))) 1 len)
        post-match (when idx (subs s (+ idx len)))]
    (when idx
      (lazy-seq (cons match-data
                      (when (seq post-match)
                        (re-seq re post-match)))))))

(defn re-matches
  "Returns the result of (re-find re s) if re fully matches s."
  {:added "1.0"
   :static true}
  [re s]
  (if (string? s)
    (let [matches (re-find re s)
          match   (if (string? matches) matches (first matches))]
      (when (= match s)
        (if (== (count matches) 1)
          (first matches)
          matches)))
    (throw "re-matches must match against a string.")))

(defn max-key
  "Returns the x for which (k x), a number, is greatest."
  {:added "1.0"
   :static true}
  ([k x] x)
  ([k x y] (if (> (k x) (k y)) x y))
  ([k x y & more]
   (reduce1 #(max-key k %1 %2) (max-key k x y) more)))

(defn min-key
  "Returns the x for which (k x), a number, is least."
  {:added "1.0"
   :static true}
  ([k x] x)
  ([k x y] (if (< (k x) (k y)) x y))
  ([k x y & more]
   (reduce1 #(min-key k %1 %2) (min-key k x y) more)))

(defn distinct
  "Returns a lazy sequence of the elements of coll with duplicates removed.
  Returns a stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  #_([]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (contains? @seen input)
            result
            (do (vswap! seen conj input)
                (rf result input))))))))
  ([coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[f :as xs] seen]
                     (when-let [s (seq xs)]
                       (if (contains? seen f)
                         (recur (rest s) seen)
                         (cons f (step (rest s) (conj seen f))))))
                   xs seen)))]
     (step coll #{}))))

(defn replace
  "Given a map of replacement pairs and a vector/collection, returns a
  vector/seq with any elements = a key in smap replaced with the
  corresponding val in smap.  Returns a transducer when no collection
  is provided."
  {:added "1.0"
   :static true}
  ([smap]
     (map #(if-let [e (find smap %)] (val e) %)))
  ([smap coll]
     (if (vector? coll)
       (reduce1 (fn [v i]
                  (if-let [e (find smap (nth v i))]
                    (assoc v i (val e))
                    v))
                coll (range (count coll)))
       (map #(if-let [e (find smap %)] (val e) %) coll))))

(defmacro dosync
  "Runs the exprs (in an implicit do) in a transaction that encompasses
  exprs and any nested calls.  Starts a transaction if none is already
  running on this thread. Any uncaught exception will abort the
  transaction and flow out of dosync. The exprs may be run more than
  once, but any effects on Refs will be atomic."
  {:added "1.0"}
  [& exprs]
  (throw "unsupported refs")
  #_`(sync nil ~@exprs))

#_ ((defmacro with-precision
      "Sets the precision and rounding mode to be used for BigDecimal operations.

  Usage: (with-precision 10 (/ 1M 3))
  or:    (with-precision 10 :rounding HALF_DOWN (/ 1M 3))

  The rounding mode is one of CEILING, FLOOR, HALF_UP, HALF_DOWN,
  HALF_EVEN, UP, DOWN and UNNECESSARY; it defaults to HALF_UP."
      {:added "1.0"}
      [precision & exprs]
      (let [[body rm] (if (= (first exprs) :rounding)
                        [(next (next exprs))
                         `((. java.math.RoundingMode ~(second exprs)))]
                        [exprs nil])]
        `(binding [*math-context* (java.math.MathContext. ~precision ~@rm)]
           ~@body)))

    (defn mk-bound-fn
      {:private true}
      [^clojure.lang.Sorted sc test key]
      (fn [e]
        (test (.. sc comparator (compare (. sc entryKey e) key)) 0)))

    (defn subseq
      "sc must be a sorted collection, test(s) one of <, <=, > or
  >=. Returns a seq of those entries with keys ek for
  which (test (.. sc comparator (compare ek key)) 0) is true"
      {:added "1.0"
       :static true}
      ([^clojure.lang.Sorted sc test key]
       (let [include (mk-bound-fn sc test key)]
         (if (#{> >=} test)
           (when-let [[e :as s] (. sc seqFrom key true)]
             (if (include e) s (next s)))
           (take-while include (. sc seq true)))))
      ([^clojure.lang.Sorted sc start-test start-key end-test end-key]
       (when-let [[e :as s] (. sc seqFrom start-key true)]
         (take-while (mk-bound-fn sc end-test end-key)
                     (if ((mk-bound-fn sc start-test start-key) e) s (next s))))))

    (defn rsubseq
      "sc must be a sorted collection, test(s) one of <, <=, > or
  >=. Returns a reverse seq of those entries with keys ek for
  which (test (.. sc comparator (compare ek key)) 0) is true"
      {:added "1.0"
       :static true}
      ([^clojure.lang.Sorted sc test key]
       (let [include (mk-bound-fn sc test key)]
         (if (#{< <=} test)
           (when-let [[e :as s] (. sc seqFrom key false)]
             (if (include e) s (next s)))
           (take-while include (. sc seq false)))))
      ([^clojure.lang.Sorted sc start-test start-key end-test end-key]
       (when-let [[e :as s] (. sc seqFrom end-key false)]
         (take-while (mk-bound-fn sc start-test start-key)
                     (if ((mk-bound-fn sc end-test end-key) e) s (next s)))))))

(defn repeatedly
  "Takes a function of no args, presumably with side effects, and
  returns an infinite (or length n if supplied) lazy sequence of calls
  to it"
  {:added "1.0"
   :static true}
  ([f] (lazy-seq (cons (f) (repeatedly f))))
  ([n f] (take n (repeatedly f))))

(defn add-classpath
  "DEPRECATED

  Adds the url (String or URL object) to the classpath per
  URLClassLoader.addURL"
  {:added "1.0"
   :deprecated "1.1"}
  [url]
  (println "WARNING: add-classpath is deprecated")
  (code/add_patha.e (erlang/binary_to_list.e url)))

(defn hash
  "Returns the hash code of its argument. Note this is the hash code
  consistent with =, and thus is different than .hashCode for Integer,
  Short, Byte and Clojure collections."

  {:added "1.0"
   :static true}
  [x]
  (clojerl.IHash/hash.e x))

(defn mix-collection-hash
  "Mix final collection hash for ordered or unordered collections.
   hash-basis is the combined collection hash, count is the number
   of elements included in the basis. Note this is the hash code
   consistent with =, different from .hashCode.
   See http://clojure.org/data_structures#hash for full algorithms."
  {:added "1.6"
   :static true}
  ^long
  [^long hash-basis ^long count]
  (throw "unimplemented hash")
  #_(clojure.lang.Murmur3/mixCollHash hash-basis count))

(defn hash-ordered-coll
  "Returns the hash code, consistent with =, for an external ordered
   collection implementing Iterable.
   See http://clojure.org/data_structures#hash for full algorithms."
  {:added "1.6"
   :static true}
  ^long
  [coll]
  (clj_murmur3/ordered.e coll))

(defn hash-unordered-coll
  "Returns the hash code, consistent with =, for an external unordered
   collection implementing Iterable. For maps, the iterator should
   return map entries whose hash is computed as
     (hash-ordered-coll [k v]).
   See http://clojure.org/data_structures#hash for full algorithms."
  {:added "1.6"
   :static true}
  ^long
  [coll]
  (clj_murmur3/unordered.e coll))

(defn interpose
  "Returns a lazy seq of the elements of coll separated by sep.
  Returns a stateful transducer when no collection is provided."
  {:added "1.0"
   :static true}
  #_([sep]
   (fn [rf]
     (let [started (volatile! false)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if @started
            (let [sepr (rf result sep)]
              (if (reduced? sepr)
                sepr
                (rf sepr input)))
            (do
              (vreset! started true)
              (rf result input))))))))
  ([sep coll]
   (drop 1 (interleave (repeat sep) coll))))

#_(defmacro definline
  "Experimental - like defmacro, except defines a named function whose
  body is the expansion, calls to which may be expanded inline as if
  it were a macro. Cannot be used with variadic (&) args."
  {:added "1.0"}
  [name & decl]
  (let [[pre-args [args expr]] (split-with (comp not vector?) decl)]
    `(do
       (defn ~name ~@pre-args ~args ~(apply (eval (list `fn args expr)) args))
       (alter-meta! (var ~name) assoc :inline (fn ~name ~args ~expr))
       (var ~name))))

(defn empty
  "Returns an empty collection of the same category as coll, or nil"
  {:added "1.0"
   :static true}
  [coll]
  (when (satisfies? clojerl.IColl coll)
    (clj_core/empty.e coll)))

#_ ((defmacro amap
      "Maps an expression across an array a, using an index named idx, and
  return value named ret, initialized to a clone of a, then setting
  each element of ret to the evaluation of expr, returning the new
  array ret."
      {:added "1.0"}
      [a idx ret expr]
      `(let [a# ~a
             ~ret (aclone a#)]
         (loop  [~idx 0]
           (if (< ~idx  (alength a#))
             (do
               (aset ~ret ~idx ~expr)
               (recur (inc ~idx)))
             ~ret))))

    (defmacro areduce
      "Reduces an expression across an array a, using an index named idx,
  and return value named ret, initialized to init, setting ret to the
  evaluation of expr at each step, returning ret."
      {:added "1.0"}
      [a idx ret init expr]
      `(let [a# ~a l# (alength a#)]
         (loop  [~idx 0 ~ret ~init]
           (if (< ~idx l#)
             (recur (inc-int ~idx) ~expr)
             ~ret))))

    (defn float-array
      "Creates an array of floats"
      {:inline (fn [& args] `(. clojure.lang.Numbers float_array ~@args))
       :inline-arities #{1 2}
       :added "1.0"}
      ([size-or-seq] (. clojure.lang.Numbers float_array size-or-seq))
      ([size init-val-or-seq] (. clojure.lang.Numbers float_array size init-val-or-seq)))

    (defn boolean-array
      "Creates an array of booleans"
      {:inline (fn [& args] `(. clojure.lang.Numbers boolean_array ~@args))
       :inline-arities #{1 2}
       :added "1.1"}
      ([size-or-seq] (. clojure.lang.Numbers boolean_array size-or-seq))
      ([size init-val-or-seq] (. clojure.lang.Numbers boolean_array size init-val-or-seq)))

    (defn byte-array
      "Creates an array of bytes"
      {:inline (fn [& args] `(. clojure.lang.Numbers byte_array ~@args))
       :inline-arities #{1 2}
       :added "1.1"}
      ([size-or-seq] (. clojure.lang.Numbers byte_array size-or-seq))
      ([size init-val-or-seq] (. clojure.lang.Numbers byte_array size init-val-or-seq)))

    (defn char-array
      "Creates an array of chars"
      {:inline (fn [& args] `(. clojure.lang.Numbers char_array ~@args))
       :inline-arities #{1 2}
       :added "1.1"}
      ([size-or-seq] (. clojure.lang.Numbers char_array size-or-seq))
      ([size init-val-or-seq] (. clojure.lang.Numbers char_array size init-val-or-seq)))

    (defn short-array
      "Creates an array of shorts"
      {:inline (fn [& args] `(. clojure.lang.Numbers short_array ~@args))
       :inline-arities #{1 2}
       :added "1.1"}
      ([size-or-seq] (. clojure.lang.Numbers short_array size-or-seq))
      ([size init-val-or-seq] (. clojure.lang.Numbers short_array size init-val-or-seq)))

    (defn double-array
      "Creates an array of doubles"
      {:inline (fn [& args] `(. clojure.lang.Numbers double_array ~@args))
       :inline-arities #{1 2}
       :added "1.0"}
      ([size-or-seq] (. clojure.lang.Numbers double_array size-or-seq))
      ([size init-val-or-seq] (. clojure.lang.Numbers double_array size init-val-or-seq)))

    (defn object-array
      "Creates an array of objects"
      {:inline (fn [arg] `(. clojure.lang.RT object_array ~arg))
       :inline-arities #{1}
       :added "1.2"}
      ([size-or-seq] (. clojure.lang.RT object_array size-or-seq)))

    (defn int-array
      "Creates an array of ints"
      {:inline (fn [& args] `(. clojure.lang.Numbers int_array ~@args))
       :inline-arities #{1 2}
       :added "1.0"}
      ([size-or-seq] (. clojure.lang.Numbers int_array size-or-seq))
      ([size init-val-or-seq] (. clojure.lang.Numbers int_array size init-val-or-seq)))

    (defn long-array
      "Creates an array of longs"
      {:inline (fn [& args] `(. clojure.lang.Numbers long_array ~@args))
       :inline-arities #{1 2}
       :added "1.0"}
      ([size-or-seq] (. clojure.lang.Numbers long_array size-or-seq))
      ([size init-val-or-seq] (. clojure.lang.Numbers long_array size init-val-or-seq)))

    (definline booleans
      "Casts to boolean[]"
      {:added "1.1"}
      [xs] `(. clojure.lang.Numbers booleans ~xs))

    (definline bytes
      "Casts to bytes[]"
      {:added "1.1"}
      [xs] `(. clojure.lang.Numbers bytes ~xs))

    (definline chars
      "Casts to chars[]"
      {:added "1.1"}
      [xs] `(. clojure.lang.Numbers chars ~xs))

    (definline shorts
      "Casts to shorts[]"
      {:added "1.1"}
      [xs] `(. clojure.lang.Numbers shorts ~xs))

    (definline floats
      "Casts to float[]"
      {:added "1.0"}
      [xs] `(. clojure.lang.Numbers floats ~xs))

    (definline ints
      "Casts to int[]"
      {:added "1.0"}
      [xs] `(. clojure.lang.Numbers ints ~xs))

    (definline doubles
      "Casts to double[]"
      {:added "1.0"}
      [xs] `(. clojure.lang.Numbers doubles ~xs))

    (definline longs
      "Casts to long[]"
      {:added "1.0"}
      [xs] `(. clojure.lang.Numbers longs ~xs)))

#_(import '(java.util.concurrent BlockingQueue LinkedBlockingQueue))

(defn seque
  "Creates a queued seq on another (presumably lazy) seq s. The queued
  seq will produce a concrete seq in the background, and can get up to
  n items ahead of the consumer. n-or-q can be an integer n buffer
  size, or an instance of java.util.concurrent BlockingQueue. Note
  that reading from a seque can block if the reader gets ahead of the
  producer."
  {:added "1.0"
   :static true}
  ([s] (seque 100 s))
  ([n-or-q s]
   (throw "unimplemented queue")
   #_(let [^BlockingQueue q (if (instance? BlockingQueue n-or-q)
                             n-or-q
                             (LinkedBlockingQueue. (int n-or-q)))
         NIL (Object.) ;nil sentinel since LBQ doesn't support nils
         agt (agent (lazy-seq s)) ; never start with nil; that signifies we've already put eos
         log-error (fn [q e]
                     (if (.offer q q)
                       (throw e)
                       e))
         fill (fn [s]
                (when s
                  (if (instance? Exception s) ; we failed to .offer an error earlier
                    (log-error q s)
                    (try
                      (loop [[x & xs :as s] (seq s)]
                        (if s
                          (if (.offer q (if (nil? x) NIL x))
                            (recur xs)
                            s)
                          (when-not (.offer q q) ; q itself is eos sentinel
                            ()))) ; empty seq, not nil, so we know to put eos next time
                      (catch Exception e
                        (log-error q e))))))
         drain (fn drain []
                 (lazy-seq
                  (let [x (.take q)]
                    (if (identical? x q) ;q itself is eos sentinel
                      (do @agt nil)  ;touch agent just to propagate errors
                      (do
                        (send-off agt fill)
                        (release-pending-sends)
                        (cons (if (identical? x NIL) nil x) (drain)))))))]
     (send-off agt fill)
     (drain))))

(defn class?
  "Returns true if x is an instance of Class"
  {:added "1.0"
   :static true}
  [x]
  (throw "unsupported class"))

(defn- is-annotation? [c]
  (throw "unsupported class"))

(defn- is-runtime-annotation? [^Class c]
  (throw "unsupported class"))

(defn- descriptor [^Class c]
  (throw "unsupported class"))

(declare process-annotation)
(defn- add-annotation [^clojure.asm.AnnotationVisitor av name v]
  (throw "unsupported class"))

(defn- process-annotation [av v]
  (throw "unsupported class"))

(defn- add-annotations
  [& _]
  (throw "unsupported class"))

(defn alter-var-root
  "Atomically alters the root binding of var v by applying f to its
  current value plus any args"
  {:added "1.0"
   :static true}
  [^clojure.lang.Var v f & args]
  (throw "unsupported alter-var-root"))

(defn bound?
  "Returns true if all of the vars provided as arguments have any bound value, root or thread-local.
   Implies that deref'ing the provided vars will succeed. Returns true if no vars are provided."
  {:added "1.2"
   :static true}
  [& vars]
  (every? clojerl.Var/is_bound.1 vars))

(defn thread-bound?
  "Returns true if all of the vars provided as arguments have thread-local bindings.
   Implies that set!'ing the provided vars will succeed.  Returns true if no vars are provided."
  {:added "1.2"
   :static true}
  [& vars]
  (every? clojerl.Var/dynamic_binding.1 vars))

(defn make-hierarchy
  "Creates a hierarchy object for use with derive, isa? etc."
  {:added "1.0"
   :static true}
  [] {:parents {} :descendants {} :ancestors {}})

(def ^{:private true}
     global-hierarchy (make-hierarchy))

(defn not-empty
  "If coll is empty, returns nil, else coll"
  {:added "1.0"
   :static true}
  [coll] (when (seq coll) coll))

(defn bases
  "Returns the immediate superclass and direct interfaces of c, if any"
  {:added "1.0"
   :static true}
  [^Class c]
  (throw "unsupported classes and interfaces"))

(defn supers
  "Returns the immediate and indirect superclasses and interfaces of c, if any"
  {:added "1.0"
   :static true}
  [^Class class]
  (throw "unsupported classes and interfaces"))

(defn isa?
  "Returns true if (= child parent), or child is directly or indirectly derived from
  parent, either via a Java type inheritance relationship or a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy"
  {:added "1.0"}
  ([child parent] (isa? global-hierarchy child parent))
  ([h child parent]
   (or (= child parent)
       (contains? ((:ancestors h) child) parent)
       (and (vector? parent) (vector? child)
            (= (count parent) (count child))
            (loop [ret true i 0]
              (if (or (not ret) (= i (count parent)))
                ret
                (recur (isa? h (child i) (parent i)) (inc i))))))))

(defn parents
  "Returns the immediate parents of tag, either via a Java type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy"
  {:added "1.0"}
  ([tag] (parents global-hierarchy tag))
  ([h tag] (not-empty
            (let [tp (get (:parents h) tag)]
              (if (class? tag)
                (into1 (set (bases tag)) tp)
                tp)))))

(defn ancestors
  "Returns the immediate and indirect parents of tag, either via a Java type
  inheritance relationship or a relationship established via derive. h
  must be a hierarchy obtained from make-hierarchy, if not supplied
  defaults to the global hierarchy"
  {:added "1.0"}
  ([tag] (ancestors global-hierarchy tag))
  ([h tag] (not-empty
            (let [ta (get (:ancestors h) tag)]
              (if (class? tag)
                (let [superclasses (set (supers tag))]
                  (reduce1 into1 superclasses
                    (cons ta
                          (map #(get (:ancestors h) %) superclasses))))
                ta)))))

(defn descendants
  "Returns the immediate and indirect children of tag, through a
  relationship established via derive. h must be a hierarchy obtained
  from make-hierarchy, if not supplied defaults to the global
  hierarchy. Note: does not work on Java type inheritance
  relationships."
  {:added "1.0"}
  ([tag] (descendants global-hierarchy tag))
  ([h tag] (if (class? tag)
             (throw "Can't get descendants of classes")
             (not-empty (get (:descendants h) tag)))))

(defn derive
  "Establishes a parent/child relationship between parent and
  tag. Parent must be a namespace-qualified symbol or keyword and
  child can be either a namespace-qualified symbol or keyword or a
  class. h must be a hierarchy obtained from make-hierarchy, if not
  supplied defaults to, and modifies, the global hierarchy."
  {:added "1.0"}
  ([tag parent]
   (assert (namespace parent))
   (assert (or (class? tag) (and (satisfies? clojerl.Named tag) (namespace tag))))

   (alter-var-root #'global-hierarchy derive tag parent) nil)
  ([h tag parent]
   (assert (not= tag parent))
   (assert (or (class? tag) (satisfies? clojerl.Named tag)))
   (assert (satisfies? clojerl.Named parent))

   (let [tp (:parents h)
         td (:descendants h)
         ta (:ancestors h)
         tf (fn [m source sources target targets]
              (reduce1 (fn [ret k]
                        (assoc ret k
                               (reduce1 conj (get targets k #{}) (cons target (targets target)))))
                      m (cons source (sources source))))]
     (or
      (when-not (contains? (tp tag) parent)
        (when (contains? (ta tag) parent)
          (throw (print-str tag "already has" parent "as ancestor")))
        (when (contains? (ta parent) tag)
          (throw (print-str "Cyclic derivation:" parent "has" tag "as ancestor")))
        {:parents (assoc (:parents h) tag (conj (get tp tag #{}) parent))
         :ancestors (tf (:ancestors h) tag td parent ta)
         :descendants (tf (:descendants h) parent ta tag td)})
      h))))

(declare flatten)

(defn underive
  "Removes a parent/child relationship between parent and
  tag. h must be a hierarchy obtained from make-hierarchy, if not
  supplied defaults to, and modifies, the global hierarchy."
  {:added "1.0"}
  ([tag parent] (alter-var-root #'global-hierarchy underive tag parent) nil)
  ([h tag parent]
    (let [parentMap (:parents h)
	  childsParents (if (parentMap tag)
			  (disj (parentMap tag) parent) #{})
	  newParents (if (not-empty childsParents)
		       (assoc parentMap tag childsParents)
		       (dissoc parentMap tag))
	  deriv-seq (flatten (map #(cons (key %) (interpose (key %) (val %)))
				       (seq newParents)))]
      (if (contains? (parentMap tag) parent)
	(reduce1 #(apply derive %1 %2) (make-hierarchy)
		(partition 2 deriv-seq))
	h))))


(defn distinct?
  "Returns true if no two of the arguments are ="
  {:tag clojerl.Boolean
   :added "1.0"
   :static true}
  ([x] true)
  ([x y] (not (= x y)))
  ([x y & more]
   (if (not= x y)
     (loop [s #{x y} [x & etc :as xs] more]
       (if xs
         (if (contains? s x)
           false
           (recur (conj s x) etc))
         true))
     false)))

(defn resultset-seq
  "Creates and returns a lazy sequence of structmaps corresponding to
  the rows in the java.sql.ResultSet rs"
  {:added "1.0"}
  [^java.sql.ResultSet rs]
  (throw "unsupported sql result set"))

(defn iterator-seq
  "Returns a seq on a java.util.Iterator. Note that most collections
  providing iterators implement Iterable and thus support seq directly.
  Seqs cache values, thus iterator-seq should not be used on any
  iterator that repeatedly returns the same mutable object."
  {:added "1.0"
   :static true}
  [iter]
  (throw "unimplemented iterator")
  #_(clojure.lang.RT/chunkIteratorSeq iter))

(defn enumeration-seq
  "Returns a seq on a java.util.Enumeration"
  {:added "1.0"
   :static true}
  [e]
  (throw "unimplemented iterator")
  #_(clojure.lang.EnumerationSeq/create e))

(defn format
  "Formats a string using io_lib/format, see io/format for format
  string syntax"
  {:added "1.0"
   :static true}
  ^clojerl.String [fmt & args]
  (->> (clj_core/to_list.e args)
       (io_lib/format.e fmt)
       erlang/list_to_binary.e))

(defn printf
  "Prints formatted output, as per format"
  {:added "1.0"
   :static true}
  [fmt & args]
  (print (apply format fmt args)))

(declare gen-class)

(defmacro with-loading-context [& body]
  `(do ~@body)
  #_`((fn loading# []
        (. clojure.lang.Var (pushThreadBindings {clojure.lang.Compiler/LOADER
                                                 (.getClassLoader (.getClass ^Object loading#))}))
        (try
         ~@body
         (finally
          (. clojure.lang.Var (popThreadBindings)))))))

(defn maybe-unquote
  [name]
  (if (seq? name)
    (if (= (first name) 'quote)
      (second name)
      name)
    name))

(defn in-ns
  [name]
  (let [name (maybe-unquote name)]
    (if (symbol? name)
      (clj_namespace/find_or_create.e name)
      (throw (str "First argument to in-ns must be a symbol, got: " (type name))))))

(defmacro ns
  "Sets *ns* to the namespace named by name (unevaluated), creating it
  if needed.  references can be zero or more of: (:refer-clojure ...)
  (:require ...) (:use ...) (:import ...) (:load ...) (:gen-class)
  with the syntax of refer-clojure/require/use/import/load/gen-class
  respectively, except the arguments are unevaluated and need not be
  quoted. (:gen-class ...), when supplied, defaults to :name
  corresponding to the ns name, :main true, :impl-ns same as ns, and
  :init-impl-ns true. All options of gen-class are
  supported. The :gen-class directive is ignored when not
  compiling. If :gen-class is not supplied, when compiled only an
  nsname__init.class will be generated. If :refer-clojure is not used, a
  default (refer 'clojure.core) is used.  Use of ns is preferred to
  individual calls to in-ns/require/use/import:

  (ns foo.bar
    (:refer-clojure :exclude [ancestors printf])
    (:require (clojure.contrib sql combinatorics))
    (:use (my.lib this that))
    (:import (java.util Date Timer Random)
             (java.sql Connection Statement)))"
  {:arglists '([name docstring? attr-map? references*])
   :added "1.0"}
  [name & references]
  (let [process-reference
        (fn [all-args]
          (let [kname    (first all-args)
                args     (rest all-args)
                name-str (clojure.core/name kname)
                name-sym (symbol "clojure.core" (str name-str "*"))]
            `(~(symbol "clojure.core" (clojure.core/name kname))
              ~@(map #(list 'quote %) args))))
        docstring  (when (string? (first references)) (first references))
        references (if docstring (next references) references)
        name (if docstring
               (vary-meta name assoc :doc docstring)
               name)
        metadata   (when (map? (first references)) (first references))
        references (if metadata (next references) references)
        name (if metadata
               (vary-meta name merge metadata)
               name)
        gen-class-clause (first (filter #(= :gen-class (first %)) references))
        gen-class-call
          (when gen-class-clause
            (list* `gen-class
                   :name (binary/replace.e (str name) "-" "_")
                   :impl-ns name
                   :main true (next gen-class-clause)))
        references (remove #(= :gen-class (first %)) references)
        ;ns-effect (clojure.core/in-ns name)
        name-metadata (meta name)]
    `(do
       (clojure.core/in-ns '~name)
       (with-loading-context
         ;; ~@(when gen-class-call (list gen-class-call))
         ~@(when (and (not= name 'clojure.core)
                      (not-any? #(= :refer-clojure (first %)) references))
             `((clojure.core/refer 'clojure.core)))
         ~@(map process-reference references))
       #_(if (= '~name 'clojure.core)
         nil
         (do (dosync (commute @#'*loaded-libs* conj '~name)) nil)))))

(defmacro refer-clojure
  "Same as (refer 'clojure.core <filters>)"
  {:added "1.0"}
  [& filters]
  `(clojure.core/refer 'clojure.core ~@filters))

(defmacro defonce
  "defs name to have the root value of the expr iff the named var has no root value,
  else expr is unevaluated"
  {:added "1.0"}
  [name expr]
  `(let [v# (def ~name)]
     (when-not (clojerl.Var/has_root.e v#)
       (def ~name ~expr))))

;;;;;;;;;;; require/use/load, contributed by Stephen C. Gilardi ;;;;;;;;;;;;;;;;;;

(defonce
  ^{:dynamic true
    :private true
    :doc "A ref to a sorted set of symbols representing loaded libs"}
  *loaded-libs* ;; TODO: should be a sorted-set
  #{})

(defonce ^:dynamic
  ^{:private true
    :doc "A stack of paths currently being loaded by this thread"}
  *pending-paths* ())

(defonce ^:dynamic
  ^{:private true :doc
    "True while a verbose load is pending"}
  *loading-verbosely* false)

(defn- throw-if
  "Throws an error with a message if pred is true"
  [pred fmt & args]
  (when pred
    (let [message   (apply format fmt (map str args))
          raw-trace (try (throw :ex) (catch :throw ex (erlang/get_stacktrace.e)))
          ;;boring?   #(not= (.getMethodName %) "doInvoke")
          ;;trace (into-array (drop 2 (drop-while boring? raw-trace)))
          ]
      ;;(.setStackTrace exception trace)
      (erlang/raise.e :error message raw-trace))))

(defn- libspec?
  "Returns true if x is a libspec"
  [x]
  (or (symbol? x)
      (and (vector? x)
           (or
            (nil? (second x))
            (keyword? (second x))))))

(defn- prependss
  "Prepends a symbol or a seq to coll"
  [x coll]
  (if (symbol? x)
    (cons x coll)
    (concat x coll)))

(defn- root-resource
  "Returns the root directory path for a lib"
  {:tag clojerl.String}
  [lib]
  (str "/"
       (-> (name lib)
           (binary/replace.e "." "/" (clj_core/to_list.e [:global]))
           (binary/replace.e "-" "_" (clj_core/to_list.e [:global])))))

(defn- index-of [s x]
  (let [matches (binary/matches.e s x)]
    (if (seq matches)
      (ffirst matches)
      -1)))

(defn- last-index-of [s x]
  (let [matches (binary/matches.e s x)]
    (if (seq matches)
      (first (last matches))
      -1)))

(defn- root-directory
  "Returns the root resource path for a lib"
  [lib]
  (let [d (root-resource lib)]
    (subs d 0 (last-index-of d "/"))))

(def ^:declared ^:redef load)

(defn- load-one
  "Loads a lib given its name. If need-ns, ensures that the associated
  namespace exists after loading. If require, records the load so any
  duplicate loads can be skipped."
  [lib need-ns require]
  (load (root-resource lib))
  (throw-if (and need-ns (not (find-ns lib)))
            "namespace '~s' not found after loading '~s'"
            lib (root-resource lib))
  (when require
    (clj_core/set!.e #'*loaded-libs* (conj *loaded-libs* lib))))

(defn- load-all
  "Loads a lib given its name and forces a load of any libs it directly or
  indirectly loads. If need-ns, ensures that the associated namespace
  exists after loading. If require, records the load so any duplicate loads
  can be skipped."
  [lib need-ns require]
  (clj_core/set!.e #'*loaded-libs*
                   (reduce1 conj
                            *loaded-libs*
                            ;; TODO: should be a sorted-set
                            (binding [*loaded-libs* (hash-set)]
                              (load-one lib need-ns require)
                              *loaded-libs*))))

(defn- load-lib
  "Loads a lib with options"
  [prefix lib & options]
  (throw-if (and prefix (pos? (index-of (name lib) "\\.")))
            "Found lib name '~s' containing period with prefix '~s'.  lib names inside prefix lists must not contain periods"
            (name lib) prefix)
  (let [lib (if prefix (symbol (str prefix \. lib)) lib)
        opts (apply hash-map options)
        as         (:as opts)
        reload     (:reload opts)
        reload-all (:reload-all opts)
        require    (:require opts)
        use        (:use opts)
        verbose    (:verbose opts)
        loaded (contains? @#'*loaded-libs* lib)
        load (cond reload-all
                   load-all
                   (or reload (not require) (not loaded))
                   load-one)
        need-ns (or as use)
        filter-opts (select-keys opts '(:exclude :only :rename :refer))
        undefined-on-entry (not (find-ns lib))]
    (binding [*loading-verbosely* (or *loading-verbosely* verbose)]
      (if load
        (try
          (load lib need-ns require)
          (catch :throw e
            (when undefined-on-entry
              (remove-ns lib))
            (throw e)))
        (throw-if (and need-ns (not (find-ns lib)))
                  "namespace '~s' not found" lib))
      (when (and need-ns *loading-verbosely*)
        (printf "(clojure.core/in-ns '~s)\n" (str (ns-name *ns*))))
      (when as
        (when *loading-verbosely*
          (printf "(clojure.core/alias '~s '~s)\n" (str as) (str lib)))
        (alias as lib))
      (when (or use (:refer filter-opts))
        (when *loading-verbosely*
          (printf "(clojure.core/refer '~s" (str lib))
          (doseq [opt filter-opts]
            (printf " ~s '~s" (key opt) (print-str (val opt))))
          (printf ")\n"))
        (apply refer lib (mapcat seq filter-opts))))))

(defn- load-libs
  "Loads libs, interpreting libspecs, prefix lists, and flags for
  forwarding to load-lib"
  [& args]
  (let [flags (filter keyword? args)
        opts (interleave flags (repeat true))
        args (filter (complement keyword?) args)]
    ; check for unsupported options
    (let [supported #{:as :reload :reload-all :require :use :verbose :refer}
          unsupported (seq (remove supported flags))]
      (throw-if unsupported
                (apply str "Unsupported option(s) supplied: "
                       (interpose "," unsupported))))
    ; check a load target was specified
    (throw-if (not (seq args)) "Nothing specified to load")
    (doseq [arg args]
      (if (libspec? arg)
        (apply load-lib nil (prependss arg opts))
        (let [prefix (first arg)
              args   (rest arg)]
          (throw-if (nil? prefix) "prefix cannot be nil")
          (doseq [arg args]
            (apply load-lib prefix (prependss arg opts))))))))

(defn- check-cyclic-dependency
  "Detects and rejects non-trivial cyclic load dependencies. The
  exception message shows the dependency chain with the cycle
  highlighted. Ignores the trivial case of a file attempting to load
  itself because that can occur when a gen-class'd class loads its
  implementation."
  [path]
  (when (some #{path} (rest *pending-paths*))
    (let [pending (map #(if (= % path) (str "[ " % " ]") %)
                       (cons path *pending-paths*))
          chain (apply str (interpose "->" pending))]
      (throw-if true "Cyclic load dependency: %s" chain))))

;; Public

(defn require
  "Loads libs, skipping any that are already loaded. Each argument is
  either a libspec that identifies a lib, a prefix list that identifies
  multiple libs whose names share a common prefix, or a flag that modifies
  how all the identified libs are loaded. Use :require in the ns macro
  in preference to calling this directly.

  Libs

  A 'lib' is a named set of resources in classpath whose contents define a
  library of Clojure code. Lib names are symbols and each lib is associated
  with a Clojure namespace and a Java package that share its name. A lib's
  name also locates its root directory within classpath using Java's
  package name to classpath-relative path mapping. All resources in a lib
  should be contained in the directory structure under its root directory.
  All definitions a lib makes should be in its associated namespace.

  'require loads a lib by loading its root resource. The root resource path
  is derived from the lib name in the following manner:
  Consider a lib named by the symbol 'x.y.z; it has the root directory
  <classpath>/x/y/, and its root resource is <classpath>/x/y/z.clj. The root
  resource should contain code to create the lib's namespace (usually by using
  the ns macro) and load any additional lib resources.

  Libspecs

  A libspec is a lib name or a vector containing a lib name followed by
  options expressed as sequential keywords and arguments.

  Recognized options:
  :as takes a symbol as its argument and makes that symbol an alias to the
    lib's namespace in the current namespace.
  :refer takes a list of symbols to refer from the namespace or the :all
    keyword to bring in all public vars.

  Prefix Lists

  It's common for Clojure code to depend on several libs whose names have
  the same prefix. When specifying libs, prefix lists can be used to reduce
  repetition. A prefix list contains the shared prefix followed by libspecs
  with the shared prefix removed from the lib names. After removing the
  prefix, the names that remain must not contain any periods.

  Flags

  A flag is a keyword.
  Recognized flags: :reload, :reload-all, :verbose
  :reload forces loading of all the identified libs even if they are
    already loaded
  :reload-all implies :reload and also forces loading of all libs that the
    identified libs directly or indirectly load via require or use
  :verbose triggers printing information about each load, alias, and refer

  Example:

  The following would load the libraries clojure.zip and clojure.set
  abbreviated as 's'.

  (require '(clojure zip [set :as s]))"
  {:added "1.0"}

  [& args]
  (apply load-libs :require args))

(defn use
  "Like 'require, but also refers to each lib's namespace using
  clojure.core/refer. Use :use in the ns macro in preference to calling
  this directly.

  'use accepts additional options in libspecs: :exclude, :only, :rename.
  The arguments and semantics for :exclude, :only, and :rename are the same
  as those documented for clojure.core/refer."
  {:added "1.0"}
  [& args] (apply load-libs :require :use args))

(defn loaded-libs
  "Returns a sorted set of symbols naming the currently loaded libs"
  {:added "1.0"}
  [] *loaded-libs*)

(defn load
  "Loads Clojure code from resources in the code path. A path is interpreted
  as code path-relative if it begins with a slash or relative to the root
  directory for the current namespace otherwise."
  {:redef true
   :added "1.0"}
  [& paths]
  (doseq [path paths]
    (let [path (if (clojerl.String/starts_with.e path "/")
                 path
                 (str (root-directory (ns-name *ns*)) "/" path))]
      (when *loading-verbosely*
        (printf "(clojure.core/load \"~s\")\n" path)
        (flush))
      (check-cyclic-dependency path)
      (when-not (= path (first *pending-paths*))
        (binding [*pending-paths* (conj *pending-paths* path)]
          (clj_core/load.e (subs path 1)))))))

(defn compile
  "Compiles the namespace named by the symbol lib into a set of
  classfiles. The source for the lib must be in a proper
  classpath-relative directory. The output files will go into the
  directory specified by *compile-path*, and that directory too must
  be in the classpath."
  {:added "1.0"}
  [lib]
  (binding [*compile-files* true]
    (load-one lib true true))
  lib)

(defn load-file
  {:added "1.0"}
  [path]
  (clj_compiler/load_file.e path))

;;;;;;;;;;;;; nested associative ops ;;;;;;;;;;;

(defn get-in
  "Returns the value in a nested associative structure,
  where ks is a sequence of keys. Returns nil if the key
  is not present, or the not-found value if supplied."
  {:added "1.2"
   :static true}
  ([m ks]
     (reduce1 get m ks))
  ([m ks not-found]
     (loop [sentinel (erlang/make_ref.e)
            m m
            ks (seq ks)]
       (if ks
         (let [m (get m (first ks) sentinel)]
           (if (identical? sentinel m)
             not-found
             (recur sentinel m (next ks))))
         m))))


(defn assoc-in
  "Associates a value in a nested associative structure, where ks is a
  sequence of keys and v is the new value and returns a new nested structure.
  If any levels do not exist, hash-maps will be created."
  {:added "1.0"
   :static true}
  [m [k & ks] v]
  (if ks
    (assoc m k (assoc-in (get m k) ks v))
    (assoc m k v)))

(defn update-in
  "'Updates' a value in a nested associative structure, where ks is a
  sequence of keys and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  nested structure.  If any levels do not exist, hash-maps will be
  created."
  {:added "1.0"
   :static true}
  ([m [k & ks] f & args]
   (if ks
     (assoc m k (apply update-in (get m k) ks f args))
     (assoc m k (apply f (get m k) args)))))

(defn update
  "'Updates' a value in an associative structure, where k is a
  key and f is a function that will take the old value
  and any supplied args and return the new value, and returns a new
  structure.  If the key does not exist, nil is passed as the old value."
  {:added "1.7"
   :static true}
  ([m k f]
   (assoc m k (f (get m k))))
  ([m k f x]
   (assoc m k (f (get m k) x)))
  ([m k f x y]
   (assoc m k (f (get m k) x y)))
  ([m k f x y z]
   (assoc m k (f (get m k) x y z)))
  ([m k f x y z & more]
   (assoc m k (apply f (get m k) x y z more))))

(defn empty?
  "Returns true if coll has no items - same as (not (seq coll)).
  Please use the idiom (seq x) rather than (not (empty? x))"
  {:added "1.0"
   :static true}
  [coll] (not (seq coll)))

(defn coll?
  "Returns true if x implements IPersistentCollection"
  {:added "1.0"
   :static true}
  [x] (satisfies? clojerl.IColl x))

(defn list?
  "Returns true if x implements IPersistentList"
  {:added "1.0"
   :static true}
  [x] (or  (instance? clojerl.List x)
           (instance? clojerl.erlang.List x)))

(defn ifn?
  "Returns true if x implements IFn. Note that many data structures
  (e.g. sets and maps) implement IFn"
  {:added "1.0"
   :static true}
  [x] (satisfies? clojerl.IFn x))

(defn fn?
  "Returns true if x implements Fn, i.e. is an object created via fn."
  {:added "1.0"
   :static true}
  [x] (erlang/is_function.e x))


(defn associative?
 "Returns true if coll implements Associative"
 {:added "1.0"
  :static true}
  [coll] (satisfies? clojerl.Associative coll))

(defn sequential?
 "Returns true if coll implements Sequential"
 {:added "1.0"
  :static true}
  [coll] (satisfies? clojerl.ISequential coll))

(defn sorted?
 "Returns true if coll implements Sorted"
 {:added "1.0"
   :static true}
  [coll]
  (throw "unimplemented sorted")
  #_(satisfies? clojerl.Sorted coll))

(defn counted?
 "Returns true if coll implements count in constant time"
 {:added "1.0"
   :static true}
  [coll] (satisfies? clojerl.Counted coll))

(defn reversible?
 "Returns true if coll implements Reversible"
 {:added "1.0"
   :static true}
  [coll]
  (throw "unimplemented reversible")
  #_(satisfies? clojerl.Reversible coll))

(def ^:dynamic
 ^{:doc "bound in a repl thread to the most recent value printed"
   :added "1.0"}
 *1)

(def ^:dynamic
 ^{:doc "bound in a repl thread to the second most recent value printed"
   :added "1.0"}
 *2)

(def ^:dynamic
 ^{:doc "bound in a repl thread to the third most recent value printed"
   :added "1.0"}
 *3)

(def ^:dynamic
 ^{:doc "bound in a repl thread to the most recent exception caught by the repl"
   :added "1.0"}
 *e)

(defn trampoline
  "trampoline can be used to convert algorithms requiring mutual
  recursion without stack consumption. Calls f with supplied args, if
  any. If f returns a fn, calls that fn with no arguments, and
  continues to repeat, until the return value is not a fn, then
  returns that non-fn value. Note that if you want to return a fn as a
  final value, you must wrap it in some data structure and unpack it
  after trampoline returns."
  {:added "1.0"
   :static true}
  ([f]
     (let [ret (f)]
       (if (fn? ret)
         (recur ret)
         ret)))
  ([f & args]
     (trampoline #(apply f args))))

(defn intern
  "Finds or creates a var named by the symbol name in the namespace
  ns (which can be a symbol or a namespace), setting its root binding
  to val if supplied. The namespace must exist. The var will adopt any
  metadata from the name symbol.  Returns the var."
  {:added "1.0"
   :static true}
  ([ns ^clojerl.Symbol name]
   (throw "unimplemented intern")
   #_(let [v (clojure.lang.Var/intern (the-ns ns) name)]
     (when (meta name) (.setMeta v (meta name)))
     v))
  ([ns name val]
   (throw "unimplemented intern")
   #_(let [v (clojure.lang.Var/intern (the-ns ns) name val)]
     (when (meta name) (.setMeta v (meta name)))
     v)))

(defmacro while
  "Repeatedly executes body while test expression is true. Presumes
  some side-effect will cause test to become false/nil. Returns nil"
  {:added "1.0"}
  [test & body]
  `(loop []
     (when ~test
       ~@body
       (recur))))

(defn memoize
  "Returns a memoized version of a referentially transparent function. The
  memoized version of the function keeps a cache of the mapping from arguments
  to results and, when calls with the same arguments are repeated often, has
  higher performance at the expense of higher memory use."
  {:added "1.0"
   :static true}
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (swap! mem assoc args ret)
          ret)))))

(defmacro condp
  "Takes a binary predicate, an expression, and a set of clauses.
  Each clause can take the form of either:

  test-expr result-expr

  test-expr :>> result-fn

  Note :>> is an ordinary keyword.

  For each clause, (pred test-expr expr) is evaluated. If it returns
  logical true, the clause is a match. If a binary clause matches, the
  result-expr is returned, if a ternary clause matches, its result-fn,
  which must be a unary function, is called with the result of the
  predicate as its argument, the result of that call being the return
  value of condp. A single default expression can follow the clauses,
  and its value will be returned if no clause matches. If no default
  expression is provided and no clause matches, an error is thrown."
  {:added "1.0"}

  [pred expr & clauses]
  (let [gpred (gensym "pred__")
        gexpr (gensym "expr__")
        emit (fn emit [pred expr args]
               (let [res    (split-at (if (= :>> (second args)) 3 2) args)
                     clause (first res)
                     a      (first clause)
                     b      (second clause)
                     c      (nth clause 2)
                     more   (second res)
                     n (count clause)]
                 (cond
                   (= 0 n) `(throw (str "No matching clause: " ~expr))
                   (= 1 n) a
                   (= 2 n) `(if (~pred ~a ~expr)
                              ~b
                              ~(emit pred expr more))
                   :else `(if-let [p# (~pred ~a ~expr)]
                            (~c p#)
                            ~(emit pred expr more)))))]
    `(let [~gpred ~pred
           ~gexpr ~expr]
       ~(emit gpred gexpr clauses))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; var documentation ;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(alter-meta! #'*agent* assoc :added "1.0")
;;(alter-meta! #'in-ns assoc :added "1.0")
;;(alter-meta! #'load-file assoc :added "1.0")

(defmacro add-doc-and-meta {:private true} [name docstring meta]
  nil
  #_`(alter-meta! (var ~name) merge (assoc ~meta :doc ~docstring)))

(add-doc-and-meta *file*
  "The path of the file being evaluated, as a String.

  When there is no file, e.g. in the REPL, the value is not defined."
  {:added "1.0"})

(add-doc-and-meta *command-line-args*
  "A sequence of the supplied command line arguments, or nil if
  none were supplied"
  {:added "1.0"})

(add-doc-and-meta *warn-on-reflection*
  "When set to true, the compiler will emit warnings when reflection is
  needed to resolve Java method calls or field accesses.

  Defaults to false."
  {:added "1.0"})

(add-doc-and-meta *compile-path*
  "Specifies the directory where 'compile' will write out .class
  files. This directory must be in the classpath for 'compile' to
  work.

  Defaults to \"classes\""
  {:added "1.0"})

(add-doc-and-meta *compile-files*
  "Set to true when compiling files, false otherwise."
  {:added "1.0"})

(add-doc-and-meta *unchecked-math*
  "While bound to true, compilations of +, -, *, inc, dec and the
  coercions will be done without overflow checks. While bound
  to :warn-on-boxed, same behavior as true, and a warning is emitted
  when compilation uses boxed math. Default: false."
  {:added "1.3"})

(add-doc-and-meta *compiler-options*
  "A map of keys to options.
  Note, when binding dynamically make sure to merge with previous value.
  Supported options:
  :elide-meta - a collection of metadata keys to elide during compilation.
  :disable-locals-clearing - set to true to disable clearing, useful for using a debugger
  Alpha, subject to change."
  {:added "1.4"})

(add-doc-and-meta *ns*
  "A clojure.lang.Namespace object representing the current namespace."
  {:added "1.0"})

(add-doc-and-meta *in*
  "A java.io.Reader object representing standard input for read operations.

  Defaults to System/in, wrapped in a LineNumberingPushbackReader"
  {:added "1.0"})

(add-doc-and-meta *out*
  "A java.io.Writer object representing standard output for print operations.

  Defaults to System/out, wrapped in an OutputStreamWriter"
  {:added "1.0"})

(add-doc-and-meta *err*
  "A java.io.Writer object representing standard error for print operations.

  Defaults to System/err, wrapped in a PrintWriter"
  {:added "1.0"})

(add-doc-and-meta *flush-on-newline*
  "When set to true, output will be flushed whenever a newline is printed.

  Defaults to true."
  {:added "1.0"})

(add-doc-and-meta *print-meta*
  "If set to logical true, when printing an object, its metadata will also
  be printed in a form that can be read back by the reader.

  Defaults to false."
  {:added "1.0"})

(add-doc-and-meta *print-dup*
  "When set to logical true, objects will be printed in a way that preserves
  their type when read in later.

  Defaults to false."
  {:added "1.0"})

(add-doc-and-meta *print-readably*
  "When set to logical false, strings and characters will be printed with
  non-alphanumeric characters converted to the appropriate escape sequences.

  Defaults to true"
  {:added "1.0"})

(add-doc-and-meta *read-eval*
 "Defaults to true (or value specified by system property, see below)
  ***This setting implies that the full power of the reader is in play,
  including syntax that can cause code to execute. It should never be
  used with untrusted sources. See also: clojure.edn/read.***

  When set to logical false in the thread-local binding,
  the eval reader (#=) and record/type literal syntax are disabled in read/load.
  Example (will fail): (binding [*read-eval* false] (read-string \"#=(* 2 21)\"))

  The default binding can be controlled by the system property
  'clojure.read.eval' System properties can be set on the command line
  like this:

  java -Dclojure.read.eval=false ...

  The system property can also be set to 'unknown' via
  -Dclojure.read.eval=unknown, in which case the default binding
  is :unknown and all reads will fail in contexts where *read-eval*
  has not been explicitly bound to either true or false. This setting
  can be a useful diagnostic tool to ensure that all of your reads
  occur in considered contexts. You can also accomplish this in a
  particular scope by binding *read-eval* to :unknown
  "
  {:added "1.0"})

(defn future?
  "Returns true if x is a future"
  {:added "1.1"
   :static true}
  [x]
  (throw "unimplemented future")
  #_(instance? java.util.concurrent.Future x))

(defn future-done?
  "Returns true if future f is done"
  {:added "1.1"
   :static true}
  [^java.util.concurrent.Future f]
  (throw "unimplemented future")
  #_(.isDone f))

(defmacro letfn
  "fnspec ==> (fname [params*] exprs) or (fname ([params*] exprs)+)

  Takes a vector of function specs and a body, and generates a set of
  bindings of functions to their names. All of the names are available
  in all of the definitions of the functions, as well as the body."
  {:added "1.0", :forms '[(letfn [fnspecs*] exprs*)],
   :special-form true, :url nil}
  [fnspecs & body]
  `(letfn* ~(vec (interleave (map first fnspecs)
                             (map #(cons `fn %) fnspecs)))
           ~@body))

(defn fnil
  "Takes a function f, and returns a function that calls f, replacing
  a nil first argument to f with the supplied value x. Higher arity
  versions can replace arguments in the second and third
  positions (y, z). Note that the function f can take any number of
  arguments, not just the one(s) being nil-patched."
  {:added "1.2"
   :static true}
  ([f x]
   (fn
     ([a] (f (if (nil? a) x a)))
     ([a b] (f (if (nil? a) x a) b))
     ([a b c] (f (if (nil? a) x a) b c))
     ([a b c & ds] (apply f (if (nil? a) x a) b c ds))))
  ([f x y]
   (fn
     ([a b] (f (if (nil? a) x a) (if (nil? b) y b)))
     ([a b c] (f (if (nil? a) x a) (if (nil? b) y b) c))
     ([a b c & ds] (apply f (if (nil? a) x a) (if (nil? b) y b) c ds))))
  ([f x y z]
   (fn
     ([a b] (f (if (nil? a) x a) (if (nil? b) y b)))
     ([a b c] (f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c)))
     ([a b c & ds] (apply f (if (nil? a) x a) (if (nil? b) y b) (if (nil? c) z c) ds)))))

;;;;;;; case ;;;;;;;;;;;;;

(defmacro case
  "Takes an expression, and a set of clauses.

  Each clause can take the form of either:

  test-constant result-expr

  (test-constant1 ... test-constantN)  result-expr

  The test-constants are not evaluated. They must be compile-time
  literals, and need not be quoted.  If the expression is equal to a
  test-constant, the corresponding result-expr is returned. A single
  default expression can follow the clauses, and its value will be
  returned if no clause matches. If no default expression is provided
  and no clause matches, an error is thrown.

  Unlike cond and condp, case does a constant-time dispatch, the
  clauses are not considered sequentially.  All manner of constant
  expressions are acceptable in case, including numbers, strings,
  symbols, keywords, and (Clojure) composites thereof. Note that since
  lists are used to group multiple constants that map to the same
  expression, a vector can be used to match a list if needed. The
  test-constants need not be all of the same type."
  {:added "1.2"}

  [e & clauses]
  `(case* ~e ~@clauses))

;; redefine reduce with internal-reduce

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helper files ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (load "core_proxy")
(load "core_print")
;; (load "genclass")
(load "core_deftype")
;; (load "gvec")
(load "instant")
(load "uuid")

(defn reduce ([f coll]) ([f val coll]))

(defn- seq-reduce
  ([f coll]
    (if-let [s (seq coll)]
      (reduce f (first s) (next s))
      (f)))
  ([f val coll]
    (loop [val val, coll (seq coll)]
      (if coll
        (let [nval (f val (first coll))]
          (if (reduced? nval)
            @nval
            (recur nval (next coll))))
        val))))

(defprotocol IKVReduce
  "Protocol for concrete associative types that can reduce themselves
   via a function of key and val faster than first/next recursion over map
   entries. Called by clojure.core/reduce-kv, and has same
   semantics (just different arg order)."
  (kv-reduce [amap f init]))

(defn reduce
  "f should be a function of 2 arguments. If val is not supplied,
  returns the result of applying f to the first 2 items in coll, then
  applying f to that result and the 3rd item, etc. If coll contains no
  items, f must accept no arguments as well, and reduce returns the
  result of calling f with no arguments.  If coll has only 1 item, it
  is returned and f is not called.  If val is supplied, returns the
  result of applying f to val and the first item in coll, then
  applying f to that result and the 2nd item, etc. If coll contains no
  items, returns val and f is not called."
  {:added "1.0"}
  ([f coll]
   (if (satisfies? clojerl.IReduce coll)
     (clojerl.IReduce/reduce.e coll f)
     (seq-reduce f coll)))
  ([f val coll]
   (if (satisfies? clojerl.IReduce coll)
     (clojerl.IReduce/reduce.e coll f val)
     (seq-reduce f val coll))))

(extend-protocol IKVReduce
  nil
  (kv-reduce
    [_ f init]
    init)

  ;;slow path default
  clojerl.Map
  (kv-reduce
    [amap f init]
    (reduce (fn [ret [k v]] (f ret k v)) init amap)))

(defn reduce-kv
  "Reduces an associative collection. f should be a function of 3
  arguments. Returns the result of applying f to init, the first key
  and the first value in coll, then applying f to that result and the
  2nd key and value, etc. If coll contains no entries, returns init
  and f is not called. Note that reduce-kv is supported on vectors,
  where the keys will be the ordinals."
  {:added "1.4"}
  ([f init coll]
     (kv-reduce coll f init)))

(defn completing
  "Takes a reducing function f of 2 args and returns a fn suitable for
  transduce by adding an arity-1 signature that calls cf (default -
  identity) on the result argument."
  {:added "1.7"}
  ([f] (completing f identity))
  ([f cf]
     (fn
       ([] (f))
       ([x] (cf x))
       ([x y] (f x y)))))

(defn transduce
  "reduce with a transformation of f (xf). If init is not
  supplied, (f) will be called to produce it. f should be a reducing
  step function that accepts both 1 and 2 arguments, if it accepts
  only 2 you can add the arity-1 with 'completing'. Returns the result
  of applying (the transformed) xf to init and the first item in coll,
  then applying xf to that result and the 2nd item, etc. If coll
  contains no items, returns init and f is not called. Note that
  certain transforms may inject or skip items."  {:added "1.7"}
  ([xform f coll] (transduce xform f (f) coll))
  ([xform f init coll]
     (let [f (xform f)
           ret (if (instance? clojerl.IReduce coll)
                 (clojerl.IReduce/reduce.e coll f init)
                 (seq-reduce coll f init))]
       (f ret))))

(defn into
  "Returns a new coll consisting of to-coll with all of the items of
  from-coll conjoined. A transducer may be supplied."
  {:added "1.0"
   :static true}
  ([to from]
   (reduce conj to from))
  ([to xform from]
   (transduce xform conj to from)))

(defn mapv
  "Returns a vector consisting of the result of applying f to the
  set of first items of each coll, followed by applying f to the set
  of second items in each coll, until any one of the colls is
  exhausted.  Any remaining items in other colls are ignored. Function
  f should accept number-of-colls arguments."
  {:added "1.4"
   :static true}
  ([f coll]
   (reduce (fn [v o] (conj v (f o))) [] coll))
  ([f c1 c2]
     (into [] (map f c1 c2)))
  ([f c1 c2 c3]
     (into [] (map f c1 c2 c3)))
  ([f c1 c2 c3 & colls]
     (into [] (apply map f c1 c2 c3 colls))))

(defn filterv
  "Returns a vector of the items in coll for which
  (pred item) returns true. pred must be free of side-effects."
  {:added "1.4"
   :static true}
  [pred coll]
  (reduce (fn [v o] (if (pred o) (conj v o) v))
          []
          coll))

(require '[clojure.erlang.io :as io])

(defn- normalize-slurp-opts
  [opts]
  (if (string? (first opts))
    (do
      (println "WARNING: (slurp f enc) is deprecated, use (slurp f :encoding enc).")
      [:encoding (first opts)])
    opts))

(defn slurp
  "Opens a reader on f and reads all its contents, returning a string.
  See clojure.erlang.io/reader for a complete list of supported arguments."
  {:added "1.0"}
  ([f & opts]
     (let [opts (normalize-slurp-opts opts)]
       (with-open [sw (new erlang.io.StringWriter)
                   r (apply io/reader f opts)]
         (io/copy r sw)
         (str sw)))))

(defn spit
  "Opposite of slurp.  Opens f with writer, writes content, then
  closes f. Options passed to clojure.erlang.io/writer."
  {:added "1.2"}
  [f content & options]
  (with-open [^erlang.io.IWriter w (apply io/writer f options)]
    (erlang.io.IWriter/write.e w (str content))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; futures (needs proxy);;;;;;;;;;;;;;;;;;
(defn future-call
  "Takes a function of no args and yields a future object that will
  invoke the function in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant
  of deref with timeout is used. See also - realized?."
  {:added "1.1"
   :static true}
  [f]
  (throw "unimplemented future")
  #_(let [f (binding-conveyor-fn f)
        fut (.submit clojure.lang.Agent/soloExecutor ^Callable f)]
    (reify
     clojure.lang.IDeref
     (deref [_] (deref-future fut))
     clojure.lang.IBlockingDeref
     (deref
      [_ timeout-ms timeout-val]
      (deref-future fut timeout-ms timeout-val))
     clojure.lang.IPending
     (isRealized [_] (.isDone fut))
     java.util.concurrent.Future
      (get [_] (.get fut))
      (get [_ timeout unit] (.get fut timeout unit))
      (isCancelled [_] (.isCancelled fut))
      (isDone [_] (.isDone fut))
      (cancel [_ interrupt?] (.cancel fut interrupt?)))))

(defmacro future
  "Takes a body of expressions and yields a future object that will
  invoke the body in another thread, and will cache the result and
  return it on all subsequent calls to deref/@. If the computation has
  not yet finished, calls to deref/@ will block, unless the variant of
  deref with timeout is used. See also - realized?."
  {:added "1.1"}
  [& body] `(future-call (^{:once true} fn* [] ~@body)))


(defn future-cancel
  "Cancels the future, if possible."
  {:added "1.1"
   :static true}
  [^java.util.concurrent.Future f]
  (throw "unimplemented future")
  #_(.cancel f true))

(defn future-cancelled?
  "Returns true if future f is cancelled"
  {:added "1.1"
   :static true}
  [^java.util.concurrent.Future f]
  (throw "unimplemented future")
  #_(.isCancelled f))

(defn pmap
  "Like map, except f is applied in parallel. Semi-lazy in that the
  parallel computation stays ahead of the consumption, but doesn't
  realize the entire result unless required. Only useful for
  computationally intensive functions where the time of f dominates
  the coordination overhead."
  {:added "1.0"
   :static true}
  ([f coll]
   (let [n (+ 2 (erlang/system_info.e :logical_processors_available))
         rets (map #(future (f %)) coll)
         step (fn step [[x & xs :as vs] fs]
                (lazy-seq
                 (if-let [s (seq fs)]
                   (cons (deref x) (step xs (rest s)))
                   (map deref vs))))]
     (step rets (drop n rets))))
  ([f coll & colls]
   (let [step (fn step [cs]
                (lazy-seq
                 (let [ss (map seq cs)]
                   (when (every? identity ss)
                     (cons (map first ss) (step (map rest ss)))))))]
     (pmap #(apply f %) (step (cons coll colls))))))

(defn pcalls
  "Executes the no-arg fns in parallel, returning a lazy sequence of
  their values"
  {:added "1.0"
   :static true}
  [& fns] (pmap #(%) fns))

(defmacro pvalues
  "Returns a lazy sequence of the values of the exprs, which are
  evaluated in parallel"
  {:added "1.0"
   :static true}
  [& exprs]
  `(pcalls ~@(map #(list `fn [] %) exprs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; clojure version number ;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *clojure-version*
  (let [properties (second (application/get_all_key.e :clojerl))
        vsn        (proplists/get_value.e :vsn properties)
        version-string (erlang/list_to_binary.e vsn)
        [_ major minor incremental qualifier snapshot]
        (re-matches
         #"(\d+)\.(\d+)\.(\d+)(?:-([a-zA-Z0-9_]+))?(?:-(SNAPSHOT))?"
         version-string)
        clojure-version {:major       (erlang/binary_to_integer.e major)
                         :minor       (erlang/binary_to_integer.e minor)
                         :incremental (erlang/binary_to_integer.e incremental)
                         :qualifier   (if (= qualifier "SNAPSHOT") nil qualifier)}]
    (if (clojerl.String/contains.e version-string "SNAPSHOT")
      (assoc clojure-version :interim true)
      clojure-version)))

(add-doc-and-meta *clojure-version*
  "The version info for Clojure core, as a map containing :major :minor
  :incremental and :qualifier keys. Feature releases may increment
  :minor and/or :major, bugfix releases will increment :incremental.
  Possible values of :qualifier include \"GA\", \"SNAPSHOT\", \"RC-x\" \"BETA-x\""
  {:added "1.0"})

(defn
  clojure-version
  "Returns clojure version as a printable string."
  {:added "1.0"}
  []
  (str (:major *clojure-version*)
       "."
       (:minor *clojure-version*)
       (when-let [i (:incremental *clojure-version*)]
         (str "." i))
       (when-let [q (:qualifier *clojure-version*)]
         (when (pos? (count q)) (str "-" q)))
       (when (:interim *clojure-version*)
         "-SNAPSHOT")))

(defn promise
  "Returns a promise object that can be read with deref/@, and set,
  once only, with deliver. Calls to deref/@ prior to delivery will
  block, unless the variant of deref with timeout is used. All
  subsequent derefs will return the same delivered value without
  blocking. See also - realized?."
  {:added "1.1"
   :static true}
  []
  (throw "unimplemented promise")
  #_(let [d (java.util.concurrent.CountDownLatch. 1)
        v (atom d)]
    (reify
     clojure.lang.IDeref
       (deref [_] (.await d) @v)
     clojure.lang.IBlockingDeref
       (deref
        [_ timeout-ms timeout-val]
        (if (.await d timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)
          @v
          timeout-val))
     clojure.lang.IPending
      (isRealized [this]
       (zero? (.getCount d)))
     clojure.lang.IFn
     (invoke
      [this x]
      (when (and (pos? (.getCount d))
                 (compare-and-set! v d x))
        (.countDown d)
        this)))))

(defn deliver
  "Delivers the supplied value to the promise, releasing any pending
  derefs. A subsequent call to deliver on a promise will have no effect."
  {:added "1.1"
   :static true}
  [promise val]
  (throw "unimplemented promise")
  #_(promise val))


(defn flatten
  "Takes any nested combination of sequential things (lists, vectors,
  etc.) and returns their contents as a single, flat sequence.
  (flatten nil) returns an empty sequence."
  {:added "1.2"
   :static true}
  [x]
  (filter (complement sequential?)
          (rest (tree-seq sequential? seq x))))

(defn group-by
  "Returns a map of the elements of coll keyed by the result of
  f on each element. The value at each key will be a vector of the
  corresponding elements, in the order they appeared in coll."
  {:added "1.2"
   :static true}
  [f coll]
  (reduce1
   (fn [ret x]
     (let [k (f x)]
       (assoc ret k (conj (get ret k []) x))))
   {} coll))

(defn partition-by
  "Applies f to each value in coll, splitting it each time f returns a
   new value.  Returns a lazy seq of partitions.  Returns a stateful
   transducer when no collection is provided."
  {:added "1.2"
   :static true}
  ([f]
   #_(fn [rf]
    (let [a (java.util.ArrayList.)
          pv (volatile! ::none)]
      (fn
        ([] (rf))
        ([result]
           (let [result (if (.isEmpty a)
                          result
                          (let [v (vec (.toArray a))]
                            ;;clear first!
                            (.clear a)
                            (unreduced (rf result v))))]
             (rf result)))
        ([result input]
           (let [pval @pv
                 val (f input)]
             (vreset! pv val)
             (if (or (identical? pval ::none)
                     (= val pval))
               (do
                 (.add a input)
                 result)
               (let [v (vec (.toArray a))]
                 (.clear a)
                 (let [ret (rf result v)]
                   (when-not (reduced? ret)
                     (.add a input))
                   ret)))))))))
  ([f coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (let [fst (first s)
              fv (f fst)
              run (cons fst (take-while #(= fv (f %)) (next s)))]
          (cons run (partition-by f (seq (drop (count run) s)))))))))

(defn frequencies
  "Returns a map from distinct items in coll to the number of times
  they appear."
  {:added "1.2"
   :static true}
  [coll]
  (reduce (fn [counts x]
            (assoc counts x (inc (get counts x 0))))
          {} coll))

(defn reductions
  "Returns a lazy seq of the intermediate values of the reduction (as
  per reduce) of coll by f, starting with init."
  {:added "1.2"}
  ([f coll]
     (lazy-seq
      (if-let [s (seq coll)]
        (reductions f (first s) (rest s))
        (list (f)))))
  ([f init coll]
     (if (reduced? init)
       (list @init)
       (cons init
             (lazy-seq
              (when-let [s (seq coll)]
                (reductions f (f init (first s)) (rest s))))))))

(defn rand-nth
  "Return a random element of the (sequential) collection. Will have
  the same performance characteristics as nth for the given
  collection."
  {:added "1.2"
   :static true}
  [coll]
  (nth coll (rand-int (count coll))))

(defn partition-all
  "Returns a lazy sequence of lists like partition, but may include
  partitions with fewer than n items at the end.  Returns a stateful
  transducer when no collection is provided."
  {:added "1.2"
   :static true}
  #_([^long n]
   (fn [rf]
     (let [a (java.util.ArrayList. n)]
       (fn
         ([] (rf))
         ([result]
            (let [result (if (.isEmpty a)
                           result
                           (let [v (vec (.toArray a))]
                             ;;clear first!
                             (.clear a)
                             (unreduced (rf result v))))]
              (rf result)))
         ([result input]
            (.add a input)
            (if (= n (.size a))
              (let [v (vec (.toArray a))]
                (.clear a)
                (rf result v))
              result))))))
  ([n coll]
     (partition-all n n coll))
  ([n step coll]
     (lazy-seq
      (when-let [s (seq coll)]
        (let [seg (doall (take n s))]
          (cons seg (partition-all n step (nthrest s step))))))))

(defn shuffle
  "Return a random permutation of coll"
  {:added "1.2"
   :static true}
  [^java.util.Collection coll]
  (throw "unimplemented shuffle")
  #_(let [al (java.util.ArrayList. coll)]
      (java.util.Collections/shuffle al)
      (clojure.lang.RT/vector (.toArray al))))

(defn map-indexed
  "Returns a lazy sequence consisting of the result of applying f to 0
  and the first item of coll, followed by applying f to 1 and the second
  item in coll, etc, until coll is exhausted. Thus function f should
  accept 2 arguments, index and item. Returns a stateful transducer when
  no collection is provided."
  {:added "1.2"
   :static true}
  ([f]
   #_(fn [rf]
     (let [i (volatile! -1)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (rf result (f (vswap! i inc) input)))))))
  ([f coll]
   (throw "unimplemented letfn*")
   #_(letfn [(mapi [idx coll]
                 (lazy-seq
                   (when-let [s (seq coll)]
                     (if (chunked-seq? s)
                       (let [c (chunk-first s)
                             size (int (count c))
                             b (chunk-buffer size)]
                         (dotimes [i size]
                           (chunk-append b (f (+ idx i) (.nth c i))))
                         (chunk-cons (chunk b) (mapi (+ idx size) (chunk-rest s))))
                       (cons (f idx (first s)) (mapi (inc idx) (rest s)))))))]
     (mapi 0 coll))))

(defn keep
  "Returns a lazy sequence of the non-nil results of (f item). Note,
  this means false return values will be included.  f must be free of
  side-effects.  Returns a transducer when no collection is provided."
  {:added "1.2"
   :static true}
  ([f]
   (fn [rf]
     (fn
       ([] (rf))
       ([result] (rf result))
       ([result input]
          (let [v (f input)]
            (if (nil? v)
              result
              (rf result v)))))))
  ([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      #_(if (chunked-seq? s)
        (let [c (chunk-first s)
              size (count c)
              b (chunk-buffer size)]
          (dotimes [i size]
            (let [x (f (nth c i))]
              (when-not (nil? x)
                (chunk-append b x))))
          (chunk-cons (chunk b) (keep f (chunk-rest s)))))
      (let [x (f (first s))]
        (if (nil? x)
          (keep f (rest s))
          (cons x (keep f (rest s)))))))))

(defn every-pred
  "Takes a set of predicates and returns a function f that returns true if all of its
  composing predicates return a logical true value against all of its arguments, else it returns
  false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical false result against the original predicates."
  {:added "1.3"}
  ([p]
     (fn ep1
       ([] true)
       ([x] (boolean (p x)))
       ([x y] (boolean (and (p x) (p y))))
       ([x y z] (boolean (and (p x) (p y) (p z))))
       ([x y z & args] (boolean (and (ep1 x y z)
                                     (every? p args))))))
  ([p1 p2]
     (fn ep2
       ([] true)
       ([x] (boolean (and (p1 x) (p2 x))))
       ([x y] (boolean (and (p1 x) (p1 y) (p2 x) (p2 y))))
       ([x y z] (boolean (and (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z))))
       ([x y z & args] (boolean (and (ep2 x y z)
                                     (every? #(and (p1 %) (p2 %)) args))))))
  ([p1 p2 p3]
     (fn ep3
       ([] true)
       ([x] (boolean (and (p1 x) (p2 x) (p3 x))))
       ([x y] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y))))
       ([x y z] (boolean (and (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z))))
       ([x y z & args] (boolean (and (ep3 x y z)
                                     (every? #(and (p1 %) (p2 %) (p3 %)) args))))))
  ([p1 p2 p3 & ps]
     (let [ps (list* p1 p2 p3 ps)]
       (fn epn
         ([] true)
         ([x] (every? #(% x) ps))
         ([x y] (every? #(and (% x) (% y)) ps))
         ([x y z] (every? #(and (% x) (% y) (% z)) ps))
         ([x y z & args] (boolean (and (epn x y z)
                                       (every? #(every? % args) ps))))))))

(defn some-fn
  "Takes a set of predicates and returns a function f that returns the first logical true value
  returned by one of its composing predicates against any of its arguments, else it returns
  logical false. Note that f is short-circuiting in that it will stop execution on the first
  argument that triggers a logical true result against the original predicates."
  {:added "1.3"}
  ([p]
     (fn sp1
       ([] nil)
       ([x] (p x))
       ([x y] (or (p x) (p y)))
       ([x y z] (or (p x) (p y) (p z)))
       ([x y z & args] (or (sp1 x y z)
                           (some p args)))))
  ([p1 p2]
     (fn sp2
       ([] nil)
       ([x] (or (p1 x) (p2 x)))
       ([x y] (or (p1 x) (p1 y) (p2 x) (p2 y)))
       ([x y z] (or (p1 x) (p1 y) (p1 z) (p2 x) (p2 y) (p2 z)))
       ([x y z & args] (or (sp2 x y z)
                           (some #(or (p1 %) (p2 %)) args)))))
  ([p1 p2 p3]
     (fn sp3
       ([] nil)
       ([x] (or (p1 x) (p2 x) (p3 x)))
       ([x y] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y)))
       ([x y z] (or (p1 x) (p2 x) (p3 x) (p1 y) (p2 y) (p3 y) (p1 z) (p2 z) (p3 z)))
       ([x y z & args] (or (sp3 x y z)
                           (some #(or (p1 %) (p2 %) (p3 %)) args)))))
  ([p1 p2 p3 & ps]
     (let [ps (list* p1 p2 p3 ps)]
       (fn spn
         ([] nil)
         ([x] (some #(% x) ps))
         ([x y] (some #(or (% x) (% y)) ps))
         ([x y z] (some #(or (% x) (% y) (% z)) ps))
         ([x y z & args] (or (spn x y z)
                             (some #(some % args) ps)))))))

(defn- ^{:dynamic true} assert-valid-fdecl
  "A good fdecl looks like (([a] ...) ([a b] ...)) near the end of defn."
  [fdecl]
  (when (empty? fdecl) (throw "Parameter declaration missing"))
  (let [argdecls (map
                   #(if (seq? %)
                      (first %)
                      (throw (if (seq? (first fdecl))
                               (str "Invalid signature \""
                                    %
                                    "\" should be a list")
                               (str "Parameter declaration \""
                                    %
                                    "\" should be a vector"))))
                   fdecl)
        bad-args (seq (remove #(vector? %) argdecls))]
    (when bad-args
      (throw (str "Parameter declaration \"" (first bad-args)
                  "\" should be a vector")))))

(defn with-redefs-fn
  "Temporarily redefines Vars during a call to func.  Each val of
  binding-map will replace the root value of its key which must be
  a Var.  After func is called with no args, the root values of all
  the Vars will be set back to their old values.  These temporary
  changes will be visible in all threads.  Useful for mocking out
  functions during testing."
  {:added "1.3"}
  [binding-map func]
  (throw "unimplemented with-redefs-fn")
  #_(let [root-bind (fn [m]
                    (doseq [[a-var a-val] m]
                      (.bindRoot ^clojure.lang.Var a-var a-val)))
        old-vals (zipmap (keys binding-map)
                         (map #(.getRawRoot ^clojure.lang.Var %) (keys binding-map)))]
    (try
      (root-bind binding-map)
      (func)
      (finally
        (root-bind old-vals)))))

(defmacro with-redefs
  "binding => var-symbol temp-value-expr

  Temporarily redefines Vars while executing the body.  The
  temp-value-exprs will be evaluated and each resulting value will
  replace in parallel the root value of its Var.  After the body is
  executed, the root values of all the Vars will be set back to their
  old values.  These temporary changes will be visible in all threads.
  Useful for mocking out functions during testing."
  {:added "1.3"}
  [bindings & body]
  `(with-redefs-fn ~(zipmap (map #(list `var %) (take-nth 2 bindings))
                            (take-nth 2 (next bindings)))
                    (fn [] ~@body)))

(defn realized?
  "Returns true if a value has been produced for a promise, delay, future or lazy sequence."
  {:added "1.3"}
  [^clojure.lang.IPending x]
  (throw "unimplemented realized?")
  #_(.isRealized x))

(defmacro cond->
  "Takes an expression and a set of test/form pairs. Threads expr (via ->)
  through each form for which the corresponding test
  expression is true. Note that, unlike cond branching, cond-> threading does
  not short circuit after the first true test expression."
  {:added "1.5"}
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]] `(if ~test (-> ~g ~step) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro cond->>
  "Takes an expression and a set of test/form pairs. Threads expr (via ->>)
  through each form for which the corresponding test expression
  is true.  Note that, unlike cond branching, cond->> threading does not short circuit
  after the first true test expression."
  {:added "1.5"}
  [expr & clauses]
  (assert (even? (count clauses)))
  (let [g (gensym)
        steps (map (fn [[test step]] `(if ~test (->> ~g ~step) ~g))
                   (partition 2 clauses))]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro as->
  "Binds name to expr, evaluates the first form in the lexical context
  of that binding, then binds name to that result, repeating for each
  successive form, returning the result of the last form."
  {:added "1.5"}
  [expr name & forms]
  `(let [~name ~expr
         ~@(interleave (repeat name) (butlast forms))]
     ~(if (empty? forms)
        name
        (last forms))))

(defmacro some->
  "When expr is not nil, threads it into the first form (via ->),
  and when that result is not nil, through the next etc"
  {:added "1.5"}
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (-> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defmacro some->>
  "When expr is not nil, threads it into the first form (via ->>),
  and when that result is not nil, through the next etc"
  {:added "1.5"}
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (nil? ~g) nil (->> ~g ~step)))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(defn ^:private preserving-reduced
  [rf]
  #(let [ret (rf %1 %2)]
     (if (reduced? ret)
       (reduced ret)
       ret)))

(defn cat
  "A transducer which concatenates the contents of each input, which must be a
  collection, into the reduction."
  {:added "1.7"}
  [rf]
  (let [rrf (preserving-reduced rf)]
    (fn
      ([] (rf))
      ([result] (rf result))
      ([result input]
         (reduce rrf result input)))))

(defn dedupe
  "Returns a lazy sequence removing consecutive duplicates in coll.
  Returns a transducer when no collection is provided."
  {:added "1.7"}
  ([]
   #_(fn [rf]
     (let [pv (volatile! ::none)]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
            (let [prior @pv]
              (vreset! pv input)
              (if (= prior input)
                result
                (rf result input))))))))
  ;; TODO: this expression is generating invalid AST
  #_([coll] (sequence (dedupe) coll)))

(defn random-sample
  "Returns items from coll with random probability of prob (0.0 -
  1.0).  Returns a transducer when no collection is provided."
  {:added "1.7"}
  ([prob]
     (filter (fn [_] (< (rand) prob))))
  ([prob coll]
     (filter (fn [_] (< (rand) prob)) coll)))

(deftype Eduction [xform coll]
  ;; Iterable
  ;; (iterator [_]
  ;;   (clojure.lang.TransformerIterator/create xform (clojure.lang.RT/iter coll)))

  ;; TODO: this expression is generating invalid AST
  ;; clojerl.IReduce
  ;; (reduce [this f init]
  ;;   ;; NB (completing f) isolates completion of inner rf from outer rf
  ;;   (transduce xform (completing f) init coll))

  clojerl.ISequential)

(defn eduction
  "Returns a reducible/iterable application of the transducers
  to the items in coll. Transducers are applied in order as if
  combined with comp. Note that these applications will be
  performed every time reduce/iterator is called."
  {:arglists '([xform* coll])
   :added "1.7"}
  [& xforms]
  (new Eduction (apply comp (butlast xforms)) (last xforms)))

(defmethod print-method Eduction [c, ^Writer w]
  (if *print-readably*
    (do
      (print-sequential "(" pr-on " " ")" c w))
    (print-simple c w)))

(defn run!
  "Runs the supplied procedure (via reduce), for purposes of side
  effects, on successive items in the collection. Returns nil"
  {:added "1.7"}
  [proc coll]
  (reduce #(proc %2) nil coll)
  nil)


(defn tagged-literal?
  "Return true if the value is the data representation of a tagged literal"
  {:added "1.7"}
  [value]
  (instance? clojerl.reader.TaggedLiteral value))

(defn tagged-literal
  "Construct a data representation of a tagged literal from a
  tag symbol and a form."
  {:added "1.7"}
  [^clojerl.Symbol tag form]
  (new clojerl.reader.TaggedLiteral tag form))

(defn reader-conditional?
  "Return true if the value is the data representation of a reader conditional"
  {:added "1.7"}
  [value]
  (instance? clojerl.reader.ReaderConditional value))

(defn reader-conditional
  "Construct a data representation of a reader conditional.
  If true, splicing? indicates read-cond-splicing."
  {:added "1.7"}
  [form ^clojerl.Boolean splicing?]
  (new clojerl.reader.ReaderConditional form splicing?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; data readers ;;;;;;;;;;;;;;;;;;

(def ^{:added "1.4"} default-data-readers
  "Default map of data reader functions provided by Clojure. May be
  overridden by binding *data-readers*."
  {'inst #'clojure.instant/read-instant-date
   'uuid #'clojure.uuid/default-uuid-reader})

(def ^{:added "1.4" :dynamic true} *data-readers*
  "Map from reader tag symbols to data reader Vars.

  When Clojure starts, it searches for files named 'data_readers.clj'
  at the root of the classpath. Each such file must contain a literal
  map of symbols, like this:

      {foo/bar my.project.foo/bar
       foo/baz my.project/baz}

  The first symbol in each pair is a tag that will be recognized by
  the Clojure reader. The second symbol in the pair is the
  fully-qualified name of a Var which will be invoked by the reader to
  parse the form following the tag. For example, given the
  data_readers.clj file above, the Clojure reader would parse this
  form:

      #foo/bar [1 2 3]

  by invoking the Var #'my.project.foo/bar on the vector [1 2 3]. The
  data reader function is invoked on the form AFTER it has been read
  as a normal Clojure data structure by the reader.

  Reader tags without namespace qualifiers are reserved for
  Clojure. Default reader tags are defined in
  clojure.core/default-data-readers but may be overridden in
  data_readers.clj or by rebinding this Var."
  {})

(def ^{:added "1.5" :dynamic true} *default-data-reader-fn*
  "When no data reader is found for a tag and *default-data-reader-fn*
  is non-nil, it will be called with two arguments,
  the tag and the value.  If *default-data-reader-fn* is nil (the
  default), an exception will be thrown for the unknown tag."
  nil)

;; (defn- data-reader-urls []
;;   (let [cl (.. Thread currentThread getContextClassLoader)]
;;     (concat
;;       (enumeration-seq (.getResources cl "data_readers.clj"))
;;       (enumeration-seq (.getResources cl "data_readers.cljc")))))

;; (defn- data-reader-var [sym]
;;   (intern (create-ns (symbol (namespace sym)))
;;           (symbol (name sym))))

;; (defn- load-data-reader-file [mappings ^java.net.URL url]
;;   (with-open [rdr (clojure.lang.LineNumberingPushbackReader.
;;                    (java.io.InputStreamReader.
;;                     (.openStream url) "UTF-8"))]
;;     (binding [*file* (.getFile url)]
;;       (let [read-opts (if (.endsWith (.getPath url) "cljc")
;;                         {:eof nil :read-cond :allow}
;;                         {:eof nil})
;;             new-mappings (read read-opts rdr)]
;;         (when (not (map? new-mappings))
;;           (throw (ex-info (str "Not a valid data-reader map")
;;                           {:url url})))
;;         (reduce
;;          (fn [m [k v]]
;;            (when (not (symbol? k))
;;              (throw (ex-info (str "Invalid form in data-reader file")
;;                              {:url url
;;                               :form k})))
;;            (let [v-var (data-reader-var v)]
;;              (when (and (contains? mappings k)
;;                         (not= (mappings k) v-var))
;;                (throw (ex-info "Conflicting data-reader mapping"
;;                                {:url url
;;                                 :conflict k
;;                                 :mappings m})))
;;              (assoc m k v-var)))
;;          mappings
;;          new-mappings)))))

;; (defn- load-data-readers []
;;   (alter-var-root #'*data-readers*
;;                   (fn [mappings]
;;                     (reduce load-data-reader-file
;;                             mappings (data-reader-urls)))))

;; (try
;;  (load-data-readers)
;;  (catch Throwable t
;;    (.printStackTrace t)
;;    (throw t)))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(defmacro simple-benchmark
  "Runs expr iterations times in the context of a let expression with
  the given bindings, then prints out the bindings and the expr
  followed by number of iterations and total time. The optional
  argument print-fn, defaulting to println, sets function used to
  print the result. expr's string representation will be produced
  using pr-str in any case."
  {:added "1.0"}
  [bindings expr iterations & {:keys [print-fn] :or {print-fn 'println}}]
  (let [bs-str   (pr-str bindings)
        expr-str (pr-str expr)]
    `(let ~bindings
       (let [start#   (erlang/monotonic_time.e :nano_seconds)
             ret#     (dotimes [_# ~iterations] ~expr)
             end#     (erlang/monotonic_time.e :nano_seconds)
             elapsed# (/ (- end# start#) 1000000)]
         (~print-fn (str ~bs-str ", " ~expr-str ", "
                      ~iterations " runs, " elapsed# " msecs"))))))
