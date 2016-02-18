(ns clojure.core)

(def
  ^{:arglists '([& items])
    :doc "Creates a new list containing the items."
    :added "1.0"}
  list (fn* [& items] (clojerl.List/new.e items)))

(def
  ^{:arglists '([& items])
    :doc "Creates a new list containing the items."
    :added "1.0"}
  list* (fn* [items] (clojerl.List/new.e items)))

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
        (let [x (cons 'fn* (clojerl.List/new.e decl))]
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
   :tag cojerl.ISeq
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
   :tag clojure.lang.ISeq
   :added "1.0"
   :static true}
 seq (fn ^:static seq [coll] (clj_core/seq.e coll)))

(def
 ^{:arglists '([p x])
   :doc "Evaluates x and tests if it extends the protocol
    p. Returns true or false"
   :added "1.0"}
 extends? (fn extends? [p x]
            (clj_core/extends?.e p (clj_core/type.e x))))

(def
 ^{:arglists '([t x])
   :doc "Evaluates x and tests if it is of type t.
    Returns true or false"
   :added "1.0"}
 instance? (fn instance? [t x] (erlang/==.e t (clj_core/type.e x))))

(def
 ^{:arglists '([x])
   :doc "Return true if x implements ISeq"
   :added "1.0"
   :static true}
 seq? (fn ^:static seq? [x] (extends? :clojerl.ISeq x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x implements IMeta"
   :added "1.0"
   :static true}
 meta? (fn ^:static meta? [x] (extends? :clojerl.IMeta x)))


#_(def
 ^{:arglists '([x])
   :doc "Return true if x is a Character"
   :added "1.0"
   :static true}
 char? (fn ^:static char? [x] (extends? Character x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x is a String"
   :added "1.0"
   :static true}
 string? (fn ^:static string? [x] (instance? :clojerl.String x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x implements IMap"
   :added "1.0"
   :static true}
 map? (fn ^:static map? [x] (extends? :clojerl.IMap x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x is a Vector"
   :added "1.0"
   :static true}
 vector? (fn ^:static vector? [x] (instance? :clojerl.Vector x)))

(def
 ^{:arglists '([x])
   :doc "Return true if x is a Symbol"
   :added "1.0"
   :static true}
 symbol? (fn ^:static symbol? [x] (instance? :clojerl.Symbol x)))

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

#_(def ^{:private true :dynamic true}
  assert-valid-fdecl (fn [fdecl]))

#_(def
 ^{:private true}
 sigs
 (fn [fdecl]
   (assert-valid-fdecl fdecl)
   (let [asig 
         (fn [fdecl]
           (let [arglist (first fdecl)
                 ;elide implicit macro args
                 arglist (if (clj_utils/equals.e '&form (first arglist)) 
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

(def ^:macro defn
  (fn* [form env name & fdecl]
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
             ;; m     (merge {:arglists (list 'quote (sigs fdecl))} m)
             m     (conj (if (meta name) (meta name) {}) m)]
         (list 'def (with-meta name m)
               (cons 'clojure.core/fn         ;; can't use syntax-quote here yet because
                                              ;; we haven't defined all the necessary functions
                     (list* (seq fdecl)))))))

(defn to-tuple
  "Returns a tuple of Objects containing the contents of coll, which
  can be any Collection.  Maps to erlang:list_to_tuple/2."
  {:tag "clojerl.erlang.Tuple"
   :added "1.0"
   :static true}
  [coll] (erlang/list_to_tuple.e (seq coll)))

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
     (if (extends? :clojerl.IMeta coll)
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
   (clj_core/hash_map.e keyvals)))

(defn hash-set
  "Returns a new hash set with supplied keys.  Any equal keys are
  handled as if by repeated uses of conj."
  {:added "1.0"
   :static true}
  ([] #{})
  ([& keys]
   (clj_core/hash_set.e keys)))

(defn sorted-map
  "keyval => key val
  Returns a new sorted map with supplied mappings.  If any keys are
  equal, they are handled as if by repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([& keyvals]
   (throw :unsupported)
   #_(clojure.lang.PersistentTreeMap/create keyvals)))

(defn sorted-map-by
  "keyval => key val
  Returns a new sorted map with supplied mappings, using the supplied
  comparator.  If any keys are equal, they are handled as if by
  repeated uses of assoc."
  {:added "1.0"
   :static true}
  ([comparator & keyvals]
   (throw :unsupported)
   #_(clojure.lang.PersistentTreeMap/create comparator keyvals)))

(defn sorted-set
  "Returns a new sorted set with supplied keys.  Any equal keys are
  handled as if by repeated uses of conj."
  {:added "1.0"
   :static true}
  ([& keys]
   (throw :unsupported)
   #_(clojure.lang.PersistentTreeSet/create keys)))

(defn sorted-set-by
  "Returns a new sorted set with supplied keys, using the supplied
  comparator.  Any equal keys are handled as if by repeated uses of
  conj."
  {:added "1.1"
   :static true}
  ([comparator & keys]
   (throw :unsupported)
   #_(clojure.lang.PersistentTreeSet/create comparator keys)))

;;;;;;;;;;;;;;;;;;;;

(defn nil?
  "Returns true if x is nil, false otherwise."
  {:tag Boolean
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
             (let [name   (with-meta name (conj (meta name) {:macro true}))
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
  {:tag Boolean,
   :added "1.0"
   :static true}
  [x] (erlang/=:=.e x false))

(defn true?
  "Returns true if x is the value true, false otherwise."
  {:tag Boolean,
   :added "1.0"
   :static true}
  [x] (erlang/=:=.e x true))

(defn not
  "Returns true if x is logical false, false otherwise."
  {:tag Boolean
   :added "1.0"
   :static true}
  [x] (if x false true))

(defn some?
  "Returns true if x is not nil, false otherwise."
  {:tag Boolean
   :added "1.6"
   :static true}
  [x] (not (nil? x)))

(defn str
  "With no args, returns the empty string. With one arg x, returns
  x.toString().  (str nil) returns the empty string. With more than
  one arg, returns the concatenation of the str values of the args."
  {:tag :clojerl.String
   :added "1.0"
   :static true}
  (^:clojerl.String [] "")
  (^:clojerl.String [^Object x]
   (if (nil? x) "" (clj_core/str.e x)))
  (^:clojerl.String [x & ys]
     ((fn [acc more]
          (if more
            (recur (clj_utils/binary_append.e acc (str (first more)))
                   (next more))
            acc))
      (clj_core/str.e x) ys)))

(defn keyword?
  "Return true if x is a Keyword"
  {:added "1.0"
   :static true}
  [x] (instance? :clojerl.Keyword x))

(defn symbol
  "Returns a Symbol with the given namespace and name."
  {:tag clojure.lang.Symbol
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
  {:tag clojure.lang.Keyword
   :added "1.0"
   :static true}
  ([name] (cond (keyword? name) name
                (symbol? name) (clj_core/keyword.e (clj_core/namespace.e name)
                                                   (clj_core/name.e name))
                (string? name) (clj_core/keyword.e name)))
  ([ns name] (clj_core/keyword.e ns name)))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(def prn
  (fn* [x]
       (io/format.e "~s~n" (seq [(str x)]))))

(def apply
    (fn*
     ([f args] (clj_core/invoke.e f (seq args)))
     ([f x args] (clj_core/invoke.e f (cons x (seq args))))
     ([f x y args] (clj_core/invoke.e f (cons x (cons y (seq args)))))
     ([f x y z args] (clj_core/invoke.e f (cons x (cons y (cons z (seq args))))))
     ([f a b c d args] (clj_core/invoke.e f (cons a (cons b (cons c (cons d (seq args)))))))))

(def =
  (fn* [a b] (erlang/==.2 a b)))

(def not
  (fn* [a] (erlang/not.1 a)))

(def assert ^:macro
  (fn* [v] (if (not v) (throw :assert))))

(def <
  (fn* [a b] (erlang/<.e a b)))

#_(


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

  (def vector
    (fn* [& xs] (clj_core/vector.e (seq xs))))

  (def hash-map
    (fn* [& xs] (clj_core/hash_map.e (seq xs))))

  (def hash-set
    (fn* [& xs] (clj_core/hash_set.e (seq xs))))

  (def merge
    (fn* [& xs] (clj_core/merge.e xs)))

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

    [& _args]
    #_(apply load-libs :require args))
  )
