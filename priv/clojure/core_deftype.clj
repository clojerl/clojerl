;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; definterface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn namespace-munge
  "Convert a Clojure namespace name to a legal Java package name."
  {:added "1.2"}
  [ns]
  (binary/replace.e (-> ns ns-name str) \- \_))

(defn munge [s] s)

;for now, built on gen-interface
(defmacro definterface
  "Creates a new Java interface with the given name and method sigs.
  The method return types and parameter types may be specified with type hints,
  defaulting to Object if omitted.
  (definterface MyInterface
    (^int method1 [x])
    (^Bar method2 [^Baz b ^Quux q]))"
  {:added "1.2"} ;; Present since 1.2, but made public in 1.5.
  [name & sigs]
  (throw "unsupported definterface"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; reify/deftype ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- parse-opts [s]
  (loop [opts {} [k v & rs :as s] s]
    (if (keyword? k)
      (recur (assoc opts k v) rs)
      [opts s])))

(defn- parse-impls [specs]
  (loop [ret {} s specs]
    (if (seq s)
      (recur (assoc ret (first s) (take-while seq? (next s)))
             (drop-while seq? (next s)))
      ret)))

(defn- parse-opts+specs [opts+specs]
  (let [[opts specs] (parse-opts opts+specs)
        impls (parse-impls specs)
        interfaces (-> (map #(if (var? (resolve %))
                               (:on (deref (resolve %)))
                               %)
                            (keys impls))
                       set
                       (disj 'Object 'java.lang.Object)
                       vec)
        methods (map (fn [[name params & body]]
                       (cons name (maybe-destructured params body)))
                     (apply concat (vals impls)))]
    (when-let [bad-opts (seq (remove #{:no-print :load-ns} (keys opts)))]
      (throw (apply print-str "Unsupported option(s) -" bad-opts)))
    [interfaces methods opts]))

(defn- imap-cons
  [this o]
  (cond
   (map-entry? o)
     (assoc this (first o) (second o))
   (vector? o)
     (assoc this (nth o 0) (nth o 1))
   :else (loop [this this
                o o]
      (if (seq o)
        (let [pair (first o)]
          (recur (assoc this (first pair) (second pair)) (rest o)))
        this))))

(defn- emit-defrecord
  "Do not use this directly - use defrecord"
  {:added "1.2"}
  [tagname cname fields interfaces methods opts]
  (let [classname (with-meta (symbol (str (namespace-munge *ns*) "." cname)) (meta cname))
        interfaces (vec interfaces)
        interface-set (set (map resolve interfaces))
        methodname-set (set (map first methods))
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        base-fields fields
        fields (conj fields '__meta '__extmap)
        type-hash (hash classname)]
    (let [gs (gensym)
          irecord (fn [[i m]]
                    [(conj i 'clojerl.IRecord)
                     (conj m '(_ [_]))])
          eqhash (fn [[i m]]
                   [(conj i 'clojerl.IHash)
                    (conj m
                          `(~'hash [this#] 1))])
          iobj (fn [[i m]]
                 [(conj i 'clojerl.IMeta)
                  (conj m
                        `(~'meta [this#] ~'__meta)
                        `(~'with_meta [this# ~gs] (new ~classname ~@(replace {'__meta gs} fields))))])
          ilookup (fn [[i m]]
                    [(conj i 'clojerl.ILookup)
                     (conj m
                           `(~'get [this# k#] (get this# k# nil))
                           `(~'get [this# k# else#]
                             (case k# ~@(mapcat (fn [fld] [(keyword fld) fld])
                                                base-fields)
                                   (get ~'__extmap k# else#))))])
          imap (fn [[i m]]
                 [(conj i
                        'clojerl.Counted 'clojerl.IColl 'clojerl.IEquiv
                        'clojerl.Associative 'clojerl.Seqable 'clojerl.IMap)
                  (conj m
                        `(~'count [this#] (+ ~(count base-fields) (count ~'__extmap)))
                        `(~'empty [this#] (throw (str "Can't create empty: " ~(str classname))))
                        `(~'cons [this# e#] ((var imap-cons) this# e#))
                        `(~'equiv [this# ~gs]
                          (boolean
                           (or (identical? this# ~gs)
                               (when (identical? (class this#) (class ~gs))
                                 (let [~gs ~(with-meta gs {:tag classname})]
                                   (and  ~@(map (fn [fld] `(= ~fld (~(symbol (str classname) (str "-" fld ".e")) ~gs))) base-fields)
                                         (= ~'__extmap (~(symbol (str classname) "-__extmap.e") ~gs))))))))
                        `(~'contains_key [this# k#] (not (identical? this# (get this# k# this#))))
                        `(~'entry_at [this# k#] (let [v# (get this# k# this#)]
                                                  (when-not (identical? this# v#)
                                                    [k# v#])))
                        `(~'assoc [this# k# ~gs]
                          (condp identical? k#
                            ~@(mapcat (fn [fld]
                                        [(keyword fld) (list* `new classname (replace {fld gs} fields))])
                                      base-fields)
                            (new ~classname ~@(remove #{'__extmap} fields) (assoc ~'__extmap k# ~gs))))
                        `(~'seq [this#] (seq (concat [~@(map #(vector (keyword %) %) base-fields)]
                                                     ~'__extmap)))
                        `(~'to_list [this#]
                          ;; TODO: Improve this implementation
                          (clojerl.Seqable/to_list.e (concat [~@(map #(vector (keyword %) %) base-fields)]
                                                             ~'__extmap)))
                        `(~'keys [this#] (map first (seq this#)))
                        `(~'vals [this#] (map second (seq this#)))
                        `(~'without [this# k#] (if (contains? #{~@(map keyword base-fields)} k#)
                                                 (dissoc (with-meta (into {} this#) ~'__meta) k#)
                                                 (new ~classname ~@(remove #{'__extmap} fields)
                                                      (not-empty (dissoc ~'__extmap k#))))))])
          istr (fn [[i m]]
                 [(conj i 'clojerl.Stringable)
                  (conj m
                        `(~'str [this#]
                          (str "#"
                               ~(str classname)
                               (merge ~'__extmap
                                      (hash-map ~@(mapcat (juxt keyword identity)
                                                          base-fields))))))])]
      (let [[i m] (-> [interfaces methods] irecord eqhash iobj ilookup imap istr)]
        `(deftype* ~(symbol (name (ns-name *ns*)) (name tagname)) ~classname ~(conj hinted-fields '__meta '__extmap)
           :implements ~(vec i)
           ~@(mapcat identity opts)
           ~@m)))))

(defn- build-positional-factory
  "Used to build a positional factory for a given type/record.  Because of the
  limitation of 20 arguments to Clojure functions, this factory needs to be
  constructed to deal with more arguments.  It does this by building a straight
  forward type/record ctor call in the <=20 case, and a call to the same
  ctor pulling the extra args out of the & overage parameter.  Finally, the
  arity is constrained to the number of expected fields and an ArityException
  will be thrown at runtime if the actual arg count does not match."
  [nom classname fields]
  (let [fn-name (symbol (str '-> nom))
        [field-args over] (split-at 20 fields)
        field-count (count fields)
        arg-count (count field-args)
        over-count (count over)
        docstring (str "Positional factory function for class " classname ".")]
    `(defn ~fn-name
       ~docstring
       [~@field-args ~@(if (seq over) '[& overage] [])]
       ~(if (seq over)
          `(if (= (count ~'overage) ~over-count)
             (new ~classname
                  ~@field-args
                  ~@(for [i (range 0 (count over))]
                      (list `nth 'overage i)))
             (throw (str "Arity exception: " (+ ~arg-count (count ~'overage)) (name '~fn-name))))
          `(new ~classname ~@field-args)))))

(defn- validate-fields
  ""
  [fields name]
  (when-not (vector? fields)
    (throw "No fields vector given."))
  (let [specials #{'__meta '__extmap}]
    (when (some specials fields)
      (throw (str "The names in " specials " cannot be used as field names for types or records."))))
  (let [non-syms (remove symbol? fields)]
    (when (seq non-syms)
      (throw (str "defrecord and deftype fields must be symbols, "
                  *ns* "." name " had: "
                  (apply str (interpose ", " non-syms)))))))

(defmacro defrecord
  "(defrecord name [fields*]  options* specs*)

  Options are expressed as sequential keywords and arguments (in any order).

  Supported options:
  :load-ns - if true, importing the record class will cause the
             namespace in which the record was defined to be loaded.
             Defaults to false.

  Each spec consists of a protocol or interface name followed by zero
  or more method bodies:

  protocol-or-interface-or-Object
  (methodName [args*] body)*

  Dynamically generates compiled bytecode for class with the given
  name, in a package with the same name as the current namespace, the
  given fields, and, optionally, methods for protocols and/or
  interfaces.

  The class will have the (immutable) fields named by
  fields, which can have type hints. Protocols/interfaces and methods
  are optional. The only methods that can be supplied are those
  declared in the protocols/interfaces.  Note that method bodies are
  not closures, the local environment includes only the named fields,
  and those fields can be accessed directly.

  Method definitions take the form:

  (methodname [args*] body)

  The argument and return types can be hinted on the arg and
  methodname symbols. If not supplied, they will be inferred, so type
  hints should be reserved for disambiguation.

  Methods should be supplied for all methods of the desired
  protocol(s) and interface(s). You can also define overrides for
  methods of Object. Note that a parameter must be supplied to
  correspond to the target object ('this' in Java parlance). Thus
  methods for interfaces will take one more argument than do the
  interface declarations. Note also that recur calls to the method
  head should *not* pass the target object, it will be supplied
  automatically and can not be substituted.

  In the method bodies, the (unqualified) name can be used to name the
  class (for calls to new, instance? etc).

  The class will have implementations of several (clojure.lang)
  interfaces generated automatically: IObj (metadata support) and
  IPersistentMap, and all of their superinterfaces.

  In addition, defrecord will define type-and-value-based =,
  and will defined Java .hashCode and .equals consistent with the
  contract for java.util.Map.

  When AOT compiling, generates compiled bytecode for a class with the
  given name (a symbol), prepends the current ns as the package, and
  writes the .class file to the *compile-path* directory.

  Two constructors will be defined, one taking the designated fields
  followed by a metadata map (nil for none) and an extension field
  map (nil for none), and one taking only the fields (using nil for
  meta and extension fields). Note that the field names __meta
  and __extmap are currently reserved and should not be used when
  defining your own records.

  Given (defrecord TypeName ...), two factory functions will be
  defined: ->TypeName, taking positional parameters for the fields,
  and map->TypeName, taking a map of keywords to field values."
  {:added "1.2"
   :arglists '([name [& fields] & opts+specs])}

  [name fields & opts+specs]
  (validate-fields fields name)
  (let [gname name
        [interfaces methods opts] (parse-opts+specs opts+specs)
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." gname))
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))]
    `(let []
       (declare ~(symbol (str  '-> gname)))
       (declare ~(symbol (str 'map-> gname)))
       ~(emit-defrecord name gname (vec hinted-fields) (vec interfaces) methods opts)
       (import ~classname)
       ~(build-positional-factory gname classname fields)
       (defn ~(symbol (str 'map-> gname))
         ~(str "Factory function for class " classname ", taking a map of keywords to field values.")
         ([m#] (~(symbol (str classname) "create.e")
                (if (map? m#) m# (into {} m#)))))
       '~classname)))

(defn record?
  "Returns true if x is a record"
  {:added "1.6"
   :static true}
  [x]
  (satisfies? clojerl.IRecord x))

(defn- emit-deftype*
  "Do not use this directly - use deftype"
  [tagname cname fields interfaces methods opts]
  (let [current-ns (ns-name *ns*)
        classname (with-meta (symbol (str (namespace-munge *ns*) "." cname)) (meta cname))
        interfaces (conj interfaces 'clojerl.IType)
        ;; Dummy method for clojerl.IType
        methods (conj methods '(_ [_]))]
    `(deftype* ~(symbol (name (ns-name *ns*)) (name tagname)) ~classname ~fields
       :implements ~interfaces
       ~@(mapcat identity opts)
       ~@methods)))

(defmacro deftype
  "(deftype name [fields*]  options* specs*)
  Options are expressed as sequential keywords and arguments (in any order).
  Supported options:
  :load-ns - if true, importing the record class will cause the
             namespace in which the record was defined to be loaded.
             Defaults to false.
  Each spec consists of a protocol or interface name followed by zero
  or more method bodies:
  protocol-or-interface-or-Object
  (methodName [args*] body)*
  Dynamically generates compiled bytecode for class with the given
  name, in a package with the same name as the current namespace, the
  given fields, and, optionally, methods for protocols and/or
  interfaces.
  The class will have the (by default, immutable) fields named by
  fields, which can have type hints. Protocols/interfaces and methods
  are optional. The only methods that can be supplied are those
  declared in the protocols/interfaces.  Note that method bodies are
  not closures, the local environment includes only the named fields,
  and those fields can be accessed directly. Fields can be qualified
  with the metadata :volatile-mutable true or :unsynchronized-mutable
  true, at which point (set! afield aval) will be supported in method
  bodies. Note well that mutable fields are extremely difficult to use
  correctly, and are present only to facilitate the building of higher
  level constructs, such as Clojure's reference types, in Clojure
  itself. They are for experts only - if the semantics and
  implications of :volatile-mutable or :unsynchronized-mutable are not
  immediately apparent to you, you should not be using them.
  Method definitions take the form:
  (methodname [args*] body)
  The argument and return types can be hinted on the arg and
  methodname symbols. If not supplied, they will be inferred, so type
  hints should be reserved for disambiguation.
  Methods should be supplied for all methods of the desired
  protocol(s) and interface(s). You can also define overrides for
  methods of Object. Note that a parameter must be supplied to
  correspond to the target object ('this' in Java parlance). Thus
  methods for interfaces will take one more argument than do the
  interface declarations. Note also that recur calls to the method
  head should *not* pass the target object, it will be supplied
  automatically and can not be substituted.
  In the method bodies, the (unqualified) name can be used to name the
  class (for calls to new, instance? etc).
  When AOT compiling, generates compiled bytecode for a class with the
  given name (a symbol), prepends the current ns as the package, and
  writes the .class file to the *compile-path* directory.
  One constructor will be defined, taking the designated fields.  Note
  that the field names __meta and __extmap are currently reserved and
  should not be used when defining your own types.
  Given (deftype TypeName ...), a factory function called ->TypeName
  will be defined, taking positional parameters for the fields"
  {:added "1.2"
   :arglists '([name [& fields] & opts+specs])}

  [name fields & opts+specs]
  (validate-fields fields name)
  (let [gname name
        [interfaces methods opts] (parse-opts+specs opts+specs)
        ns-part (namespace-munge *ns*)
        classname (symbol (str ns-part "." gname))
        hinted-fields fields
        fields (vec (map #(with-meta % nil) fields))
        [field-args over] (split-at 20 fields)]
    `(let []
       ~(emit-deftype* name gname (vec hinted-fields) (vec interfaces) methods opts)
       (import ~classname)
       ~(build-positional-factory gname classname fields)
       ;; Types are not reified so we just return the symbol.
       '~classname)))

;;;;;;;;;;;;;;;;;;;;;;; protocols ;;;;;;;;;;;;;;;;;;;;;;;;

(defn- protocol?
  [maybe-p]
  (and (clj_module/is_clojure.e maybe-p)
       (clj_module/is_protocol.e maybe-p)))

(defn- implements? [protocol atype]
  (satisfies? protocol atype))

#_(defn extenders
  "Returns a collection of the types explicitly extending protocol"
  {:added "1.2"}
  [protocol]
  (keys (:impls protocol)))

(defn- assert-same-protocol [protocol-name method-syms]
  (doseq [m method-syms]
    (let [v (resolve m)
          p (:protocol (meta v))]
      (when (and v (bound? v) (not= protocol-name p))
        (binding [*out* *err*]
          (println "Warning: protocol" protocol-name "is overwriting"
                   (if p
                     (str "method " v " of protocol " p)
                     (str "function " v))))))))

(defn- emit-protocol-function
  [iname {name :name arglists :arglists :as sigs}]
  (map (fn [args]
         `(defn ~(vary-meta name assoc :protocol iname) ~args
            (clojerl.protocol/resolve.e ~(keyword iname)
                                        ~(keyword name)
                                        ~@args)))
       arglists))

(defn- emit-protocol [name opts+sigs]
  (let [iname (symbol (str (munge (namespace-munge *ns*)) "." (munge name)))
        [opts sigs]
        (loop [opts {:on (list 'quote iname) :on-interface iname} sigs opts+sigs]
          (condp #(%1 %2) (first sigs)
            string? (recur (assoc opts :doc (first sigs)) (next sigs))
            keyword? (recur (assoc opts (first sigs) (second sigs)) (nnext sigs))
            [opts sigs]))
        sigs (when sigs
               (reduce1 (fn [m s]
                          (let [name-meta (meta (first s))
                                mname (with-meta (first s) nil)
                                [arglists doc]
                                (loop [as [] rs (rest s)]
                                  (if (vector? (first rs))
                                    (recur (conj as (first rs)) (next rs))
                                    [(seq as) (first rs)]))]
                            (when (some #{0} (map count arglists))
                              (throw (str "Definition of function " mname " in protocol " name " must take at least one arg.")))
                            (when (m (keyword mname))
                              (throw (str "Function " mname " in protocol " name " was redefined. Specify all arities in single definition.")))
                            (assoc m (keyword mname)
                                   (merge name-meta
                                          {:name (vary-meta mname assoc :doc doc :arglists arglists)
                                           :arglists arglists
                                           :doc doc}))))
                        {} sigs))
        meths (mapcat (fn [sig]
                        (let [m (munge (:name sig))]
                          (map #(vector m (count %)) (:arglists sig))))
                      (vals sigs))]
    `(do
       ~(when sigs
          `(#'assert-same-protocol '~iname
                                   '~(clj_core/to_list.e (map :name (vals sigs)))))
       ~@(mapcat (partial emit-protocol-function iname) (vals sigs))
       (~'defprotocol* ~iname ~@meths)
       '~name)))

(defmacro defprotocol
  "A protocol is a named set of named methods and their signatures:
  (defprotocol AProtocolName

    ;optional doc string
    \"A doc string for AProtocol abstraction\"

  ;method signatures
    (bar [this a b] \"bar docs\")
    (baz [this a] [this a b] [this a b c] \"baz docs\"))

  No implementations are provided. Docs can be specified for the
  protocol overall and for each method. The above yields a set of
  polymorphic functions and a protocol object. All are
  namespace-qualified by the ns enclosing the definition The resulting
  functions dispatch on the type of their first argument, which is
  required and corresponds to the implicit target object ('this' in
  Java parlance). defprotocol is dynamic, has no special compile-time
  effect, and defines no new types or classes. Implementations of
  the protocol methods can be provided using extend.

  defprotocol will automatically generate a corresponding interface,
  with the same name as the protocol, i.e. given a protocol:
  my.ns/Protocol, an interface: my.ns.Protocol. The interface will
  have methods corresponding to the protocol functions, and the
  protocol will automatically work with instances of the interface.

  Note that you should not use this interface with deftype or
  reify, as they support the protocol directly:

  (defprotocol P
    (foo [this])
    (bar-me [this] [this y]))

  (deftype Foo [a b c]
   P
    (foo [this] a)
    (bar-me [this] b)
    (bar-me [this y] (+ c y)))

  (bar-me (Foo. 1 2 3) 42)
  => 45

  (foo
    (let [x 42]
      (reify P
        (foo [this] 17)
        (bar-me [this] x)
        (bar-me [this y] x))))
  => 17"
  {:added "1.2"}
  [name & opts+sigs]
  (emit-protocol name opts+sigs))

(defn normalize-methods
  [specs]
  (->> specs
      (map (fn [proto-or-method]
             (cond (symbol? proto-or-method) [proto-or-method]
                   (vector? (second proto-or-method)) [proto-or-method]
                   :else
                   (let [[name & methods] proto-or-method]
                     (map (partial cons name) methods)))))
      (apply concat)))

(defmacro extend-type
  "A macro that expands into an extend call. Useful when you are
  supplying the definitions explicitly inline, extend-type
  automatically creates the maps required by extend.  Propagates the
  class as a type hint on the first argument of all fns.

  (extend-type MyType
    Countable
      (cnt [c] ...)
    Foo
      (bar [x y] ...)
      (baz ([x] ...) ([x y & zs] ...)))"
  {:added "1.2"}
  [t & specs]
  `(extend-type* ~t ~@(normalize-methods specs)))

(defn- emit-extend-protocol [p specs]
  (let [impls (parse-impls specs)]
    `(do
       ~@(map (fn [[t fs]]
                `(extend-type ~t ~p ~@fs))
              impls))))

(defmacro extend-protocol
  "Useful when you want to provide several implementations of the same
  protocol all at once. Takes a single protocol and the implementation
  of that protocol for one or more types. Expands into calls to
  extend-type:

  (extend-protocol Protocol
    AType
      (foo [x] ...)
      (bar [x y] ...)
    BType
      (foo [x] ...)
      (bar [x y] ...)
    AClass
      (foo [x] ...)
      (bar [x y] ...)
    nil
      (foo [x] ...)
      (bar [x y] ...))

  expands into:

  (do
   (clojure.core/extend-type AType Protocol
     (foo [x] ...)
     (bar [x y] ...))
   (clojure.core/extend-type BType Protocol
     (foo [x] ...)
     (bar [x y] ...))
   (clojure.core/extend-type AClass Protocol
     (foo [x] ...)
     (bar [x y] ...))
   (clojure.core/extend-type nil Protocol
     (foo [x] ...)
     (bar [x y] ...)))"
  {:added "1.2"}

  [p & specs]
  (emit-extend-protocol p specs))
