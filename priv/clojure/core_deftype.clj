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

(defn- emit-deftype*
  "Do not use this directly - use deftype"
  [tagname cname fields interfaces methods opts]
  (let [current-ns (ns-name *ns*)
        classname (with-meta (symbol (str (namespace-munge *ns*) "." cname)) (meta cname))
        interfaces (conj interfaces 'clojerl.IType)
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
       #_(import ~classname)
       ~(build-positional-factory gname classname fields)
       ;; Types are not reified so we just return the symbol.
       '~classname)))
