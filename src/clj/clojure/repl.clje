;   Copyright (c) Chris Houser, Dec 2008. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

; Utilities meant to be used interactively at the REPL

(ns
  ^{:author "Chris Houser, Christophe Grand, Stephen Gilardi, Michel Salim"
    :doc "Utilities meant to be used interactively at the REPL"}
  clojure.repl)

(def ^:private special-doc-map
  '{. {:url "erlang_interop#dot"
       :forms [(.instanceMember instance args*)
               (.instanceMember Classname args*)
               (Classname/staticMethod args*)
               Classname/staticField]
       :doc "The instance member form works for both fields and methods.
  They all expand into calls to the dot operator at macroexpansion time."}
    def {:forms [(def symbol doc-string? init?)]
         :doc "Creates and interns a global var with the name
  of symbol in the current namespace (*ns*) or locates such a var if
  it already exists.  If init is supplied, it is evaluated, and the
  root binding of the var is set to the resulting value.  If init is
  not supplied, the root binding of the var is unaffected."}
    do {:forms [(do exprs*)]
        :doc "Evaluates the expressions in order and returns the value of
  the last. If no expressions are supplied, returns nil."}
    if {:forms [(if test then else?)]
        :doc "Evaluates test. If not the singular values nil or false,
  evaluates and yields then, otherwise, evaluates and yields else. If
  else is not supplied it defaults to nil."}
    monitor-enter {:forms [(monitor-enter x)]
                   :doc "Synchronization primitive that should be avoided
  in user code. Use the 'locking' macro."}
    monitor-exit {:forms [(monitor-exit x)]
                  :doc "Synchronization primitive that should be avoided
  in user code. Use the 'locking' macro."}
    new {:forms [(Classname. args*) (new Classname args*)]
         :url "erlang_interop#new"
         :doc "The args, if any, are evaluated from left to right, and
  passed to the constructor of the class named by Classname. The
  constructed object is returned."}
    quote {:forms [(quote form)]
           :doc "Yields the unevaluated form."}
    recur {:forms [(recur exprs*)]
           :doc "Evaluates the exprs in order, then, in parallel, rebinds
  the bindings of the recursion point to the values of the exprs.
  Execution then jumps back to the recursion point, a loop or fn method."}
    set! {:forms[(set! var-symbol expr)
                 (set! (. instance-expr instanceFieldName-symbol) expr)
                 (set! (. Classname-symbol staticFieldName-symbol) expr)]
          :url "vars#set"
          :doc "Used to set thread-local-bound vars."}
    throw {:forms [(throw expr)]
           :doc "The expr is evaluated and thrown, therefore it should
  yield an instance of some derivee of Throwable."}
    try {:forms [(try expr* catch-clause* finally-clause?)]
         :doc "catch-clause => (catch classname name expr*)
  finally-clause => (finally expr*)

  Catches and handles exceptions."}
    var {:forms [(var symbol)]
         :doc "The symbol must resolve to a var, and the Var object
itself (not its value) is returned. The reader macro #'x expands to (var x)."}})

(defn- special-doc [name-symbol]
  (assoc (or (special-doc-map name-symbol) (meta (resolve name-symbol)))
         :name name-symbol
         :special-form true))

(defn- namespace-doc [nspace]
  (assoc (meta nspace) :name (ns-name nspace)))

(defn- print-doc [m]
  (println "-------------------------")
  (println (str (when-let [ns (:ns m)] (str (ns-name ns) "/")) (:name m)))
  (cond
    (:forms m) (doseq [f (:forms m)]
                 (print "  ")
                 (prn f))
    (:arglists m) (prn (:arglists m)))
  (if (:special-form m)
    (do
      (println "Special Form")
      (println " " (:doc m))
      (if (contains? m :url)
        (when (:url m)
          (println (str "\n  Please see http://clojure.org/" (:url m))))
        (println (str "\n  Please see http://clojure.org/special_forms#"
                      (:name m)))))
    (do
      (when (:macro m)
        (println "Macro"))
      (println " " (:doc m)))))

(defn find-doc
  "Prints documentation for any var whose documentation or name
 contains a match for re-string-or-pattern"
  {:added "1.0"}
  [re-string-or-pattern]
    (let [re (re-pattern re-string-or-pattern)
          ms (concat (mapcat #(sort-by :name (map meta (vals (ns-interns %))))
                             (all-ns))
                     (map namespace-doc (all-ns))
                     (map special-doc (keys special-doc-map)))]
      (doseq [m ms
              :when (and (:doc m)
                         (or (re-find re (:doc m))
                             (re-find re (str (:name m)))))]
               (print-doc m))))

(defmacro doc
  "Prints documentation for a var or special form given its name"
  {:added "1.0"}
  [name]
  (if-let [special-name ('{& fn catch try finally try} name)]
    (#'print-doc (#'special-doc special-name))
    (cond
      (special-doc-map name) `(#'print-doc (#'special-doc '~name))
      (find-ns name) `(#'print-doc (#'namespace-doc (find-ns '~name)))
      (resolve name) `(#'print-doc (meta (var ~name))))))

;; ----------------------------------------------------------------------
;; Examine Clojure functions (Vars, really)

(defn- find-file-in-code-path [filename]
  (->> (code/get_path.e)
       (map #(filename/join.e (clj_core/to_list.e [% "**" filename])))
       (map erlang/binary_to_list.1)
       (mapcat filelib/wildcard.1)
       (filter filelib/is_regular.1)
       first
       erlang/list_to_binary.1))

(defn source-fn
  "Returns a string of the source code for the given symbol, if it can
  find it.  This requires that the symbol resolve to a Var defined in
  a namespace for which the .clj is in the classpath.  Returns nil if
  it can't find the source.  For most REPL usage, 'source' is more
  convenient.

  Example: (source-fn 'filter)"
  [x]
  (when-let [v (resolve x)]
    (when-let [filepath (:file (meta v))]
      (when-let [filepath (find-file-in-code-path filepath)]
        (with-open [rdr (erlang.io.File/open.e filepath)]
          (dotimes [_x (dec (:line (meta v)))]
            (erlang.io.IReader/read_line.e rdr))
          (let [text      (new erlang.io.StringWriter "")
                pbr       rdr ;; TODO: this was originally a proxy writing to rdr
                read-opts (if (clojerl.String/ends_with.e filepath "cljc")
                            {:read-cond :allow} {})]
            (if (= :unknown *read-eval*)
              (throw "Unable to read source while *read-eval* is :unknown.")
              (read (new erlang.io.PushbackReader rdr) #_read-opts))
            (str text)))))))

(defmacro source
  "Prints the source code for the given symbol, if it can find it.
  This requires that the symbol resolve to a Var defined in a
  namespace for which the .clj is in the classpath.

  Example: (source filter)"
  [n]
  `(println (or (source-fn '~n) (str "Source not found"))))

(defn apropos
  "Given a regular expression or stringable thing, return a seq of all
public definitions in all currently-loaded namespaces that match the
str-or-pattern."
  [str-or-pattern]
  (let [matches? (if (regex? str-or-pattern)
                   #(re-find str-or-pattern (str %))
                   #(clojerl.String/contains.e (str %) (str str-or-pattern)))]
    (sort (mapcat (fn [ns]
                    (let [ns-name (str ns)]
                      (map #(symbol ns-name (str %))
                           (filter matches? (keys (ns-publics ns))))))
                  (all-ns)))))

(defn dir-fn
  "Returns a sorted seq of symbols naming public vars in
  a namespace"
  [ns]
  (sort (map first (ns-publics (the-ns ns)))))

(defmacro dir
  "Prints a sorted directory of public vars in a namespace"
  [nsname]
  `(doseq [v# (dir-fn '~nsname)]
     (println v#)))

(defn demunge
  "Given a string representation of a fn class,
  as in a stack trace element, returns a readable version."
  {:added "1.3"}
  [fn-name]
  fn-name)

(defn root-cause
  "Returns the initial cause of an exception or error by peeling off all of
  its wrappers"
  {:added "1.3"}
  [st]
  (first st))

(defn info [x k default]
  (let [info (nth x 3)]
    (proplists/get_value.e k info default)))

(defn filename [x]
  (erlang/list_to_binary.e (info x :file
                                 (erlang/binary_to_list.e "NO_SOURCE_FILE"))))

(defn line-number [x]
  (info x :line "?"))

(defn module [x]
  (erlang/atom_to_binary.e (first x) :utf8))

(defn function [x]
  (erlang/atom_to_binary.e (second x) :utf8))

(defn stack-element-str
  "Returns a (possibly unmunged) string representation of a StackTraceElement"
  {:added "1.3"}
  [el]
  (let [file (filename el)
        clojure-fn? (and file (or (clojerl.String/ends_with.e file ".clj")
                                  (clojerl.String/ends_with.e file ".cljc")
                                  (= file "NO_SOURCE_FILE")))]
    (str (if clojure-fn?
           (demunge (module el))
           (str (module el) "/" (function el)))
         " (" (filename el) ":" (line-number el) ")")))

(defn pst
  "Prints a stack trace of the exception, to the depth requested. If none supplied, uses the root cause of the
  most recent repl exception (*e), and a depth of 12."
  {:added "1.3"}
  ([] (pst 12))
  ([e-or-depth]
     (if (number? e-or-depth)
       (when-let [e *e]
         (pst (root-cause e) e-or-depth))
       (pst e-or-depth 12)))
  ([^Throwable e depth]
     (binding [*out* *err*]
       (println (str e
                     (when-let [info (ex-data e)] (str " " (pr-str info)))))
       (let [st    (erlang/get_stacktrace.e)
             cause e]
         (doseq [el st]
           (println (str \tab (stack-element-str el))))
         (when cause
           (println "Caused by:")
           (pst cause (min depth
                           (+ 2 (- (count (erlang/get_stacktrace.e))
                                   (count st))))))))))

;; ----------------------------------------------------------------------
;; Handle Ctrl-C keystrokes

(defn thread-stopper
  "Returns a function that takes one arg and uses that as an exception message
  to stop the given thread.  Defaults to the current thread"
  ([] (thread-stopper (erlang/self.e)))
  ([process] (fn [msg] (erlang/exit.e process msg))))
