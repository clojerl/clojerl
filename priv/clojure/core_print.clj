;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'clojure.core)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; printing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *print-meta* nil)

(def ^:dynamic
 ^{:doc "*print-length* controls how many items of each collection the
  printer will print. If it is bound to logical false, there is no
  limit. Otherwise, it must be bound to an integer indicating the maximum
  number of items of each collection to print. If a collection contains
  more items, the printer will print items up to the limit followed by
  '...' to represent the remaining items. The root binding is nil
  indicating no limit."
   :added "1.0"}
 *print-length* nil)

(def ^:dynamic
 ^{:doc "*print-level* controls how many levels deep the printer will
  print nested objects. If it is bound to logical false, there is no
  limit. Otherwise, it must be bound to an integer indicating the maximum
  level to print. Each argument to print is at level 0; if an argument is a
  collection, its items are at level 1; and so on. If an object is a
  collection and is at a level greater than or equal to the value bound to
  *print-level*, the printer prints '#' to represent it. The root binding
  is nil indicating no limit."
   :added "1.0"}
 *print-level* nil)

(def ^:dynamic *verbose-defrecords* false)

(defn write
  "Wrap io/fwrite.e"
  [w format & data]
  (io/fwrite.e w format (clj_core/seq_to_list.e data)))

(defn- print-sequential [begin, print-one, sep, end, sequence, w]
  (binding [*print-level* (and (not *print-dup*) *print-level* (dec *print-level*))]
    (if (and *print-level* (neg? *print-level*))
      (write w "#")
      (do
        (write w begin)
        (when-let [xs (seq sequence)]
          (if (and (not *print-dup*) *print-length*)
            (loop [[x & xs] xs
                   print-length *print-length*]
              (if (zero? print-length)
                (write w "...")
                (do
                  (print-one x w)
                  (when xs
                    (write w sep)
                    (recur xs (dec print-length))))))
            (loop [[x & xs] xs]
              (print-one x w)
              (when xs
                (write w sep)
                (recur xs)))))
        (write w end)))))

(defn- print-meta [o, w]
  (when-let [m (meta o)]
    (when (and (pos? (count m))
               (or *print-dup*
                   (and *print-meta* *print-readably*)))
      (write w "^")
      (if (and (= (count m) 1) (:tag m))
          (pr-on (:tag m) w)
          (pr-on m w))
      (write w " "))))

(defn print-simple [o, ^Writer w]
  (print-meta o w)
  (write w (str o)))

(defmethod print-method :default [o, w]
  (print-simple o w))

(defmethod print-method :clojerl.Nil [o, w]
  (write w "nil"))

(defmethod print-dup :clojerl.Nil [o w] (print-method o w))

(defmethod print-method :clojerl.Keyword [o, w]
  (write w (str o)))

(defmethod print-dup :clojerl.Keyword [o w] (print-method o w))

(defmethod print-method :clojerl.Integer [o, w]
  (write w (str o)))

(defmethod print-method :clojerl.Float [o, w]
  (write w (str o)))

(defmethod print-method :clojerl.Boolean [o, w]
  (write w (str o)))

(defmethod print-dup :clojerl.Boolean [o w] (print-method o w))

(defmethod print-method :clojerl.Symbol [o, w]
  (print-simple o w))

(defmethod print-dup :clojerl.Symbol [o w] (print-method o w))

(defmethod print-method :clojerl.Var [o, w]
  (print-simple o w))

(defmethod print-dup :clojerl.Var [o, w]
  (write w (str "#=(var " (namespace o) "/" (name o) ")")))

(defmethod print-method :clojerl.List [o, ^Writer w]
  (print-meta o w)
  (print-sequential "(" pr-on " " ")" o w))

(defmethod print-method :clojerl.erlang.List [o, ^Writer w]
  (print-meta o w)
  (print-sequential "(" pr-on " " ")" o w))

(defmethod print-dup :clojerl.List [o w] (print-method o w))
(defmethod print-dup :clojerl.erlang.List [o w] (print-method o w))

(def ^{:tag String
       :doc "Returns escape string for char or nil if none"
       :added "1.0"}
  char-escape-string
    {\newline "\\n"
     \tab  "\\t"
     \return "\\r"
     \" "\\\""
     \\  "\\\\"
     \formfeed "\\f"
     \backspace "\\b"})

(defmethod print-method :clojerl.String [^String s, ^Writer w]
  (if (or *print-dup* *print-readably*)
    (do (write w \")
      (dotimes [n (count s)]
        (let [c (clojerl.String/char_at.e s n)
              e (char-escape-string c)]
          (if e (write w e) (write w c))))
      (write w \"))
    (write w s))
  nil)

(defmethod print-dup :clojerl.String [s w] (print-method s w))

(defmethod print-method :clojerl.Vector [v, ^Writer w]
  (print-meta v w)
  (print-sequential "[" pr-on " " "]" v w))

(defn- print-map [m print-one w]
  (print-sequential
   "{"
   (fn [e  ^Writer w]
     (do (print-one (key e) w) (write w \space) (print-one (val e) w)))
   ", "
   "}"
   (seq m) w))

(defmethod print-method :clojerl.Map [m, ^Writer w]
  (print-meta m w)
  (print-map m pr-on w))

(defmethod print-method :clojerl.Set [s, ^Writer w]
  (print-meta s w)
  (print-sequential "#{" pr-on " " "}" (seq s) w))
