;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^{:doc "Clojure String utilities

It is poor form to (:use clojure.string). Instead, use require
with :as to specify a prefix, e.g.

(ns your.namespace.here
  (:require [clojure.string :as str]))

Design notes for clojure.string:

1. Strings are objects (as opposed to sequences). As such, the
   string being manipulated is the first argument to a function;
   passing nil will result in a NullPointerException unless
   documented otherwise. If you want sequence-y behavior instead,
   use a sequence.

2. Functions are generally not lazy, and call straight to host
   methods where those are available and efficient.

3. Functions take advantage of String implementation details to
   write high-performing loop/recurs instead of using higher-order
   functions. (This is not idiomatic in general-purpose application
   code.)

4. When a function is documented to accept a string argument, it
   will take any implementation of the correct *interface* on the
   host platform. In Java, this is CharSequence, which is more
   general than String. In ordinary usage you will almost always
   pass concrete strings. If you are doing something unusual,
   e.g. passing a mutable implementation of CharSequence, then
   thread-safety is your responsibility."
      :author "Stuart Sierra, Stuart Halloway, David Liebke"}
  clojure.string
  #_(:refer-clojure :exclude (replace reverse)))

(defn ^String reverse
  "Returns s with its characters reversed."
  {:added "1.2"}
  [^CharSequence s]
  (-> (when s (str s))
      unicode/characters_to_list.1
      lists/reverse.1
      erlang/list_to_binary.1))

(defn ^String re-quote-replacement
  "Given a replacement string that you wish to be a literal
   replacement for a pattern match in replace or replace-first, do the
   necessary escaping of special characters in the replacement."
  {:added "1.5"}
  [^CharSequence replacement]
  (erlang.util.Regex/quote.e (when replacement (str replacement))))

(defn- replace-by
  [s re f]
  (with-open [buffer (new erlang.io.StringWriter)]
    (let [append erlang.io.StringWriter/write.2
          length (count s)
          matches (re-run re s :global #[:capture :all :index])]
      (loop [index 0
             [[[i len] :as m] & ms] matches]
        (if m
          (let [m (map (fn [[i len]] (subs s i (+ i len))) m)
                m (if (= (count m) 1) (first m) m)]
            (append buffer (subs s index i))
            (append buffer (f m))
            (recur (+ i len) ms))
          (do
            (append buffer (subs s index length))
            (str buffer)))))))

(defn ^String replace
  "Replaces all instance of match with replacement in s.

   match/replacement can be:

   string / string
   char / char
   pattern / (string or function of match).

   See also replace-first.

   The replacement is literal (i.e. none of its characters are treated
   specially) for all cases above except pattern / string.

   For pattern / string, $1, $2, etc. in the replacement string are
   substituted with the string that matched the corresponding
   parenthesized group in the pattern.  If you wish your replacement
   string r to be used literally, use (re-quote-replacement r) as the
   replacement argument.  See also documentation for
   java.util.regex.Matcher's appendReplacement method.

   Example:
   (clojure.string/replace \"Almost Pig Latin\" #\"\\b(\\w)(\\w+)\\b\" \"$2$1ay\")
   -> \"lmostAay igPay atinLay\""
  {:added "1.2"}
  [s match replacement]
  (let [s (when s (str s))
        replace erlang.util.Regex/replace.4
        opts (clj_core/seq_to_list.e [:global])]
    (cond
      (string? match) (replace match s (str replacement) opts)
      (regex? match) (if (or (string? replacement)
                             (instance? :erlang.io.StringWriter replacement))
                       (replace match s (str replacement) opts)
                       (replace-by s match replacement))
      :else (throw (str "Invalid match arg: " match)))))

(defn- replace-first-by
  [s re f]
  (with-open [buffer (new erlang.io.StringWriter)]
    (let [append erlang.io.StringWriter/write.2
          length (count s)
          [[i len] :as matches] (re-run re s #[:capture :all :index])
          matches (map (fn [[i len]] (subs s i (+ i len))) matches)
          matches (if (= (count matches) 1) (first matches) matches)]
      (if i
        (do
          (append buffer (subs s 0 i))
          (append buffer (f matches))
          (append buffer (subs s (+ i len) length))
          (str buffer))
        s))))

(defn- replace-first-str
  [s match replace]
  (let [s (str s)
        i (clojerl.String/index_of.e s match)]
    (if (= -1 i)
      s
      (str (subs s 0 i) replace (subs s (+ i (count match)))))))

(defn ^String replace-first
  "Replaces the first instance of match with replacement in s.

   match/replacement can be:

   char / char
   string / string
   pattern / (string or function of match).

   See also replace.

   The replacement is literal (i.e. none of its characters are treated
   specially) for all cases above except pattern / string.

   For pattern / string, $1, $2, etc. in the replacement string are
   substituted with the string that matched the corresponding
   parenthesized group in the pattern.  If you wish your replacement
   string r to be used literally, use (re-quote-replacement r) as the
   replacement argument.  See also documentation for
   java.util.regex.Matcher's appendReplacement method.

   Example:
   (clojure.string/replace-first \"swap first two words\"
                                 #\"(\\w+)(\\s+)(\\w+)\" \"$3$2$1\")
   -> \"first swap two words\""
  {:added "1.2"}
  [s match replacement]
  (let [s (when s (str s))
        replace erlang.util.Regex/replace.4
        opts (clj_core/seq_to_list.e [])]
    (cond
      (or (string? match)
          (instance? :erlang.io.StringWriter match))
      (replace-first-str s (str match) (str replacement))
      (regex? match)
      (if (or (string? replacement)
              (instance? :erlang.io.StringWriter replacement))
        (replace match s (str replacement) opts)
        (replace-first-by s match replacement))
      :else (throw (str "Invalid match arg: " match)))))

(defn ^String join
  "Returns a string of all elements in coll, as returned by (seq coll),
   separated by an optional separator."
  {:added "1.2"}
  ([coll]
   (apply str coll))
  ([separator coll]
   (clojerl.String/join.e (clj_core/seq_to_list.e coll) separator)))

(defn ^String upper-case
  "Converts string to all upper-case."
  {:added "1.2"}
  [^CharSequence s]
  (clojerl.String/to_upper.e (when s (str s))))

(defn ^String lower-case
  "Converts string to all lower-case."
  {:added "1.2"}
  [^CharSequence s]
  (clojerl.String/to_lower.e (when s (str s))))

(defn ^String capitalize
  "Converts first character of the string to upper-case, all other
  characters to lower-case."
  {:added "1.2"}
  [^CharSequence s]
  (let [s (when s (str s))]
    (if (< (count s) 2)
      (upper-case s)
      (str (upper-case (subs s 0 1))
           (lower-case (subs s 1))))))

(defn split
  "Splits string on a regular expression.  Optional argument limit is
  the maximum number of splits. Not lazy. Returns vector of the splits."
  {:added "1.2"}
  ([s re]
   (vec (erlang.util.Regex/split.e re
                                  (when s (str s))
                                  (clj_core/seq_to_list.e []))))
  ([s re limit]
   (vec (erlang.util.Regex/split.e re
                                   (when s (str s))
                                   (clj_core/seq_to_list.e [:global #[:match_limit limit]])))))

(defn split-lines
  "Splits s on \\n or \\r\\n."
  {:added "1.2"}
  [s]
  (vec (split s #"\r?\n")))

(defn ^String trim
  "Removes whitespace from both ends of string."
  {:added "1.2"}
  [s]
  (let [s (when s (str s))
        len (when s (count s))]
    (loop [rindex len]
      (if (zero? rindex)
        ""
        (if (clojerl.String/is_whitespace.e (clojerl.String/char_at.e s (dec rindex)))
          (recur (dec rindex))
          ;; there is at least one non-whitespace char in the string,
          ;; so no need to check for lindex reaching len.
          (loop [lindex 0]
            (if (clojerl.String/is_whitespace.e (clojerl.String/char_at.e s lindex))
              (recur (inc lindex))
              (subs s lindex rindex))))))))

(defn ^String triml
  "Removes whitespace from the left side of string."
  {:added "1.2"}
  [^CharSequence s]
  (let [s (when s (str s))
        len (when s (count s))]
    (loop [index 0]
      (if (= len index)
        ""
        (if (clojerl.String/is_whitespace.e (clojerl.String/char_at.e s index))
          (recur (inc index))
          (subs s index len))))))

(defn ^String trimr
  "Removes whitespace from the right side of string."
  {:added "1.2"}
  [^CharSequence s]
  (let [s (when s (str s))]
    (loop [index (when s (count s))]
      (if (zero? index)
        ""
        (if (clojerl.String/is_whitespace.e (clojerl.String/char_at.e s (dec index)))
          (recur (dec index))
          (subs s 0 index))))))

(defn ^String trim-newline
  "Removes all trailing newline \\n or return \\r characters from
  string.  Similar to Perl's chomp."
  {:added "1.2"}
  [^CharSequence s]
  (let [s (when s (str s))]
    (loop [index (when s (count s))]
      (if (zero? index)
        ""
        (let [ch (clojerl.String/char_at.e s (dec index))]
          (if (or (= ch \newline) (= ch \return))
            (recur (dec index))
            (subs (str s) 0 index)))))))

(defn blank?
  "True if s is nil, empty, or contains only whitespace."
  {:added "1.2"}
  [s]
  (let [s (when s (str s))
        len (count s)]
    (if s
      (loop [index (int 0)]
        (if (= len index)
          true
          (if (= :whitespace (clj_utils/char_type.e (binary/at.e s index)))
            (recur (inc index))
            false)))
      true)))

(defn starts-with?
  "True if s starts with substr."
  {:added "1.8"}
  [s substr]
  (clojerl.String/starts_with.e (when s (str s)) substr))

(defn ^String escape
  "Return a new string, using cmap to escape each character ch
   from s as follows:

   If (cmap ch) is nil, append ch to the new string.
   If (cmap ch) is non-nil, append (str (cmap ch)) instead."
  {:added "1.2"}
  [s cmap]
  (let [s (when s (str s))
        length (count s)
        append erlang.io.StringWriter/write.2]
    (with-open [buffer (new erlang.io.StringWriter)]
      (loop [index 0
             buffer buffer]
        (if (= length index)
          (str buffer)
          (let [ch (clojerl.String/char_at.e s index)]
            (if-let [replacement (cmap ch)]
              (append buffer replacement)
              (append buffer ch))
            (recur (inc index) buffer)))))))

(defn index-of
  "Return index of value (string or char) in s, optionally searching
  forward from from-index or nil if not found."
  {:added "1.8"}
  ([s value]
   (let [result (clojerl.String/index_of.e (str s) value)]
     (if (= result -1)
       nil
       result)))
  ([s value from-index]
   (let [result (clojerl.String/index_of.e (str s) value from-index)]
     (if (= result -1)
       nil
       result))))

(defn last-index-of
  "Return last index of value (string or char) in s, optionally
  searching backward from from-index or nil if not found."
  {:added "1.8"}
  ([^CharSequence s value]
   (let [result (clojerl.String/last_index_of.e (str s) value)]
     (if (= result -1)
       nil
       result)))
  ([^CharSequence s value ^long from-index]
   (let [result (clojerl.String/last_index_of.e (str s) value from-index)]
     (if (= result -1)
       nil
       result))))

(defn ends-with?
  "True if s ends with substr."
  {:added "1.8"}
  [^CharSequence s ^String substr]
  (clojerl.String/ends_with.e (str s) substr))

(defn includes?
  "True if s includes substr."
  {:added "1.8"}
  [^CharSequence s ^CharSequence substr]
  (clojerl.String/contains.e (str s) substr))
