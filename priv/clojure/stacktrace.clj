;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;; stacktrace.clj: print Clojure-centric stack traces

;; by Stuart Sierra
;; January 6, 2009

(ns ^{:doc "Print stack traces oriented towards Clojure, not Java."
       :author "Stuart Sierra"}
  clojure.stacktrace)

(defn root-cause
  "Returns the last 'cause' Throwable in a chain of Throwables."
  {:added "1.1"}
  [tr]
  (if-let [cause (first tr)]
    (recur cause)
    tr))

(defn get-stacktrace
  []
  (let [st (erlang/get_stacktrace.e)]
    (if-not (empty? st)
      st
      (try
        (throw :fake)
        (catch :throw e
          (erlang/get_stacktrace.e))))))

(defn filename
  [e]
  (let [info (nth e 3)]
    (when (list? info)
      (when-let [filename (proplists/get_value.e :file info)]
        (erlang/list_to_binary.1 filename)))))

(defn line-num
  [e]
  (let [info (nth e 3)]
    (if (list? info)
      (proplists/get_value.e :line info "?")
      info)))

(defn module [e]
  (first e))

(defn function [e]
  (second e))

(defn print-trace-element
  "Prints a Clojure-oriented view of one element in a stack trace."
  {:added "1.1"}
  [e]
  (let [class  (module e)
	method (function e)
        info   (nth e 4)
        file   (filename e)
        line   (line-num e)]
    (let [match (re-matches #"^([A-Za-z0-9_.-]+)\$(\w+)__\d+$" (str class))]
      (if (and match (= "invoke" method))
	(apply printf "%s/%s" (rest match))
	(printf "%s.%s" class method)))
    (printf " (%s:%d)" (or file "") line)))

(defn print-throwable
  "Prints the class and message of a Throwable."
  {:added "1.1"}
  [tr]
  (throw "unimplemented throwable")
  #_(printf "%s: %s" (.getName (class tr)) (.getMessage tr)))

(defn print-stack-trace
  "Prints a Clojure-oriented stack trace.
  Prints a maximum of n stack frames (default: unlimited).
  Does not print chained exceptions (causes)."
  {:added "1.1"}
  ([st] (print-stack-trace st nil))
  ([st n]
   #_(print-throwable tr)
   (newline)
   (print " at ")
   (if-let [e (first st)]
     (print-trace-element e)
     (print "[empty stack trace]"))
   (newline)
   (doseq [e (if (nil? n)
               (rest st)
               (take (dec n) (rest st)))]
     (print "    ")
     (print-trace-element e)
     (newline))))

(defn print-cause-trace
  "Like print-stack-trace but prints chained exceptions (causes)."
  {:added "1.1"}
  ([st] (print-cause-trace st nil))
  ([st n]
   (throw "unsupported")))

(defn e
  "REPL utility.  Prints a brief stack trace for the root cause of the
  most recent exception."
  {:added "1.1"}
  []
  (print-stack-trace (root-cause *e) 8))
