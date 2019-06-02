(ns benchmark.compare
  (:refer-clojure :exclude [compare])
  (:require [clojure.string :as str]
            [clojure.pprint :as pp]
            [benchmark.report :as b]))

(defn process-line
  [acc line]
  (let [[_ expr runs time] (re-find #"(.*), (\d+) runs, (\d+) ms" line)
        time (when time (erlang/binary_to_integer time))
        item {:expr expr :runs runs :time time}]
    (if expr
      (conj acc item)
      acc)))

(defn vm-clojure-info [content]
  (let [[_ vm] (re-find #";;; VM\s+=\s+(.+)\n" content)
        [_ clojure] (re-find #";;; Clojure\s+=\s+(.+)\n" content)]
    {:vm vm :clojure clojure}))

(defn make-report
  [content]
  (->> (str/split content #"\n")
       (reduce process-line [])))

(defn print-report
  [items]
  (->> items
       (sort-by :ratio-num)
       (pp/print-table [:clj :clje :runs :time-clj :time-clje :ratio])))

(defn compare
  [{expr-clj :expr t-clj :time runs :runs}
   {expr-clje :expr t-clje :time}]
  (let [ratio (if t-clje (-> (/ t-clje t-clj) float) "-")]
    {:clj       expr-clj
     :clje      expr-clje
     :runs      runs
     :time-clj  t-clj
     :time-clje t-clje
     :ratio-num ratio
     :ratio     (format "~.2f" ratio)}))

(defn -main [& [clj clje]]
  (let [items-clj  (-> (slurp clj) make-report)
        items-clje (-> (slurp clje) make-report)
        result (map compare items-clj items-clje)
        table  (with-out-str (print-report result))
        table  (str/replace table "-+-" "-|-")
        {jvm :vm clj-version :clojure} (vm-clojure-info (slurp clj))
        {beam :vm clje-version :clojure} (vm-clojure-info (slurp clje))]
    (println "## Clojure JVM")
    (println "- VM: " jvm)
    (println "- Clojure: " clj-version)
    (println)
    (println "## Clojure BEAM")
    (println "- VM: " beam)
    (println "- Clojure: " clje-version)
    (println)
    (println "## Comparison")
    (print table)))
