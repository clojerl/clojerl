(ns examples.run-tests
  (:require [clojure.string :as str]
            clojure.test))

(defn- path->symbol [root path]
  (let [path (if root
               (subs path (count root))
               path)
        ns-name (-> path
                    filename/rootname.e
                    (str/replace #"/" ".")
                    (str/replace #"_" "-"))
        ns-symbol (symbol ns-name)]
    ns-symbol))

(def ignore-nss #{'clojure.test-clojure.test})

(defn -main [& [test-dir root]]
  (when test-dir
    (let [paths (->> (file-seq test-dir)
                     (filter (complement filelib/is_dir.1)))
          ns-symbols (->> paths
                          (map (partial path->symbol root))
                          (filter (comp not ignore-nss))
                          doall)
          _ (doseq [ns-symbol ns-symbols] (require [ns-symbol :verbose true]))
          result (apply clojure.test/run-tests ns-symbols)]
      (when (or (pos? (:error result))
                (pos? (:fail result)))
        (throw result)))))
