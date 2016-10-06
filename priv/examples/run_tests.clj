(ns examples.run-tests
  (:require [clojure.string :as str]
            clojure.test))

(defn require-ns [root path]
  (let [path (if root
               (subs path (count root))
               path)
        ns-name (-> path
                    filename/rootname.e
                    (str/replace #"/" ".")
                    (str/replace #"_" "-"))
        ns-symbol (symbol ns-name)]
    (require ns-symbol)
    ns-symbol))

(defn -main [& [test-dir root]]
  (when test-dir
    (let [paths (->> (file-seq test-dir)
                     (filter (complement filelib/is_dir.1)))
          namespaces (mapv (partial require-ns root) paths)]
      (apply clojure.test/run-tests namespaces))))
