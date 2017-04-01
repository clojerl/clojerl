(ns clojure.test-clojure.binary
  (:use clojure.test))

(deftest t-bin-reader
  (is (= "a" #bin["a"]))
  (is (= "a" #bin[[97]]))
  (is (thrown? :error #bin[["a"]])))
