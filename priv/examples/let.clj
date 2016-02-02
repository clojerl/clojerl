(ns examples.let)

;; Bind an anonymous function and call it

(let* [f (fn* [] :bound-fn*)]
  (clojure.core/prn (f)))

;; Nested lets with shadowed binding names

(let* [x :outer]
      (clojure.core/prn x) ;; should print :outer
      (let* [x :inner]
            (clojure.core/prn x)) ;; should print :inner
      (clojure.core/prn x) ;; should print :outer
)

;; Let with bindings with the same name

(let* [x :first
       x [x :second]]
      (clojure.core/prn x) ;; should print [:first :second]
)
