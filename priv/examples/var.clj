(ns examples.var)

(def x 1)

(def prn
  (fn* [x]
    (io/format.e "~s~n" (clj_core/seq.e [(clj_core/str.e x)]))))

(prn (var x))

(prn #'x)

(prn x)

(def ^:dynamic *y* :y)

;; Return root binding
(prn *y*)

;; Return dynamic binding
(clojerl.Var/push_bindings.e {#'*y* :bound-y})
(prn *y*)
(clojerl.Var/pop_bindings.e)

;; Define unbound
(def z)

(prn z)
