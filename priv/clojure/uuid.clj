;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.uuid)

(defn- default-uuid-reader [form]
  {:pre [(string? form)]}
  (erlang.util.UUID/new.e form))

(defmethod print-method :erlang.util.UUID [uuid ^java.io.Writer w]
  (erlang.io.IWriter/write.e w (str "#uuid \"" (str uuid) "\"")))

(defmethod print-dup :erlang.util.UUID [o w]
  (print-method o w))
