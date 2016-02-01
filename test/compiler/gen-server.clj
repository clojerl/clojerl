(ns example.gen-server
    (:behavior :gen-server))

(defn init [args]
  #[:ok {}])

(defn handle-call [req from state]
  #[:noreply state])

(defn handle-cast
  ([#[:store key value] state]
   #[:noreply (update-in state [:store] assoc key value)])
  ([#[:get key] state]
   #[:reply, (get-in state [:store key]), state]))

(defn handle-info [info state]
  #[:noreply state])

(defn terminate [reason, state]
  :ok)

(defn code-change [old-vsn, state, extra]
  #[:ok state])

(defn format-status [opt [pdict state]]
  :ok)
