(ns repl-sessions.trikl1-basic-screen-demo
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.screen :as screen]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl1.util :as util]))

(defonce ss (telnet/server-socket 9999))
(defonce connections (atom []))

(defn conn []
  (last @connections))

(defn init-conn [conn]
  conn)

(defn telnet-opts [sock]
  {:client-socket sock
   :init-sequence conn/default-init-sequence
   :reset-sequence conn/default-reset-sequence
   :listeners {::debug prn
               ::size conn/resize-listener}})

(defonce conn-loop
  (future
    (while true
      (let [cs (telnet/accept-connection ss)]
        (println "Connected!")
        (swap! connections
               conj
               (init-conn (telnet/telnet-connection (telnet-opts cs))))))
    (println "conn-loop broke")))

(swap!
 (:state (conn))
 (fn [state]
   (assoc state :screen (screen/new-screen (:size state)))))

(let [screen (:screen @(:state (conn)))]
  (reset!
   (:!matrix screen)
   (screen/update-matrix
    (screen/charel-matrix screen)
    (fn [col row charel]
      (assoc charel
             :char (if (even? row) \# \=)
             :fg [col row (+ row col)]))))
  nil)

(do
  (screen/flush! (conn))
  nil)

;; ▓▒░➢
;; 🮐🮒ࣹ
;; ↜↝
;; 🮐🮒ࣹ
