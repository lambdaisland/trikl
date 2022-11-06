(ns repl-sessions.trikl1-20221031-does-halloween-bring-salvation
  (:require
   [lambdaisland.trikl1.ratom :as ratom]
   [lambdaisland.trikl1.simple-object-system :as obj]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl2.component :as c]
   [lambdaisland.trikl2.window :as window]))

(defonce windows (atom []))

(defn win []
  (last @windows))

(defn connect-opts [sock]
  {:client-socket sock
   :init-sequence (str "\r" term/HIDE-CURSOR)
   :reset-sequence (str term/SHOW-CURSOR)})

(defonce running? (volatile! true))
#_(vswap! running? not)

(comment
  (do
    (defonce ss (telnet/server-socket 9999))

    (def conn-loop
      (future
        (try
          (while @running?
            (let [cs (telnet/accept-connection ss)
                  conn [telnet/telnet-connection (connect-opts cs)]]
              (swap! windows conj (obj/create window/InlineWindow {:conn conn
                                                                   :lines 1}))))
          (catch Exception e
            (println "conn-loop exception")
            (throw e)))
        (println "conn-loop broke"))))

  conn-loop
  (win)

  ((win) 'mount
   (obj/create c/StatefulComponent nil)
   #_
   (obj/create c/TextLine {:text "hellox"
                           :fg [0 200 0]
                           :bg nil}))

  (swap! c/state update :count inc)


  #_(conn/shutdown (conn))

  (def state (ratom/ratom {:count 0}))

  (swap! state update :count inc)

  (defn my-compo [caption]
    [c/Text {:fg [0 0 200]
             :bg [255 255 255]} caption "=" (:count @state)
     "\n" "hello"])

  (:root (win))

  ((win) 'mount
   (c/hiccup->component [my-compo "GO!"]) ))
