(ns repl-sessions.trikl1-20221031-does-halloween-bring-salvation
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.display :as display]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl1.util :as util]
   [lambdaisland.trikl1.simple-object-system :as obj]
   [lambdaisland.trikl2.event-loop :as event-loop]
   [lambdaisland.trikl2.window :as window]
   [lambdaisland.trikl2.component :as c]
   [malli.provider :as mp]))

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
   (obj/create c/TextLine {:text "hello"
                           :fg [100 0 0]
                           :bg nil}))

  (let [e (java.lang.Exception.)]
    (.setStackTrace e
                    (into-array StackTraceElement
                                (concat [(StackTraceElement. "foo" "bar" "baz" 123)]
                                        (.getStackTrace e)             )))
    (throw e))
  (.getStackTrace (java.lang.Throwable.))
  #_(conn/shutdown (conn)))
