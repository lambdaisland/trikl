(ns lambdaisland.trikl2.ui
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.log :as log]
   [lambdaisland.trikl1.ratom :as ratom]
   [lambdaisland.trikl1.simple-object-system :as obj]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl2.component :as c]
   [lambdaisland.trikl2.window :as window])
  (:import
   (java.io Closeable)))

(defn cli-ui [render-fn {:keys [mode] :or {mode :fullscreen}}]
  (log/info :cli-ui/starting {:mode mode})
  (let [win (obj/create
             (case mode
               :inline     window/InlineWindow
               :fullscreen window/Window)
             {:conn
              [conn/stdio-connection
               (case mode
                 :inline
                 {:init-sequence  conn/default-inline-init-sequence
                  :reset-sequence conn/default-inline-reset-sequence}
                 :fullscreen {}) ]
              :lines     1
              :auto-grow true})]
    (win 'mount (c/hiccup->component [render-fn]))))

(defn telnet-ui [render-fn {:keys [port mode]
                            :or   {port 9999
                                   mode :fullscreen}}]
  (log/info :telnet-ui/starting {:mode mode
                                 :port 9999})
  (let [ss    (telnet/server-socket port)
        stop? (volatile! false)
        wins  (atom [])]
    {:stop! (fn []
              (run! (comp conn/shutdown :conn) @wins)
              (.close ^Closeable ss)
              (vreset! stop? true))
     :wins  wins
     :conn-loop
     (future
       (while (not @stop?)
         (let [cs   (telnet/accept-connection ss)
               conn [telnet/telnet-connection (case mode
                                                :inline
                                                {:client-socket  cs
                                                 :init-sequence  conn/default-inline-init-sequence
                                                 :reset-sequence conn/default-inline-reset-sequence}
                                                :fullscreen
                                                {:client-socket cs})]]
           (let [win (obj/create (case mode
                                   :inline     window/InlineWindow
                                   :fullscreen window/Window)
                                 {:conn      conn
                                  :lines     1
                                  :auto-grow true})]
             (swap! wins conj win)
             (win 'mount (c/hiccup->component [render-fn]))))))}))

(comment
  (defn -main [& _]
    (let [state (ratom/ratom {:count 0})]
      (cli-ui (fn []
                [c/Text {:fg [0 0 200]
                         :bg [255 255 255]} "hello" "=" (:count @state)
                 "\n" "hello"])
              {:mode :inline})
      #_(while true
          (Thread/sleep 1000)
          (swap! state update :count inc))
      (Thread/sleep 1000)
      (swap! state update :count inc))
    @(promise)))