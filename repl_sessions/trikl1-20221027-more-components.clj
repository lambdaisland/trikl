(ns trikl1-20221021-more-components
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.screen :as screen]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl1.util :as util]
   [lambdaisland.trikl1.simple-object-system :as obj]
   [lambdaisland.trikl2.event-loop :as event-loop])
  (:import (java.util.concurrent Delayed
                                 DelayQueue
                                 PriorityBlockingQueue
                                 TimeUnit
                                 BlockingQueue
                                 LinkedBlockingQueue)))

(set! *warn-on-reflection* true)
(set! *math-context* :warn-on-boxed)

(obj/defklass Surface []
  :- {:x int? :y int? :width int? :height int?}

  )

(obj/defklass ConnectionState []
  (prep [{:keys [listeners make-state]
          :or   {listeners {}}
          :as   opts}]
    {:screen    nil
     :size      nil
     :listeners listeners})

  (init [_ _]
    (let [event-loop (obj/instance event-loop/EventLoop
                                   {:on-event #(obj/call this 'on-event %2)})]
      (obj/call event-loop 'start!)
      (add-watch this ::resize-screen
                 (fn [_ _ _ _] (obj/call this 'adjust-screen-size!)))
      (swap! this assoc ::event-loop event-loop)))

  (on-event [_ e]
    (println "got event" this e))

  (mount [_ component]
    )

  (adjust-screen-size! [{[w h] :size, screen :screen :as $}]
    (when size
      (cond
        (not screen)
        (swap! $ assoc :screen (screen/new-screen w 1))

        (not= w (first (:size screen)))
        (swap! $ update :screen screen/resize [w (second (:size screen))])
        ))))

(do
  (defonce ss (telnet/server-socket 9999))
  (defonce connections (atom []))

  (defn conn []
    (last @connections))

  (defn conn-state []
    (:state (conn)))

  (defn telnet-opts [sock]
    {:client-socket sock
     :init-sequence (str "\r" term/HIDE-CURSOR)
     :reset-sequence (str term/SHOW-CURSOR)
     :make-state #(obj/instance ConnectionState %)
     :listeners {::debug prn
                 ::size conn/resize-listener}})

  (defonce conn-loop
    (future
      (try
        (while true
          (let [cs (telnet/accept-connection ss)]
            (println "Connected!")
            (swap! connections
                   conj
                   (telnet/telnet-connection (telnet-opts cs)))))
        (catch Exception e
          (println "conn-loop exception")
          (throw e)))
      (println "conn-loop broke")))

  (obj/call
   (:event-loop)
   'enqueue
   {:type ::test})
  (keys  @(conn-state))
  (comment
    conn-loop
    (conn))

  #_(conn/shutdown (conn)))

(deftype XXX []
  )

(let [{:keys [foo]} (XXX.)]
  foo)
