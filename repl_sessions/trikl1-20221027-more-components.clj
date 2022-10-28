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

(obj/defklass ConnectionState [])

(obj/defklass Surface []
  :- {:x int? :y int? :width int? :height int? :matrix [:fn util/atom?]}
  (put-char [{:keys [x y height width matrix]} x' y' charel]
    (if (and (< y' height)
             (< x' width))
      (swap! matrix assoc-in [(+ y y') (+ x x')] charel)
      (println "out of bounds"
               y' '< height '/
               x' '< width)))

  (subsurface [{:keys [x y height width matrix]} x' y' w h]
    (obj/instance
     Surface
     {:matrix matrix
      :x (+ x x')
      :y (+ y y')
      :width (min w (- width x'))
      :height (min h (- height y'))})))

(obj/defklass BaseComponent []
  (preferred-size [_] [0 0])
  (minimum-size [$] (obj/call $ 'preferred-size))
  (maximum-size [$] (obj/call $ 'preferred-size))
  (draw [{:keys [surface]}]))

(obj/defklass StyledText [BaseComponent]
  :- {:text string?}
  (preferred-size [{:keys [^String text]}]
    [(.length text) 1])
  (draw [{:keys [surface ^String text fg bg x y]}]
    (dotimes [i (.length text)]
      (obj/call
       'put-char
       surface (+ x i) y (screen/->Charel (.charAt text i) fg bg)))))

(obj/defklass ConnectionState []
  (prep [{:keys [listeners make-state]
          :or   {listeners {}}
          :as   opts}]
    {:screen    nil
     :size      nil
     :listeners listeners})

  (init [$ _]
    (let [event-loop (obj/instance event-loop/EventLoop
                                   {:on-event #(obj/call $ 'on-event %2)})]
      (obj/call event-loop 'start!)
      (add-watch $ ::resize-screen
                 (fn [_ _ _ _] (obj/call $ 'adjust-screen-size!)))
      (swap! $ assoc ::event-loop event-loop)))

  (on-event [$ e]
    (println "got event" $ e))

  (mount [_ root]
    )

  (adjust-screen-size! [{[w h :as size] :size, screen :screen :as $}]
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

  (def conn-loop
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

  #_(obj/call
     (:event-loop (conn))
     'enqueue
     {:type ::test})
  #_(keys  @(conn-state))
  (comment
    conn-loop
    (conn))

  #_(conn/shutdown (conn)))

(deftype XXX []
  )
