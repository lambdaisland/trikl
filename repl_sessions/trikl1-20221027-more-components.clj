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

  (subsurface [{:keys [x y height width matrix] :as self} x' y' w h]
    (obj/with self {:x (+ x x')
                    :y (+ y y')
                    :width (min w (- width x'))
                    :height (min h (- height y'))})))

(obj/defklass BaseComponent []
  (preferred-size [_]
    [0 0])
  (minimum-size [self]
    (self/preferred-size))
  (maximum-size [self]
    (self/preferred-size))
  (draw [{:keys [surface]}]
    ))

(obj/defklass StyledText [BaseComponent]
  :- {:text string?}
  (preferred-size [{:keys [^String text]}]
    [(.length text) 1])
  (draw [{:keys [surface ^String text fg bg x y]}]
    (dotimes [i (.length text)]
      (surface 'put-char
               (+ x i) y
               (screen/->Charel (.charAt text i) fg bg)))))

(obj/defklass ConnectionState []
  (prep [{:keys [listeners make-state]
          :or   {listeners {}}
          :as   opts}]
    {:screen    nil
     :size      nil
     :listeners listeners})

  (write [self s])

  (init [self _]
    (let [event-loop (obj/specify (obj/create event-loop/EventLoop)
                       (on-event [_ e]
                         (self 'on-event e)))]
      (swap! self (fn [self]
                    (-> self
                        (assoc ::event-loop event-loop)
                        (assoc-in [:listeners ::event-loop]
                                  (fn [e]
                                    (event-loop 'enqueue e))))))
      (add-watch self ::resize-screen
                 (fn [_ _ _ _]
                   (self/adjust-screen-size!)))
      (event-loop 'start!)))

  (on-event [self e]
    (println "got event" e))

  (new-viewport! [{:keys [matrix]} [width height]]
    (swap!
     self assoc :viewport
     (obj/create Surface {:x 0 :y 0 :width width :height height
                          :matrix (atom matrix)})))

  (grow-viewport [{:keys [screen] :as self} lines]
    (let [{:keys [size term-state]} screen
          [cols rows] size]
      (self 'write (term/move-relative (:cursor term-state) [0 rows]))
      (self 'write (apply str (repeat lines "\n")))
      (swap! self
             (fn [selv]
               (-> selv
                   (update :screen
                           #(-> %
                                (assoc-in [:term-state :cursor 0] 0)
                                (update-in [:term-state :cursor 1] + lines)
                                (screen/resize [cols (+ rows lines)])))
                   (assoc :viewport (self 'new-viewport)))))))

  (mount [self root]
    (swap! self assoc :root root)
    (self 'draw-root))

  (draw-root [{:keys [root screen viewport] :as self}]
    (let [{:keys [size !matrix]} screen
          [width height] size
          [_ lines] (root 'preferred-size)]
      (if (< height lines)
        (do
          (self 'grow-viewport (- lines height))
          (reset! !matrix (screen/charel-matrix width lines))
          (swap! self assoc :viewport (self 'new-viewport))
          (root 'draw viewport)
          ))))

  (adjust-screen-size! [{[w h :as size] :size, screen :screen :as self}]
    (when size
      (cond
        (not screen)
        (swap! self (fn [selv]assoc :screen) (screen/new-screen w 1))

        (not= w (first (:size screen)))
        (swap! self update :screen screen/resize [w (second (:size screen))])
        ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defonce connections (atom []))

(defn conn []
  (last @connections))

(defn conn-state []
  (:state (conn)))

(defn telnet-opts [sock]
  {:client-socket sock
   :init-sequence (str "\r" term/HIDE-CURSOR)
   :reset-sequence (str term/SHOW-CURSOR)
   :make-state #(obj/create ConnectionState %)
   :listeners {::debug prn
               ::size conn/resize-listener}})

(defonce running? (volatile! true))

(comment
  (do
    (defonce ss (telnet/server-socket 9999))

    (defonce conn-loop
      (future
        (try
          (while @running?
            (let [cs (telnet/accept-connection ss)
                  conn (telnet/telnet-connection (telnet-opts cs))]
              (obj/specify! (:state conn)
                (write [_ s]
                  (conn/write conn s)))
              (println "Connected!")
              (swap! connections conj conn)))
          (catch Exception e
            (println "conn-loop exception")
            (throw e)))
        (println "conn-loop broke"))))

  conn-loop
  (conn)

  (
   (conn-state)
   'write "hello")

  #_(conn/shutdown (conn)))
