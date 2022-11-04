(ns lambdaisland.trikl2.window
  (:require
   [clojure.set :as set]
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.display :as display]
   [lambdaisland.trikl1.simple-object-system :as obj]
   [lambdaisland.trikl1.util :as util]
   [lambdaisland.trikl2.event-loop :as event-loop]
   [lambdaisland.trikl2.surface :as surface]))

(obj/defklass Window []
  :- {:input-loop some?
      :event-loop some?
      :size [:tuple int? int?]
      :cursor [:tuple int? int?]
      :matrix [:vector [:vector some?]]
      :conn conn/schema}

  (prep [{:keys [conn] :as opts}]
    (let [event-loop (obj/create event-loop/EventLoop)
          conn       ((first conn) (assoc (second conn)
                                          :dispatch
                                          #(event-loop 'enqueue %2)))
          display    (display/new-display (assoc opts :conn conn))]
      (assoc display
             :event-loop event-loop
             :input-loop (util/thread "input-loop"
                           (try
                             (conn/input-loop conn)
                             (finally
                               (println "Input loop broke!")))))))

  (init [{:keys [conn event-loop input-loop] :as self} _]
    (obj/specify! event-loop
      (on-event [_ e]
        (self 'on-event e)))
    (.start input-loop)
    (event-loop 'start!))

  (close! [{:keys [event-loop conn input-loop]}]
    (event-loop 'stop!)
    (.interrupt input-loop)
    (conn/shutdown conn))

  (mount [{:keys [event-loop] :as self} root]
    (swap! self assoc :root root)
    (swap! root assoc :dirty? true)
    (event-loop 'enqueue {:type :redraw}))

  (flush! [{:keys [matrix canvas conn] :as self}]
    (let [sb (StringBuilder.)
          new-state (display/diff sb @self matrix canvas)]
      (conn/write conn (str sb))
      (swap! self
             (fn [win]
               (-> win
                   (assoc :matrix canvas :canvas nil)
                   (merge new-state))))))

  (on-event [self e]
    (println (str "window>>" e))
    (let [handler (symbol (str "on-" (name (:type e))))]
      (when (obj/has-method? self handler)
        (self handler e))))

  (on-screen-size [self {:keys [screen-size]}]
    (swap! self display/resize screen-size))

  (on-redraw [{:keys [size root event-loop] :as self} e]
    (swap! self assoc :canvas (apply display/charel-matrix size))
    (if (:dirty? root)
      (root 'draw
            (obj/create surface/Surface {:x 0 :y 0
                                         :width (first size)
                                         :height (first size)
                                         :window self}))
      (root 'visit
            (fn [c]
              (when (:dirty? c)
                (c 'draw (obj/create surface/Surface {:x (:x c) :y (:y c)
                                                      :width (:width c)
                                                      :height (:height c)
                                                      :window self}))
                :prune-children))))
    (root 'visit
          (fn [c]
            (doseq [a (set/difference (:trace c) (:last-trace c))]
              (add-watch a [::redraw c]
                         (fn [_ _ _ _]
                           (swap! c assoc :dirty? true)
                           (event-loop 'enqueue {:type :redraw}))))
            (doseq [a (set/difference (:last-trace c) (:trace c))]
              (remove-watch a [::redraw c]))
            (swap! c assoc :last-trace (:trace c))))
    (self 'flush!)))

(obj/defklass InlineWindow [Window]
  :- {:lines int?}
  (prep [{:keys [lines] :as opts}]
    (assoc ((get Window 'prep) opts) :lines lines))

  (on-screen-size [self {:keys [screen-size]}]
    (let [[width _] screen-size]
      (swap! self display/resize [width (:lines self)]))))
