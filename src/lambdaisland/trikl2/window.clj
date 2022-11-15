(ns lambdaisland.trikl2.window
  (:require
   [clojure.set :as set]
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.display :as display]
   [lambdaisland.trikl1.log :as log]
   [lambdaisland.trikl1.simple-object-system :as obj]
   [lambdaisland.trikl1.term :as term]
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
    (.start ^Thread input-loop)
    (event-loop 'start!))

  (close! [{:keys [event-loop conn input-loop]}]
    (event-loop 'stop!)
    (.interrupt ^Thread input-loop)
    (conn/shutdown conn))

  (mount [{:keys [event-loop size] :as self} root]
    (swap! self assoc :root root)
    (swap! root assoc :dirty? true)
    (event-loop 'enqueue {:type :redraw :origin :mount}))

  (flush! [{:keys [matrix canvas conn fg bg cursor size] :as self}]
    (let [sb (StringBuilder.)
          new-state (display/diff sb {:fg fg :bg bg :cursor cursor :size size} matrix canvas)]
      (log/trace :window/flush {:str (str sb) :new-state (select-keys new-state [:fg :bg :size :cursor])})
      (conn/write conn (str sb))
      (swap! self
             (fn [win]
               (-> win
                   (assoc
                     :matrix (:canvas win)
                     :canvas nil
                     :fg (:fg new-state)
                     :bg (:bg new-state)
                     :cursor (:cursor new-state)))))))

  (on-event [self e]
    (log/trace :window/on-event {:event e :window {:cursor (:cursor self) :size (:size self)}})
    (let [handler (symbol (str "on-" (name (:type e))))]
      (if (obj/has-method? self handler)
        (self handler e)
        (log/warn :window/missing-handler e)))
    (log/trace :window/after-event {:event e :window {:cursor (:cursor self) :size (:size self)}}))

  (on-screen-size [{:keys [root event-loop] :as self} {:keys [screen-size]}]
    (log/info :window/on-screen-size {:new-size screen-size #_#_:matrix (:matrix self)})
    (swap! self display/resize screen-size)
    (swap! root assoc :dirty? true)
    (event-loop 'enqueue {:type :redraw :origin :resize}))

  (on-redraw [{:keys [size root event-loop matrix] :as self} e]
    (when (and root (not= [0 0] size))
      (swap! self assoc :canvas matrix)
      (if (:dirty? root)
        (root 'draw
              (obj/create surface/Surface {:x 0 :y 0
                                           :width (first size)
                                           :height (second size)
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
              (doseq [a (set/difference (:trace c) (:last-trace c))
                      :let [i (rand-int 1000)]]
                (log/trace :window/watching a :i i)
                (add-watch a [::redraw c]
                           (fn [_ _ old new]
                             (log/trace :window/redraw-triggered i :old old :new new)
                             (swap! c assoc :dirty? true)
                             (event-loop 'enqueue {:type :redraw :origin (obj/klassname c)})))
                (log/trace :window/watched a :i i))
              (doseq [a (set/difference (:last-trace c) (:trace c))]
                (remove-watch a [::redraw c]))
              (swap! c assoc :last-trace (:trace c))))
      (self 'flush!))))

(obj/defklass InlineWindow [Window]
  :- {:lines int?}
  (prep [{:keys [lines auto-grow] :as opts}]
    (assoc ((get Window 'prep) opts)
           :lines lines
           :auto-grow auto-grow))

  (on-screen-size [{:keys [root event-loop conn] :as self} {:keys [screen-size]}]
    (let [[width _] screen-size]
      (log/info :inline-window/on-screen-size {:new-size screen-size :matrix (:matrix self)})
      (conn/write conn "\r")
      (swap! self (fn [self]
                    (-> self
                        (assoc-in [:cursor 0] 0)
                        (display/resize [width (:lines self)]))))
      (swap! root assoc :dirty? true)
      (event-loop 'enqueue {:type :redraw :origin :resize})))

  (grow-viewport [{:keys [size cursor conn] :as self} lines]
    (log/debug :inline-window/grow-viewport {:from size :to lines})
    (let [[cols rows] size]
      (conn/write conn (term/move-relative cursor [0 (dec rows)]))
      (conn/write conn (apply str (repeat (- lines rows) "\n")))
      (swap! self
             (fn [self]
               (-> self
                   (assoc :cursor [0 (dec lines)])
                   (display/resize [cols lines]))))))

  (on-redraw [{:keys [size root] :as self} e]
    (log/debug :root/min-size (root 'minimum-size)
               :not-zero (not= size [0 0])
               :auto-grow (:auto-grow self))
    (when (and root (not= size [0 0]) (:auto-grow self))
      (let [[_ lines] (root 'minimum-size)]
        (when (< (second size) lines)
          (self 'grow-viewport lines))))
    (obj/supercall self 'on-redraw e)))
