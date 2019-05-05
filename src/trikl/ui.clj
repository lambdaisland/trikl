(ns trikl.ui
  (:require [trikl.core :as t]))

(def ^:dynamic *clients*)

(defn client-handler [clients focus state root client]
  (when focus
    (t/focus! client focus))

  (add-watch state ::rerender (fn [& _]
                                (binding [*clients* clients]
                                  (t/render client [root @state]))))

  (t/add-listener client
                  ::event-listener
                  (fn [{:keys [type] :as e}]
                    (let [mapping     {:input       :keypress
                                       :screen-size :resize}
                          handler-key (keyword (str "on-" (name (mapping type type))))]
                      (let [screen    @(:screen client)
                            focus     (:focus screen)
                            [_ attrs] (get-in screen [:elements focus])]
                        (when-let [handler (and attrs (get attrs handler-key))]
                          (handler e)))
                      (if (= type :screen-size)
                        (binding [*clients* clients]
                          (t/render client [root @state])))

                      nil)))

  (binding [*clients* clients]
    (t/render client [root @state])))

(defn start! [{:keys [root state telnet? stdio? telnet-port handler focus]
               :or   {telnet-port 1357}}]
  (let [clients (atom [])
        server  (t/start-server clients
                                (partial #'client-handler clients focus state root)
                                telnet-port)]
    (when stdio?
      (let [client (t/stdio-client)]
        (swap! (:clients server) conj client)
        (#'client-handler clients focus state root client)))

    (when (instance? clojure.lang.Var root)
      (add-watch root
                 ::rerender
                 (fn [& _]
                   (binding [*clients* clients]
                     (doseq [client @clients]
                       (t/rerender! client))))))

    server))

(defn stop! [server]
  (t/stop-server server))

(defn local-state [init-value]
  (let [clients *clients*
        state (atom init-value)]
    (assert clients)
    (add-watch state
               ::rerender
               (fn [& _]
                 (prn "re-rerender" (count @clients))
                 (binding [*clients* clients]
                   (doseq [client @clients]
                     (t/rerender! client)))))
    state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn List [_ _]
  (let [state (local-state {:idx 0})]
    (fn [{:keys [id focused? data render on-keypress on-select]
          :or {render str}} _]
      (when on-select
        (add-watch state
                   ::on-select
                   (fn [& _]
                     (on-select (nth data (:idx @state))))))
      [:box {:id [id :inner]
             :focusable? true
             :on-keypress (fn [e]
                            (case (:key e)
                              :up (swap! state update :idx dec)
                              :down (swap! state update :idx inc)
                              nil)
                            (when on-keypress (on-keypress e)))}
       (let [idx (:idx @state)]
         (map-indexed (fn [idx' item]
                        [:span (if (= idx' idx)
                                 {:styles (if focused?
                                            {:fg [0 0 0] :bg [255 255 255]}
                                            {:fg [0 0 0] :bg [170 180 160]})}
                                 {})
                         (render item) (repeat 100 " ") "\n"])
                      data))])))

(def app-state (atom {}))

(defn with-state [attrs children]
  (prn "with-state")
  (let [counter (local-state 0)]
    (fn [attrs children]
      [:span {:id :xxx
              :on-keypress (fn [e]
                             (prn [:xxx e])
                             (swap! counter inc))}
       "count is " @counter])))

(defn root [attrs children]
  [:span
   #_[with-state {:id :yyy}]

   [List {:id :xxx
          :data [{:x 1}
                 {:x 2}
                 {:x 3}]}]
   ])

(stop! +s+)

(def +s+
  (start! {:root    #'root
           :state   app-state
           :telnet? true
           :focus   [:xxx :inner]}))


(comment
  (:focus

   @(:screen (first @(:clients +s+))))

  )
