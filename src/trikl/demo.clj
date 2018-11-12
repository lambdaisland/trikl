(ns trikl.demo
  (:require [trikl.core :refer :all]))

(defn app [{:keys [input]}]
  [:box {:x 10 :y 5 :width 20 :height 10 :styles {:bg [50 50 200]}}
   [:box {:x 1 :y 1 :width 18 :height 8 :styles {:bg [200 50 0]}}
    [:span {:styles {:fg [0 0 150]}} "Your input:\n"]
    [:span {:styles {:fg [200 200 200]}} input]]])

(defn client-handler [client]
  (let [state (atom {:input ""})]
    (add-listener client
                  ::key-listener
                  (fn [event]
                    (prn [:client event])
                    (when-let [k (:key event)]
                      (when (<= 10 (long k) 126)
                        (swap! state update :input str k)))))
    (render client (app @state))
    (add-watch (:size client)
               ::on-resize
               (fn [_ _ _ _]
                 (render client (app @state))))
    (add-watch state
               ::on-change
               (fn [_ _ _ state]
                 (render client (app state))))))

#_
(def stop-server (start-server client-handler))

#_
(stop-server)
