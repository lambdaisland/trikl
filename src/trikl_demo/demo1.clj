(ns trikl-demo.demo1
  (:require [trikl.client :as client]
            [trikl.render :as render]
            [trikl.tree :as tree]))

(def state (atom {:id "foo"
                  }))

(defn input [props]
  {:id (:id props (gensym "input"))
   :init-state (constantly "")
   :on-key (fn [e]
             (when (:char e)
               (swap! (tree/cstate) str (:char e))))
   :render (fn [props]
             [:stack {:height 3 :width 20 :bg [150 100 50]}
              [:stack {:x 1 :y 1 :width 18 :bg [100 150 50]}
               @(tree/cstate)]])})


(def server (client/start-server
             (fn [client]
               (render/render! client [input @state]))))

(add-watch state
           ::render
           (fn [_ _ _ new]
             (render/render! server [input @state])))

#_
(client/stop-server server)

(render/focus! server "foo")

#_
(render/render! server [input @state])

#_
(swap! state assoc :txt "llooo!")

#_
(-> server
    :clients
    deref
    first
    :state
    deref
    :ui-tree
    )
