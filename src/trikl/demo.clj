(ns trikl.demo
  (:require [trikl.core :as t]))

(def client (atom nil))

(def stop-server
  (t/start-server #(reset! client %) 1359))

;; telnet localhost 1357
(defn component [attrs children]
  (let [count (atom 1)]
    (fn [attrs children]
      [:span {:id :count
              :on-key (fn [e]
                        (swap! count inc))}
       "Count is " @count])))

(stop-server)
(t/render @client [component {:id :component}])

(t/add-listener @client
                ::listen
                (fn [e]
                  (let [screen @(:screen @client)
                        focus (:focus screen)
                        compo (get-in screen [:elements focus])]
                    (when-let [handler (and compo (:on-key (second compo)))]
                      (handler e)
                      (t/render @client (get-in screen [:elements :trikl.core/root]))))))



(-> @(:screen @client)
    :elements
    :count
    second
    :on-key
    (apply [nil]))


#_(stop-server)
#_(t/stop-client @client)

#_(t/stdio-client)







;; Strings
(t/render @client "Hello, world!")



;; Spans with styles
(t/render @client [:line-box {:styles {:fg [50 50 200]
                                       :bg [255 100 100]}}
                   "Hello, world!"])



;; Box for positioning
(t/render @client [:box {:x 2 :y 2}
                   [:span {:styles {:fg [50 50 200]
                                    :bg [255 100 100]}}
                    "Hello, world!"]])

(prn :x)

(:charels @(:screen @client))

;; Box with size
(t/render @client [:box {:x 2 :y 2 :width 10 :height 2 :styles {:bg [255 100 100]}}
                   [:span {:styles {:fg [50 50 200]}}
                    "Hello, world!"]])



;; Line box
(t/render @client [:line-box {:x 20 :y 10
                              :width 15 :height 5
                              :styles {:bg [255 100 100]}}
                   [:span {:styles {:fg [50 50 200]}}
                    "Hello, world!"]])



(t/render @client [:line-box {:x 20 :y 10
                              :styles {:bg [255 100 100]}
                              }
                   [:span {:styles {:fg [50 50 200]}}
                    "Hello, world!"]])




;; Columns
(t/render @client [:cols
                   [:line-box {:styles {:bg [0 100 255]}}]
                   [:line-box {:styles {:bg [255 100 100]}}]])




;; Rows
(t/render @client [:rows
                   [:line-box {:styles {:bg [0 100 255]}}]
                   [:line-box {:styles {:bg [255 100 100]}}]])




;; Columns/rows divide remaining space
(t/render @client [:rows
                   [:line-box {:styles {:bg [0 100 255]}}]
                   [:line-box {:height 7 :styles {:bg [255 100 100]}}]])




;; Components
(defn component [attrs children]
  [:rows
   [:line-box {:styles {:bg [0 100 255]}}]
   [:line-box {:styles {:bg [255 100 100]}}]])

(t/render @client [:cols
                   [component]
                   [component]])



;; Event listener
(t/add-listener @client
                ::input-listener
                #(t/render (:trikl/client (meta %))
                           [:rows
                            [:box]
                            [:line-box {:height 8}
                             "Last message:\n"
                             (prn-str %)]]))




;; Little demo app
(def state (atom {:x 10
                  :y 5
                  :color [(rand-int 255) (rand-int 255) (rand-int 255)]
                  :last-message nil}))

(defn app [{:keys [x y color last-message]} _]
  [:rows
   [:box]
   [:line-box {:height 8}
    "Last message:\n"
    (prn-str last-message)]])

(defn render! [client]
  (t/render client [#'app @state]))

(t/add-listener @client
                ::input-listener
                (fn [msg]
                  (swap! state assoc :last-message msg)
                  (render! (:trikl/client (meta msg)))))




;; Add interactivity

(defn app [{:keys [x y color last-message]} _]
  [:rows
   [:box
    [:box {:x x :y y :height 1 :width 1 :styles {:fg color}} "●" #_"⏣"]]
   [:line-box {:height 8}
    "Last message:\n"
    (prn-str last-message)]])

(t/add-listener @client
                ::input-listener
                (fn [msg]
                  (swap!
                   state
                   (fn [state]
                     (let [state (assoc state :last-message msg)]
                       (case (:key msg)
                         :up (update state :y dec)
                         :down (update state :y inc)
                         :left (update state :x dec)
                         :right (update state :x inc)
                         :space (assoc state :color [(rand-int 255) (rand-int 255) (rand-int 255)])
                         state))))
                  (render! (:trikl/client (meta msg)))))

#_
(t/remove-listener @client ::input-listener)
