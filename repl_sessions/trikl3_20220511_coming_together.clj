(ns repl-sessions.trikl3-20220511-coming-together
  (:require [lambdaisland.trikl1.connection :as conn]
            [lambdaisland.trikl1.log :as log]
            [lambdaisland.trikl1.ratom :as ratom]
            [lambdaisland.trikl1.term :as term]
            [lambdaisland.trikl2.component :as c]
            [lambdaisland.trikl2.ui :as ui]))

(do
  (def model (ratom/ratom
              (vec
               (for [x (range 20)]
                 {:total 1000
                  :amount (rand-int 1000) #_(* 10 x)
                  :delta (rand-nth (remove #{0} (range -10 10)))
                  :color [(rand-int 255)
                          (rand-int 255)
                          (rand-int 255)]}))))

  (reset! model (vec
                 (for [x (range 20)]
                   {:total 1000
                    :amount (rand-int 1000) #_(* 10 x)
                    :delta (rand-nth (remove #{0} (range -10 10)))
                    :color [(rand-int 255)
                            (rand-int 255)
                            (rand-int 255)]})))

  (defn animate-frame! []
    (swap! model
           (fn [ms]
             (mapv (fn [{:keys [amount delta total] :as m}]
                     (let [amount (+ amount delta)]
                       (cond
                         (< amount 0)
                         (assoc m :amount 0 :delta (- delta))
                         (< total amount)
                         (assoc m :amount total :delta (- delta))
                         :else
                         (assoc m :amount amount :delta delta))))
                   ms))))

  ;; (animate-frame!)
  ;; (swap! model update-in [0 :amount] + 100)

  (def stop? false)

  (defonce update-loop
    (future
      (while (not @#'stop?)
        (Thread/sleep 10)
        (animate-frame!))))

  (defn main []
    `[~c/Stack
      ~@(for [i (range 20)]
          [c/ProgressBar {:fg @(ratom/cursor model [i :color])
                          :bg [255 255 255]
                          :model (ratom/cursor model [i])}])])
  @(ratom/cursor model [0 :color])
  (def telnet (ui/telnet-ui main {:mode :fullscreen
                                  :port 9877}))

  (defn win []
    (last @(:wins telnet))))

(conn/request-screen-size
 (:out (:conn (win))))
((:event-loop (win)) 'enqueue
 {:type :screen-size
  :screen-size [77 24]})

(:conn-loop telnet)
(:matrix (win))
(:canvas (win))

((:stop! telnet))
(keys @(:root (last @(:wins telnet))))
(add-watch (first @(:wins telnet))
           ::cursor
           (fn [_ _ old new]
             (if (not= (:cursor old) (:cursor new))
               (log/debug :NEW-CURSOR [(:cursor old) '-> (:cursor new)]))))

(conn/write
 (:conn (first @(:wins telnet)))
 term/REQUEST-POSITION)

((first @(:wins telnet)) 'on-redraw nil)

(:matrix (first @(:wins telnet)))
(:canvas (first @(:wins telnet)))

(count (first (:matrix (first @(:wins telnet)))))





(def state (ratom/ratom {:count 0}))

(swap! state update :count inc)

(defn main []
  [c/Text {:fg [0 0 200]
           :bg [255 255 255]}  "count=" (:count @state)
   "\n" "hello"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
