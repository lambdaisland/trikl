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
                  :amount (rand-int 1000)
                  :delta (rand-nth [-3 -2 -1 1 2 3])}))))

  (defn animate-frame! []
    (swap! model
           (fn [ms]
             (mapv (fn [{:keys [amount total delta]}]
                     (let [amount (+ amount delta)]
                       (cond
                         (< amount 0)
                         {:amount 0 :total total :delta (- delta)}
                         (< total amount)
                         {:amount total :total total :delta (- delta)}
                         :else
                         {:amount amount :total total :delta delta})))
                   ms))))

  (animate-frame!)


  (def stop? false)

  (future
    (while (not @#'stop?)
      (Thread/sleep 10)
      (animate-frame!)))

  (defn main []
    `[~c/Stack
      ~@(for [i (range 20)]
          [c/ProgressBar {:fg [(rand-int 255)
                               (rand-int 255)
                               (rand-int 255)]
                          :bg [255 255 255]
                          :model (ratom/cursor model [i])}])])


  (def telnet (ui/telnet-ui main {:mode :fullscreen
                                  :port 9877})))

(:conn-loop telnet)

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
