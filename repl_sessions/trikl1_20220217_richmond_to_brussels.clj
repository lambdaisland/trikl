(ns repl-sessions.trikl1-20220217-richmond-to-brussels
  (:require [lambdaisland.trikl1.telnet :as telnet]
            [lambdaisland.trikl1.term :as term]
            [lambdaisland.trikl1.connection :as conn]
            [lambdaisland.trikl1.screen :as screen]))

(def ss (telnet/server-socket 9999))
(def cs (telnet/accept-connection ss))

(def tc (telnet/telnet-connection {:client-socket cs
                                   :init-sequence nil
                                   :reset-sequence nil
                                   :listeners {::debug prn}}))


(conn/write tc term/REQUEST-POSITION)
(conn/write tc term/HIDE-CURSOR)
(conn/write tc term/SHOW-CURSOR)
(conn/write tc term/CLEAR-SCREEN)

(conn/write tc "\n\n\n\n")

(def screen
  (atom
   (assoc
     (screen/new-screen 1 60)
     :term-state {:cursor [0 0]}
     :conn tc)))

(defn draw-line [matrix line x y-idx styles]
  (loop [matrix        matrix
         x-idx          x
         [char & chars] line]
    (if char
      (do
        (recur (update-in matrix [y-idx x-idx]
                          (fn [ch]
                            (assoc ch
                                   :char char
                                   :fg (:fg styles)
                                   :bg (or (:bg styles) (:bg ch)))))
               (inc x-idx)
               chars))
      matrix)))

(defn draw! [!screen f & args]
  (swap!
   !screen
   (fn [{:keys [matrix term-state conn] :as screen}]
     (let [new-matrix (apply f matrix args)
           sb (StringBuilder.)
           new-state (screen/diff sb term-state matrix new-matrix)]
       (conn/write conn (str sb))
       (assoc screen
              :matrix new-matrix
              :term-state new-state)))))

(defn add-blank-line! [!screen]
  (swap!
   screen
   (fn [{:keys [size term-state conn] :as screen}]
     (let [[rows cols] size]
       (conn/write conn (term/move-relative (:cursor term-state) [0 rows]))
       (conn/write conn "\n")
       (-> screen
           (assoc-in [:term-state :cursor 0] 0)
           (update-in [:term-state :cursor 1] inc)
           (screen/resize [(inc rows) cols]))))))


(add-blank-line! screen)
(draw! screen
       draw-line
       " ► Hello, world---xxx"
       0
       1
       {:fg [150 130 150]})


(defn blank! []
  (draw! screen (fn [_]
                  (screen/charel-matrix (first (:size @screen))
                                        (second (:size @screen))))))

(def state (atom {:choices ["One" "Two" "Three"]
                  :selection 0}))

(defn render-state [!screen !state]
  (let [matrix (screen/charel-matrix @!screen)
        {:keys [choices selection]} @!state]
    (draw!
     screen
     (constantly
      (second
       (reduce (fn [[idx matrix] choice]
                 [(inc idx)
                  (cond-> matrix
                    (= idx selection)
                    (draw-line "►" 1 idx {:fg [0 230 0]})
                    :->
                    (draw-line choice 3 idx {:fg [130 130 0]}))])
               [0 matrix]
               choices))))))

(swap! state update :selection inc)

(conn/add-listener (:conn @screen) ::arrows
                   (fn [{:keys [key]}]
                     (case key
                       :up
                       (swap! state update :selection dec)
                       :down
                       (swap! state update :selection inc)
                       nil)
                     (render-state screen state)))

(def prev @screen)

(conn/shutdown tc)
