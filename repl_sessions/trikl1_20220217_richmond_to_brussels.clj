(ns repl-sessions.trikl1-20220217-richmond-to-brussels
  (:require [lambdaisland.trikl1.telnet :as telnet]
            [lambdaisland.trikl1.term :as term]
            [lambdaisland.trikl1.connection :as conn]
            [lambdaisland.trikl1.screen :as screen]
            [trikl.tree :as tree]))

(def ss (telnet/server-socket 9999))
(def cs (telnet/accept-connection ss))

(def tc (telnet/telnet-connection {:client-socket cs
                                   :init-sequence nil
                                   :reset-sequence nil
                                   :listeners {::debug prn}}))


(conn/write tc term/REQUEST-POSITION)

(conn/write tc "\n\n\n\n")

(def screen
  (atom
   (assoc
     (screen/new-screen 5 60)
     :term-state {:cursor [0 4]}
     :conn tc)))

(defn draw-line [charels line x y-idx styles]
  (loop [charels        charels
         x-idx          x
         [char & chars] line]
    (if char
      (do
        (recur (update-in charels [y-idx x-idx]
                          (fn [ch]
                            (assoc ch
                                   :char char
                                   :fg (:fg styles)
                                   :bg (or (:bg styles) (:bg ch)))))
               (inc x-idx)
               chars))
      charels)))

(defn draw! [!screen f & args]
  (swap!
   !screen
   (fn [{:keys [charels term-state conn] :as screen}]
     (let [new-charels (apply f charels args)
           sb (StringBuilder.)
           new-state (screen/diff sb term-state charels new-charels)]
       (conn/write tc (str sb))
       (assoc screen
              :charels new-charels
              :term-state new-state)))))


(draw! screen
       draw-line
       "Hello, world"
       0
       0
       {:fg [130 130 150]})

@screen
(def prev @screen)

(conn/shutdown tc)
(screen/new-screen 5 60)
(:cursor @screen)
(conn/write tc (str sb))
