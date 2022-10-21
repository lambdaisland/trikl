(ns trikl1-20221021-components-reprise
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.screen :as screen]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl1.util :as util]))

(set! *math-context* :warn-on-boxed)

(defprotocol ISurface
  (-width [_])
  (-height [_])
  (-put-char [_ x y charel])
  (-subsurface [_ x y w h]))

(defprotocol IComponent
  (-preferred-size [_])
  (-minimum-size [_])
  (-maximum-size [_])
  (-draw [_ surface]))

(defn viewport-surface [[^long x ^long y] [^long width ^long height] !matrix]
  (reify ISurface
    (-width [_] width)
    (-height [_] height)
    (-put-char [_ x' y' charel]
      (if (and (< y' height)
               (< x' width))
        (swap! !matrix assoc-in [(+ y y') (+ x x')] charel)
        (println "out of bounds"
                 y' '< height '/
                 x' '< width)))
    (-subsurface [_ x' y' w h]
      (prn [:sub x' y' w h])
      (viewport-surface [(+ x x') (+ y y')]
                        [(min w (- width x'))
                         (min h (- height y'))]
                        !matrix))))

(defn styled-text [^String text [^long x ^long y] {:keys [fg bg]}]
  (reify IComponent
    (-preferred-size [_] [(.length text) 1])
    (-minimum-size [_] [(.length text) 1])
    (-maximum-size [_] [(.length text) 1])
    (-draw [_ surface]
      (dotimes [i (.length text)]
        (-put-char surface (+ x i) y (screen/->Charel (.charAt text i) fg bg))))))

(defn stack [& children]
  (reify IComponent
    (-preferred-size [_]
      (let [szs (map -preferred-size children)]
        [(transduce (map first) + szs)
         (transduce (map second) + szs)]))
    (-minimum-size [_]
      (let [szs (map -minimum-size children)]
        [(transduce (map first) + szs)
         (transduce (map second) + szs)]))
    (-maximum-size [_]
      (let [szs (map -maximum-size children)]
        [(transduce (map first) + szs)
         (transduce (map second) + szs)]))
    (-draw [_ surface]
      (let [width (-width surface)]
        (reduce
         (fn [line child]
           (let [[_ height] (-minimum-size child)]
             (-draw child (-subsurface surface 0 line width height))
             (+ line height)))
         0
         children)))))

(defn auto-padding [child top right bottom left]
  (reify IComponent
    (-preferred-size [_]
      (let [[w h] (-preferred-size child)]
        [(+ w left right) (+ h top bottom)]))
    (-minimum-size [_]
      (-minimum-size child))
    (-maximum-size [_]
      [9999 9999])
    (-draw [_ surface]
      (let [w (-width surface)
            h (-height surface)
            [cw ch] (-preferred-size child)
            pad-left (min 0 (long (Math/floor (/ (- w cw) 2))))
            pad-right (- w cw pad-left)
            pad-top  (min 0 (long (Math/floor (/ (- h ch) 2))))
            pad-bottom (- h ch pad-top)]
        (-draw child (-subsurface
                      surface
                      pad-left pad-top
                      (- w pad-right) (- h pad-bottom)))))))



(defn add-blank-lines! [conn n]
  (let [{:keys [state]} conn
        {:keys [screen]} @state
        {:keys [size term-state]} (:screen @state)
        [cols rows] size]
    (conn/write conn (term/move-relative (:cursor term-state) [0 rows]))
    (conn/write conn (apply str (repeat n "\n")))
    (swap! state update :screen
           #(-> %
                (assoc-in [:term-state :cursor 0] 0)
                (update-in [:term-state :cursor 1] + n)
                (screen/resize [cols (+ rows n)])))))

(defn draw! [conn root]
  (let [{:keys [state]} conn
        {:keys [screen]} @state
        {:keys [size !matrix]} screen]
    (let [[_ lines] (-preferred-size root)]
      (if (< (second size) lines)
        (do
          (add-blank-lines! conn (- lines (second size)))
          (reset! !matrix (screen/charel-matrix (first size) lines))
          (-draw root (viewport-surface [0 0] [(first size) lines] !matrix)))
        (do
          (reset! !matrix (screen/charel-matrix screen))
          (-draw root (viewport-surface [0 0] size !matrix)))))))

(time
 (do
   (draw! (conn)
          (stack
           (styled-text "xxx" [5 0] {:fg [100 0 0]})
           (styled-text "Hello, world! !" [6 0] {:fg [0 100 0]})
           (styled-text "Hello, world! !" [6 0] {:fg [0 0 100]})))
   (flush! (conn))))

(time (println "ok"))

(defonce ss (telnet/server-socket 9999))
(defonce connections (atom []))

(defn conn []
  (last @connections))

(defn init-conn [conn]
  (add-watch
   (:state conn)
   ::resize-screen
   (fn [_ _ _ state]
     (println ::resize-screen (:size state))
     (if-let [[w h :as size] (:size state)]
       (cond
         (not (:screen state))
         (swap! (:state conn) assoc :screen (screen/new-screen w 1))

         (not= w (first (:size (:screen state))))
         (swap! (:state conn) update :screen screen/resize [w (second (:size (:screen state)))]))
       state)))
  conn)

(defn telnet-opts [sock]
  {:client-socket sock
   :init-sequence (str "\r" term/HIDE-CURSOR)
   :reset-sequence (str term/SHOW-CURSOR)
   :listeners {::debug prn
               ::size conn/resize-listener}})

(defonce conn-loop
  (future
    (while true
      (let [cs (telnet/accept-connection ss)]
        (println "Connected!")
        (swap! connections
               conj
               (init-conn (telnet/telnet-connection (telnet-opts cs))))))
    (println "conn-loop broke")))



(comment
  (conn/shutdown (conn))

  (conn)

  conn-loop)
