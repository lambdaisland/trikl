(ns trikl1-20221021-components-reprise
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.screen :as screen]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl1.util :as util])
  (:import (java.util.concurrent Delayed
                                 DelayQueue
                                 PriorityBlockingQueue
                                 TimeUnit
                                 BlockingQueue
                                 LinkedBlockingQueue)))

(set! *warn-on-reflection* true)
(set! *math-context* :warn-on-boxed)

(defprotocol ISurface
  (-width [_])
  (-height [_])
  (-put-char [_ x y charel])
  (-subsurface [_ x y w h])
  (-bounding-box [_])
  (-conn [_]))

(defprotocol IComponent
  (-preferred-size [_])
  (-minimum-size [_])
  (-maximum-size [_])
  (-draw [_ surface])
  (-unmount [_]))

(defn delayed [val ms]
  (let [target-ms (+ ms (System/currentTimeMillis))]
    (reify
      Delayed
      (getDelay [this unit]
        (.convert unit (- target-ms (System/currentTimeMillis))
                  TimeUnit/MILLISECONDS))
      (compareTo [this that]
        (- (.getDelay ^Delayed this TimeUnit/MILLISECONDS)
           (.getDelay ^Delayed that TimeUnit/MILLISECONDS)))
      clojure.lang.IDeref
      (deref [this]
        val))))

(defn emit [conn e]
  (assert conn)
  (assert (:delay-queue conn))
  (assert (:event-queue conn))
  (if-let [ms (:delay-ms e)]
    (.offer ^BlockingQueue (:delay-queue conn) (delayed e ms))
    (.offer ^BlockingQueue (:event-queue conn) e)))

(defn redraw! [component surface]
  (emit (-conn surface)
        {:type :redraw
         :component component
         :bounding-box (-bounding-box surface)
         :priority 0}))

(defn viewport-surface [conn [^long x ^long y] [^long width ^long height]]
  (assert conn)
  (let [!matrix (:!matrix (:screen @(:state conn)))]
    (reify ISurface
      (-conn [_] conn)
      (-width [_] width)
      (-height [_] height)
      (-bounding-box [_] [x y width height])
      (-put-char [_ x' y' charel]
        (if (and (< y' height)
                 (< x' width))
          (swap! !matrix assoc-in [(+ y y') (+ x x')] charel)
          (println "out of bounds"
                   y' '< height '/
                   x' '< width)))
      (-subsurface [_ x' y' w h]
        (viewport-surface conn
                          [(+ x x') (+ y y')]
                          [(min w (- width x'))
                           (min h (- height y'))])))))

(defn styled-text [^String text [^long x ^long y] {:keys [fg bg]}]
  (reify IComponent
    (-preferred-size [_] [(.length text) 1])
    (-minimum-size [_] [(.length text) 1])
    (-maximum-size [_] [(.length text) 1])
    (-draw [_ surface]
      (dotimes [i (.length text)]
        (-put-char surface (+ x i) y (screen/->Charel (.charAt text i) fg bg))))))

(defn spinner [{:keys [fg bg]}]
  (let [!state (atom (cycle (reverse "⣾⣽⣻⢿⡿⣟⣯⣷")))]
    (reify IComponent
      (-preferred-size [_] [1 1])
      (-minimum-size [_] [1 1])
      (-maximum-size [_] [1 1])
      (-draw [this surface]
        (-put-char surface 0 0 (screen/->Charel (first (swap! !state next)) fg bg))
        (emit (-conn surface)
              {:type :redraw
               :component this
               :bounding-box (-bounding-box surface)
               :priority 0
               :delay-ms 200})))))

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
          (-draw root (viewport-surface conn [0 0] [(first size) lines])))
        (do
          (reset! !matrix (screen/charel-matrix screen))
          (-draw root (viewport-surface conn [0 0] size)))))))

(defmulti handle-event (fn [conn e] (:type e)))

(defmethod handle-event :default [conn e]
  (println "event:" e))

(defmethod handle-event :draw [conn e]
  (draw! conn (:root @(:state conn)))
  (screen/flush! conn))

(defmethod handle-event :redraw [conn {:keys [component bounding-box]}]
  (-draw component
         (viewport-surface conn
                           (take 2 bounding-box)
                           (drop 2 bounding-box)))
  (screen/flush! conn))

(defn start-event-loop [conn]
  (let [event-queue (PriorityBlockingQueue.
                     11
                     (comparator (fn [this that]
                                   (< (:priority this 100)
                                      (:priority that 100)))))
        delay-queue (DelayQueue.)
        conn (assoc conn
                    :event-queue event-queue
                    :delay-queue delay-queue)]
    (.start (Thread. (fn []
                       (try
                         (while true
                           (let [e (.take delay-queue)]
                             (.offer event-queue @e)))
                         (catch Throwable t
                           (println "delay-error:" t))))
                     "trikl-delay-handling"))
    (.start (Thread. (fn []
                       (while true
                         (let [e (.take event-queue)]
                           (try
                             (handle-event conn e)
                             (catch Throwable t
                               (println "error handling event:" e "\n" t))))))
                     "trikl-event-handling"))
    (swap! (:state conn)
           (fn [state]
             (assoc-in state [:listeners ::event-forwarding]
                       (fn [e]
                         (.offer event-queue e)))))
    conn))

(defn mount! [conn root]
  (swap! (:state conn) assoc :root root)
  (emit conn {:type :draw
              :priority 0}))


(comment
  (do
    (defonce ss (telnet/server-socket 9999))
    (defonce connections (atom []))

    (defn conn []
      (last @connections))

    (defn init-conn [conn]
      (add-watch
       (:state conn)
       ::resize-screen
       (fn [_ _ _ state]
         (if-let [[w h :as size] (:size state)]
           (cond
             (not (:screen state))
             (swap! (:state conn) assoc :screen (screen/new-screen w 1))

             (not= w (first (:size (:screen state))))
             (swap! (:state conn) update :screen screen/resize [w (second (:size (:screen state)))]))
           state)))
      (start-event-loop conn))

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

    (conn/shutdown (conn))
    )

  (mount! (conn)
          (stack
           (styled-text "xxx" [5 0] {:fg [100 0 0]})
           (styled-text "Hello, world! !" [6 0] {:fg [0 100 0]})
           (styled-text "Hello, world! !" [6 0] {:fg [0 0 100]})))


  )

(:event-queue (conn))

(comment
  (conn/shutdown (conn))

  (conn)

  conn-loop)
