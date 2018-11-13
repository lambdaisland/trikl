(ns trikl.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lambdaisland.ansi :as ansi]
            [trikl.telnet :as telnet]))

(Thread/setDefaultUncaughtExceptionHandler
 (reify Thread$UncaughtExceptionHandler
   (uncaughtException [_ thread ex]
     (println ex "Uncaught exception on" (.getName thread)))))

(def ESC \u001b)
(def RESET (str ESC "[m"))

(defrecord Charel [char fg bg])

(defn csi-fg [color]
  (if (nil? color)
    (str ESC "[39m")
    (let [[r g b] color]
      (str ESC "[38;2;" r ";" g ";" b "m"))))

(defn csi-bg [color]
  (if (nil? color)
    (str ESC "[49m")
    (let [[r g b] color]
      (str ESC "[48;2;" r ";" g ";" b "m"))))

(defn csi-move [[col row]]
  (str ESC "[" (inc row) ";" (inc col) "H"))

(defn assoc-col [screen col]
  (assoc-in screen [:pos 0] col))

(defn update-col [screen f & args]
  (apply update-in screen [:pos 0] f args))

(defn assoc-row [screen row]
  (assoc-in screen [:pos 1] row))

(defn update-row [screen f & args]
  (apply update-in screen [:pos 1] f args))

(defn col [screen]
  (get-in screen [:pos 0]))

(defn row [screen]
  (get-in screen [:pos 1]))

(defn push-styles [screen styles]
  (-> screen
      (update :stack conj (:styles screen))
      (update :styles merge styles)))

(defn pop-styles [screen]
  (-> screen
      (assoc :styles (last (:stack screen)))
      (update :stack pop)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn split-el [el]
  (when (vector? el)
    (let [[tag & rst] el]
      (let [fst (first rst)]
        (if (map? fst)
          [tag fst (rest rst)]
          [tag {} rst])))))

(defmulti draw (fn [element screen]
                 (if (vector? element)
                   (first element)
                   (type element))))

(defn draw-line [line screen]
  (let [[x y] (:pos screen)
        [min-x min-y max-x max-y] (:viewport screen)
        line (remove #{\return} ;; don't want no carriage returns
                     (subs line 0 (max 0 (min (count line) (- max-x x)))))]
    (reduce (fn [screen char]
              (-> screen
                  (update-in [:charels (row screen) (col screen)]
                             #(-> %
                                  (assoc :char char)
                                  (merge (:styles screen))))
                  (update-col inc)))
            screen
            line)))

(defmethod draw java.util.List [els screen]
  (reduce #(draw %2 %1) screen els))

(defmethod draw clojure.lang.Fn [el screen]
  (let [[f attrs children] (split-el)]
    (draw (f attrs children) screen)))

(defn apply-viewport [attrs screen]
  (let [[vx vy vw vh] (:viewport screen)
        {:keys [x y width height styles]
         :or {x 0 y 0 width vw height vh}} attrs
        x (min (+ x vx) (+ vx vw))
        y (min (+ y vy) (+ vy vh))
        width (min width (- vw x))
        height (min height (- vh y))]
    [x y width height]))

(defmethod draw :box [el screen]
  (let [[_ attrs children] (split-el el)
        [x y width height] (apply-viewport attrs screen)
        styles (:styles attrs)
        coords (for [col (range x (+ x width))
                     row (range y (+ y height))]
                 [row col])
        screen' (-> screen
                    (push-styles styles)
                    (assoc :pos [x y]
                           :viewport [x y (+ x width) (+ y height)])
                    (update :charels
                            (fn [charels]
                              (reduce #(update-in %1 %2 merge styles)
                                      charels
                                      coords))))]
    (-> (reduce #(draw %2 %1) screen' children)
        (assoc :pos (:pos screen)
               :viewport (:viewport screen))
        (pop-styles))))

(defmethod draw :span [el screen]
  (let [[_ attrs children] (split-el el)
        {:keys [styles]} attrs
        screen' (push-styles screen styles)]
    (-> (reduce #(draw %2 %1) screen' children)
        (pop-styles))))

(defmethod draw String [s screen]
  (let [[x y] (:pos screen)
        [min-x min-y max-x max-y] (:viewport screen)]
    (let [lines (str/split s #"(?<=\n)")]
      (reduce (fn [screen line]
                (if (>= (row screen) max-y)
                  (reduced screen)
                  (let [nl? (= \newline (last line))
                        line (cond->> line nl? butlast nl? (apply str))]
                    (cond-> (draw-line line screen)
                      nl? (assoc-col min-x)
                      nl? (update-row inc)))))
              screen
              lines))))

(defmethod draw Character [char screen]
  (let [[x y] (:pos screen)
        [min-x min-y max-x max-y] (:viewport screen)]
    (if (and (<= min-x x max-x) (<= min-y y max-y))
      (-> screen
          (update-in [:charels (row screen) (col screen)]
                     #(-> %
                          (assoc :char char)
                          (merge (:styles screen))))
          (update-col inc))
      screen)))

(defmethod draw :line-box [el screen]
  (let [[_ attrs children] (split-el el)
        [x y width height] (apply-viewport attrs screen)
        [tl t tr r br b bl l] (:lines attrs "╭─╮│╯─╰│")]
    (draw [:box attrs
           tl (repeat (- width 2) t) tr "\n"
           (repeat (- height 2)
                   (str l
                        (apply str (repeat (- width 2) \space))
                        r
                        "\n"))
           bl (repeat (- width 2) b) br "\n"
           [:box (assoc attrs :x 1 :y 1 :width (- width 2) :height (- height 2))
            children]]
          screen)))

(defmethod draw :cols [el screen]
  (let [[_ attrs children] (split-el el)
        [x y width height] (apply-viewport attrs screen)
        ratios             (:ratios attrs (repeat (count children) 1))
        widths             (map #(:width (second (split-el %))) children)
        remaining          (- width (apply + (remove nil? widths)))
        total              (apply + (keep (fn [[w r]]
                                            (when (nil? w) r))
                                          (map vector widths ratios)))]
    (assert (= (count children) (count ratios) (count widths)))
    (let [children (reduce (fn [res [c w r]]
                             (let [x  (apply + (map #(get-in % [1 :width]) res))
                                   ww (or w (Math/round (double (/ (* remaining r) total))))
                                   ww (if (= (count res) (dec (count children)))
                                        (- width x)
                                        ww)]
                               (conj res
                                     (if w
                                       (-> c
                                           (assoc-in [1 :x] x)
                                           (assoc-in [1 :width] ww))
                                       [:box {:x x
                                              :width ww}
                                        c]))))
                           []
                           (map vector children widths ratios))]
      (->> children
           (reduce (fn [s ch] (draw ch s)) screen)))))

(defmethod draw :rows [el screen]
  (let [[_ attrs children] (split-el el)
        [x y width height] (apply-viewport attrs screen)
        ratios             (:ratios attrs (repeat (count children) 1))
        heights            (map #(:height (second (split-el %))) children)
        remaining          (- height (apply + (remove nil? heights)))
        total              (apply + (keep (fn [[w r]]
                                            (when (nil? w) r))
                                          (map vector heights ratios)))]
    (assert (= (count children) (count ratios) (count heights)))
    (let [children (reduce (fn [res [c w r]]
                             (let [y  (apply + (map #(get-in % [1 :height]) res))
                                   ww (or w (Math/round (double (/ (* remaining r) total))))
                                   ww (if (= (count res) (dec (count children)))
                                        (- height y)
                                        ww)]
                               (conj res
                                     (if w
                                       (-> c
                                           (assoc-in [1 :y] y)
                                           (assoc-in [1 :height] ww))
                                       [:box {:y     y
                                              :height ww}
                                        c]))))
                           []
                           (map vector children heights ratios))]
      (->> children
           (reduce (fn [s ch] (draw ch s)) screen)))))

(defn parse-screen-size [csi]
  (when csi
    (let [[_ row col] (re-find #"(\d+);(\d+)R" csi)]
      [(Integer/parseInt row) (Integer/parseInt col)])))

(defn handle-input [in handler]
  (let [reader (io/reader in)
        buffer (char-array 1024)
        n      (.read reader buffer)]
    (if (= -1 n)
      :eof
      (loop [[x & xs] (take n buffer)]
        (if x
          (cond
            (= ESC x)
            (let [[_ csi rst] (ansi/next-csi (apply str x xs))
                  size        (parse-screen-size csi)]
              (when size
                (handler {:type        :screen-size
                          :screen-size size}))
              (recur rst))

            :else
            (do
              (handler {:type      :input
                        :key       (if (= \return x) \newline x)
                        :codepoint (long x)})
              (recur xs))))))))

(defn request-screen-size [out]
  (let [csi (fn [& args]
              (.write out (.getBytes (apply str  ESC "[" args))))
        buffer (byte-array 1024)]
    (csi "s") ;; save cursor position
    (csi "5000;5000H")
    (csi "6n")
    (csi "u"))) ;; save cursor position

(defn virtual-screen [rows cols]
  {:pos [0 0]
   :size [cols rows]
   :viewport [0 0 cols rows]
   :styles {:fg nil :bg nil}
   :stack []
   :charels (into []
                  (map (fn [_]
                         (into []
                               (map (fn [_ ]
                                      (map->Charel {:char \space})))
                               (range cols))))
                  (range rows))})

(defn row= [old new row-idx]
  (let [new-row (get-in new [:charels row-idx])
        old-row (get-in old [:charels row-idx])]
    (= new-row old-row)))

(defn render-stretch [sb styles charels]
  (reduce (fn [styles {:keys [char fg bg]}]
            (when (not= fg (:fg styles))
              (.append sb (csi-fg fg)))
            (when (not= bg (:bg styles))
              (.append sb (csi-bg bg)))
            (.append sb char)
            (assoc styles :fg fg :bg bg))
          styles
          charels))

(defn diff-screen [sb old new]
  (let [[_ _ max-col max-row] (:viewport new)
        diff-rows (remove (partial row= old new) (range max-row))]
    (->> diff-rows
         (reduce
          (fn [styles row-idx]
            (let [old-row (get-in old [:charels row-idx])
                  new-row (get-in new [:charels row-idx])
                  old-cnt (count old-row)
                  new-cnt (count new-row)
                  old-row (cond-> old-row
                            (> new-cnt old-cnt)
                            (concat (repeat (- new-cnt old-cnt)
                                            (map->Charel {}))))
                  pairs (map vector (range) old-row new-row)]
              (loop [styles styles
                     pairs (remove (fn [[_ o n]] (= o n)) pairs)]
                (if (seq pairs)
                  (let [[[col-idx] :as stretch]
                        (->> pairs
                             (partition-all 2 1)
                             (take-while (fn [[[c1] [c2]]] (= (inc c1) c2)))
                             (map second)
                             (cons (first pairs)))
                        charels (map last stretch)]
                    (.append sb (csi-move [col-idx row-idx]))
                    (recur (render-stretch sb styles charels)
                           (drop (count charels) pairs)))
                  styles))))
          (:styles old))
         (assoc new :styles))))

(defn start-input-loop [{:keys [in listeners]}]
  (future
    (try
      (while true
        (handle-input
         in
         (fn [event]
           (run! #(% event) (vals @listeners)))))
      (catch Throwable t
        (println "Exception in input loop" t)))))

(defn start-client [{:keys [out] :as client}]
  (telnet/prep-telnet out)
  (.write out (.getBytes (str ESC "[?1049h" ;; alternate screen
                              ESC "[2J"     ;; clear screen
                              ESC "[H"      ;; upper left corner
                              ESC "[m"      ;; default colors
                              ESC "[?25l"   ;; hide cursor
                              )))
  (request-screen-size out))

(defn stop-client [{:keys [out socket] :as client}]
  (.write out (.getBytes (str ESC "[?1049l" ;; regular screen
                              ESC "[m"      ;; default colors
                              ESC "[?25h"   ;; show cursor
                              )))
  (.close socket))

(defn accept-client [server]
  (let [socket (telnet/accept-connection server)
        size (atom nil)
        client {:socket    socket
                :in        (.getInputStream socket)
                :out       (.getOutputStream socket)
                :size      size
                :screen    (atom nil)
                :listeners (atom {:resize (fn [e]
                                            (when-let [s (:screen-size e)]
                                              (when (not= s @size)
                                                (reset! size s))))})}]
    (start-client client)
    (start-input-loop client)
    client))

(defn add-listener [client key listener]
  (swap! (:listeners client) assoc key listener))

(defn remove-listener [client key]
  (swap! (:listeners client) dissoc key))

(defmacro time-info [expr desc]
  `(let [start# (System/nanoTime)
         ret# ~expr]
     (println (str ~desc ": " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " ms"))
     ret#))

(defn render [{:keys [size screen out] :as client} element]
  (prn [:render @size])
  (when-let [size @size]
    (let [empty-screen (apply virtual-screen size)
          prev-screen  @screen
          next-screen  (time-info (draw element empty-screen) "draw")
          sb           (StringBuilder.)
          new-screen   (time-info (diff-screen sb (or prev-screen empty-screen) next-screen) "diff")
          commands     (str sb)]
      (request-screen-size out)
      (.write out (.getBytes commands))
      (reset! screen new-screen)
      nil)))

(defn force-render [client element]
  (reset! (:screen client) (apply virtual-screen @(:size client)))
  (render client element))

(defn start-server
  ([client-handler]
   (start-server client-handler 1357))
  ([client-handler port]
   (let [server (telnet/server-socket port)
         clients (atom [])
         stop!  (fn []
                  (run! stop-client @clients)
                  (.close server))]
     (future
       (try
         (loop [client (accept-client server)]
           (swap! clients conj client)
           (client-handler client)
           (recur (accept-client server)))
         (catch Throwable t
           (println "Exception in server loop" t))))
     stop!)))
