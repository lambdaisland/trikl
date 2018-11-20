(ns trikl.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [lambdaisland.ansi :as ansi]
            [trikl.telnet :as telnet])
  (:import clojure.lang.PersistentVector
           java.io.OutputStream
           [java.net ServerSocket Socket]
           java.util.Iterator))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true #_:warn-on-boxed)

(Thread/setDefaultUncaughtExceptionHandler
 (reify Thread$UncaughtExceptionHandler
   (uncaughtException [_ thread ex]
     (println ex "Uncaught exception on" (.getName thread)))))

(def ESC \u001b)
(def RESET (str ESC "[m"))

(defrecord Charel [char fg bg])
(defrecord VirtualScreen [pos size bounding-box styles stack ^PersistentVector charels])

(def BLANK (map->Charel {:char \space}))

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

(defn csi-move [[^long col ^long row]]
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
          [tag {} (or rst ())])))))

(defmulti draw (fn [element screen]
                 (if (vector? element)
                   (if (keyword? (first element))
                     (first element)
                     (type (first element)))
                   (type element))))

(defn draw-line [line screen]
  (let [[x y] (:pos screen)
        [^long min-x ^long min-y ^long max-x ^long max-y] (:bounding-box screen)
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
  (let [[f attrs children] (split-el el)]
    (draw (f attrs children) screen)))

(defn apply-bounding-box [attrs screen]
  (let [[^long vx ^long vy ^long vw ^long vh] (:bounding-box screen)
        {:keys [^long x ^long y ^long width ^long height styles]
         :or {x 0 y 0 width vw height vh}} attrs
        x (min (+ x vx) (+ vx vw))
        y (min (+ y vy) (+ vy vh))
        width (min width (- vw x))
        height (min height (- vh y))]
    [x y width height]))

(defmethod draw :box [el screen]
  (let [[_ attrs children] (split-el el)
        [x y width height] (apply-bounding-box attrs screen)
        styles (:styles attrs)
        coords (for [col (range x (+ x width))
                     row (range y (+ y height))]
                 [row col])
        screen' (-> screen
                    (push-styles styles)
                    (assoc :pos [x y]
                           :bounding-box [x y (+ x width) (+ y height)])
                    (update :charels
                            (fn [charels]
                              (reduce #(update-in %1 %2 merge styles)
                                      charels
                                      coords))))]
    (-> (reduce #(draw %2 %1) screen' children)
        (assoc :pos (:pos screen)
               :bounding-box (:bounding-box screen))
        (pop-styles))))

(defmethod draw :span [el screen]
  (let [[_ attrs children] (split-el el)
        {:keys [styles]} attrs
        screen' (push-styles screen styles)]
    (-> (reduce #(draw %2 %1) screen' children)
        (pop-styles))))

(defmethod draw String [s screen]
  (let [[x y] (:pos screen)
        [min-x min-y max-x max-y] (:bounding-box screen)]
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
        [min-x min-y max-x max-y] (:bounding-box screen)]
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
        [x y width height] (apply-bounding-box attrs screen)
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
        [x y width height] (apply-bounding-box attrs screen)
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
  (let [[_ attrs children]                         (split-el el)
        [^long x ^long y ^long width ^long height] (apply-bounding-box attrs screen)
        ratios                                     (:ratios attrs (repeat (count children) 1))
        heights                                    (map #(:height (second (split-el %))) children)
        ^long remaining                            (- height (apply + (remove nil? heights)))
        ^long total                                (apply + (keep (fn [[w r]]
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
                                       [:box {:y      y
                                              :height ww}
                                        c]))))
                           []
                           (map vector children heights ratios))]
      (->> children
           (reduce (fn [s ch] (draw ch s)) screen)))))

(defn parse-screen-size [csi]
  (when csi
    (when-let [[_ row col] (re-find #"(\d+);(\d+)R" csi)]
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

(defn request-screen-size [^OutputStream out]
  (let [csi (fn [& args]
              (let [^String code (apply str ESC "[" args)]
                (.write out (.getBytes code))))
        buffer (byte-array 1024)]
    (csi "s") ;; save cursor position
    (csi "5000;5000H")
    (csi "6n")
    (csi "u"))) ;; save cursor position

(defn ^VirtualScreen virtual-screen [rows cols]
  (map->VirtualScreen {:pos [0 0]
                       :size [cols rows]
                       :bounding-box [0 0 cols rows]
                       :styles {:fg nil :bg nil}
                       :stack []
                       :charels (vec (repeat rows (vec (repeat cols BLANK))))}))

(def NULL_ITERATOR
  (reify java.util.Iterator
    (hasNext [_] false)
    (next [_] nil)))

(defn diff-row [^StringBuilder sb row-idx styles ^PersistentVector old ^PersistentVector new]
  (let [^Iterator old-it (if old (.iterator old) NULL_ITERATOR)
        ^Iterator new-it (if new (.iterator new) NULL_ITERATOR)]
    (loop [styles  styles
           col-idx 0
           streak? false
           old-ch  (and (.hasNext old-it) (.next old-it))
           new-ch  (and (.hasNext new-it) (.next new-it))]
      (let [old-next? (.hasNext old-it)
            new-next? (.hasNext new-it)
            cont?     (or old-next? new-next?)]
        (if (= old-ch new-ch)
          (if cont?
            (recur styles
                   (inc col-idx)
                   false
                   (when old-next? (.next old-it))
                   (when new-next? (.next new-it)))
            styles)
          (let [{:keys [char fg bg] :or {char \space}} new-ch
                fg? (not= fg (:fg styles))
                bg? (not= bg (:bg styles))]
            (when-not streak?
              (.append sb (csi-move [col-idx row-idx])))
            (when fg? (.append sb (csi-fg fg)))
            (when bg? (.append sb (csi-bg bg)))
            (.append sb char)
            (let [new-styles (if (or fg? bg?) new-ch styles)]
              (if cont?
                (recur new-styles
                       (inc col-idx)
                       true
                       (when old-next? (.next old-it))
                       (when new-next? (.next new-it)))
                new-styles))))))))

(defn diff-screen [sb ^VirtualScreen old ^VirtualScreen new]
  (let [^PersistentVector old-charels (:charels old)
        ^PersistentVector new-charels (:charels new)
        ^Iterator old-row-it (.iterator old-charels)
        ^Iterator new-row-it (.iterator new-charels)]
    (loop [styles  (:styles old)
           row-idx 0
           old-row (and (.hasNext old-row-it) (.next old-row-it))
           new-row (and (.hasNext new-row-it) (.next new-row-it))]
      (let [old-next? (.hasNext old-row-it)
            new-next? (.hasNext new-row-it)
            cont?     (or old-next? new-next?)]
        (let [styles (diff-row sb row-idx styles old-row new-row)]
          (if cont?
            (recur styles
                   (inc row-idx)
                   (when old-next? (.next old-row-it))
                   (when new-next? (.next new-row-it)))
            (assoc new :styles styles)))))))

(defn start-input-loop [{:keys [in listeners]}]
  (future
    (while true
      (try
        (handle-input
         in
         (fn [event]
           (run! #(% event) (vals @listeners))))
        (catch Throwable t
          (println "Exception in input loop" t))))))

(defn start-client [{:keys [^OutputStream out] :as client}]
  (telnet/prep-telnet out)
  (.write out (.getBytes (str ESC "[?1049h" ;; alternate screen
                              ESC "[2J"     ;; clear screen
                              ESC "[H"      ;; upper left corner
                              ESC "[m"      ;; default colors
                              ESC "[?25l"   ;; hide cursor
                              )))
  (request-screen-size out))

(defn stop-client [{:keys [^OutputStream out ^Socket socket] :as client}]
  (.write out (.getBytes (str ESC "[?1049l" ;; regular screen
                              ESC "[m"      ;; default colors
                              ESC "[?25h"   ;; show cursor
                              )))
  (.close socket))

(defn accept-client [server]
  (let [^Socket socket (telnet/accept-connection server)
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

(defn start-server
  ([client-handler]
   (start-server client-handler 1357))
  ([client-handler port]
   (let [^ServerSocket server (telnet/server-socket port)
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

(defn- render* [{:keys [^VirtualScreen screen ^OutputStream out] :as client} element size]
  (let [empty-screen (apply virtual-screen size)
        prev-screen  @screen
        prev-screen  (let [resize (- (count (:charels prev-screen)) (first size))]
                       (if (> resize 0)
                         (update prev-screen :charels #(into [] (drop resize) %))
                         prev-screen))
        next-screen  (draw element empty-screen)
        sb           (StringBuilder.)
        new-screen   (diff-screen sb (or prev-screen empty-screen) next-screen)
        commands     (str sb)]
    (.write out (.getBytes commands))
    (reset! screen new-screen)))

(defn render [{:keys [size screen out] :as client} element]
  (let [lkey (keyword (str (gensym "render")))]
    (add-listener client
                  lkey
                  (fn [e]
                    (remove-listener client lkey)
                    (when-let [s (:screen-size e)]
                      (render* client element s)))))
  (request-screen-size out)
  nil)

(defn force-render [client element]
  (reset! (:screen client) (apply virtual-screen @(:size client)))
  (render client element))

(defn render-watch! [client element state]
  (render client [element @state])
  (add-watch state ::render (fn [_ _ _ new-state]
                              (render client [element new-state]))))

(defn unwatch! [state]
  (remove-watch state ::render))
