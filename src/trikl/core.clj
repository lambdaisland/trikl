(ns trikl.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as shell]
            [trikl.telnet :as telnet])
  (:import clojure.lang.PersistentVector
           java.util.Iterator
           java.lang.ProcessBuilder$Redirect
           sun.misc.Signal
           sun.misc.SignalHandler
           [java.io InputStream OutputStream IOException]
           [java.net ServerSocket Socket]
           [java.nio.charset CharsetDecoder Charset CodingErrorAction]
           [java.nio ByteBuffer CharBuffer]
           java.io.StringWriter))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true #_:warn-on-boxed)

(Thread/setDefaultUncaughtExceptionHandler
 (reify Thread$UncaughtExceptionHandler
   (uncaughtException [_ thread ex]
     (println ex "Uncaught exception on" (.getName thread)))))

(def ESC \u001b)
(def CSI_PATTERN #"\x1b\[[\x30-x3F]*[\x20-\x2F]*[\x40-\x7E]")

(defrecord Charel [char fg bg])
(defrecord VirtualScreen [pos size bounding-box styles stack ^PersistentVector charels])

(def BLANK (map->Charel {:char \space}))

;; Can be accessed by client rendering code
(def ^:dynamic *screen-size* nil)
(def ^:dynamic *bounding-box* nil)

(defn box-size []
  (when-let [[x y w h] *bounding-box*]
    [w h]))

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

(defn col ^long [screen]
  (get-in screen [:pos 0]))

(defn row ^long [screen]
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

(defmethod draw :default [o screen]
  (draw (str o) screen))

(defn draw-line [line screen]
  (let [[x y] (:pos screen)
        [^long min-x ^long min-y ^long max-x ^long max-y] (:bounding-box screen)
        line (remove #{\return} ;; don't want no carriage returns
                     (subs line 0 (max 0 (min (count line) (- max-x x)))))
        styles (:styles screen)]
    (reduce (fn [screen char]
              (-> screen
                  (update-in [:charels (row screen) (col screen)]
                             (fn [^Charel ch]
                               (->Charel char (:fg styles (:fg ch)) (:bg styles (:bg ch)))))
                  (update-col inc)))
            screen
            line)))

(defmethod draw nil [_ screen]
  screen)

(defmethod draw java.util.List [els screen]
  (reduce #(draw %2 %1) screen els))

(defmethod draw clojure.lang.Fn [el screen]
  (let [[f attrs children] (split-el el)]
    (draw (f attrs children) screen)))

(defmethod draw clojure.lang.MultiFn [el screen]
  (let [[f attrs children] (split-el el)]
    (draw (f attrs children) screen)))

(defmethod draw clojure.lang.Var [el screen]
  (let [[f attrs children] (split-el el)]
    (draw (f attrs children) screen)))

(defn apply-bounding-box [attrs screen]
  (let [[^long vx ^long vy ^long vw ^long vh] (:bounding-box screen)
        {:keys [^long x ^long y ^long width ^long height styles]
         :or {x 0 y 0 width vw height vh}} attrs
        x (max 0 (min (+ x vx) (+ vx vw)))
        y (max 0 (min (+ y vy) (+ vy vh)))
        width (min width (- vw x))
        height (min height (- vh y))]
    [x y width height]))

(defmethod draw :box [el screen]
  (let [[_ attrs children] (split-el el)
        [x y width height :as bbox] (apply-bounding-box attrs screen)
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
    (binding [*bounding-box* bbox]
      (-> (reduce #(draw %2 %1) screen' children)
          (assoc :pos (:pos screen)
                 :bounding-box (:bounding-box screen))
          (pop-styles)))))

(defmethod draw :span [el screen]
  (let [[_ attrs children] (split-el el)
        {:keys [styles]} attrs
        screen' (push-styles screen styles)]
    (-> (reduce #(draw %2 %1) screen' children)
        (pop-styles))))

(defmethod draw String [s screen]
  (let [[x y] (:pos screen)
        [^long min-x ^long min-y ^long max-x ^long max-y]
        (:bounding-box screen)]
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
        [^long min-x ^long min-y ^long max-x ^long max-y]
        (:bounding-box screen)]
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
        [x y width height :as bbox] (apply-bounding-box attrs screen)
        [tl t tr r br b bl l] (:lines attrs "╭─╮│╯─╰│")]
    (binding [*bounding-box* [(inc x) (inc y) (dec width) (dec height)]]
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
            screen))))

(defmethod draw :cols [el screen]
  (let [[_ attrs children] (split-el el)
        [x y width height :as bbox] (apply-bounding-box attrs screen)
        ratios             (:ratios attrs (repeat (count children) 1))
        widths             (map #(:width (second (split-el %))) children)
        remaining          (- width (apply + (remove nil? widths)))
        total              (apply + (keep (fn [[w r]]
                                            (when (nil? w) r))
                                          (map vector widths ratios)))]
    #_(assert (= (count children) (count ratios) (count widths)))
    (binding [*bounding-box* bbox]
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
             (reduce (fn [s ch] (draw ch s)) screen))))))

(defmethod draw :rows [el screen]
  (let [[_ attrs children]                         (split-el el)
        [^long x ^long y ^long width ^long height :as bbox] (apply-bounding-box attrs screen)
        ratios                                     (:ratios attrs (repeat (count children) 1))
        heights                                    (map #(:height (second (split-el %))) children)
        ^long remaining                            (- height (apply + (remove nil? heights)))
        ^long total                                (apply + (keep (fn [[w r]]
                                                                    (when (nil? w) r))
                                                                  (map vector heights ratios)))]
    #_(assert (= (count children) (count ratios) (count heights)))
    (binding [*bounding-box* bbox]
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
             (reduce (fn [s ch] (draw ch s)) screen))))))

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
            new-next? (.hasNext new-it)]
        (if (= old-ch new-ch)
          (if new-next?
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
              (if new-next?
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
            new-next? (.hasNext new-row-it)]
        (let [styles (diff-row sb row-idx styles old-row new-row)]
          (if new-next?
            (recur styles
                   (inc row-idx)
                   (when old-next? (.next old-row-it))
                   (when new-next? (.next new-row-it)))
            (assoc new :styles styles)))))))

(defn parse-screen-size [csi]
  (when csi
    (when-let [[_ row col] (re-find #"(\d+);(\d+)R" csi)]
      [(Integer/parseInt row) (Integer/parseInt col)])))

(defn ^CharsetDecoder charset-decoder
  ([]
   (charset-decoder "UTF-8"))
  ([encoding]
   (-> (Charset/forName encoding)
       .newDecoder
       (.onMalformedInput CodingErrorAction/IGNORE)
       (.onUnmappableCharacter CodingErrorAction/IGNORE))))

(defn char-in-range? [min ch max]
  (<= (long min) (long ch) (long max)))

(defn input-consumer-loop [^InputStream in handler]
  (let [buf-size 8192
        buffer   (byte-array buf-size)
        buffer'  (byte-array buf-size)
        decoder  (charset-decoder)]
    (try
      (loop [state {}]
        #_(prn state)
        (let [{:keys [^CharBuffer char-buf]} state]
          (if (or (nil? char-buf) (not (.hasRemaining char-buf)))
            ;; Fill byte buffer, pull out telnet codes, decode to char buffer
            (let [end (.read in buffer 0 buf-size)

                  {:keys [dest-end commands]}
                  (telnet/filter-commands {:src buffer :dest buffer' :end end})

                  ^CharacterBuffer char-buf (.decode decoder (ByteBuffer/wrap buffer' 0 dest-end))]
              (run! #(handler {:type :telnet :command %}) commands)
              (recur {:char-buf char-buf}))

            ;; handle next character
            (let [ch (char (.get char-buf))
                  ansi-state (:ansi-state state)]
              (cond
                ;; The ESC [ is followed by any number (including none) of "parameter bytes" in
                ;; the range 0x30–0x3F (ASCII 0–9:;<=>?), then by any number of "intermediate
                ;; bytes" in the range 0x20–0x2F (ASCII space and !"#$%&'()*+,-./), then finally
                ;; by a single "final byte" in the range 0x40–0x7E (ASCII @A–Z[\]^_`a–z{|}~).
                (= ch (char ESC))
                (if (.hasRemaining char-buf)
                  (recur (assoc state
                                :ansi-state :init
                                :ansi-command []))
                  (do
                    (handler {:type :input :char ch})
                    (recur state)))

                (and (= :init ansi-state) (= \[ ch))
                (recur (-> state
                           (assoc :ansi-state :param-bytes)
                           (update :ansi-command conj ch)))

                (and (= :init ansi-state) (= \O ch))
                (recur (-> (assoc state :ansi-state :fn-key)
                           (update :ansi-command conj ch)))

                (and (= :param-bytes ansi-state) (char-in-range? \0 ch \?))
                (recur (update state :ansi-command conj ch))

                (and (#{:param-bytes :interm-bytes} ansi-state) (char-in-range? \space ch \/))
                (recur (-> (assoc state :ansi-state :interm-bytes)
                           (update :ansi-command conj ch)))

                (and (#{:param-bytes :interm-bytes :fn-key} ansi-state) (char-in-range? \@ ch \~))
                (do
                  (handler {:type :ansi :command (String. (chars (into-array Character/TYPE (conj (:ansi-command state) ch))))})
                  (recur (dissoc state :ansi-state :ansi-command)))

                :else
                (do
                  (handler {:type :input :char ch})
                  (recur (dissoc state :ansi-state :ansi-command))))))))
      (catch IOException e
        (println "Input loop closed")
        :done)
      (catch Exception e
        (println "Error during input loop" e)))))

(def ansi-key-commands
  {"" :esc
   "[A" :up
   "[B" :down
   "[C" :right
   "[D" :left
   "[H" :home
   "[E" :end
   "[2~" :insert
   "[3~" :delete
   "OP" :f1
   "OQ" :f2
   "OR" :f3
   "OS" :f4
   "[15~" :f5
   "[17~" :f6
   "[18~" :f7
   "[19~" :f8
   "[20~" :f9
   "[21~" :f10
   "[23~" :f11
   "[24~" :f12})

(defn char->key [ch]
  (cond
    (= 27 (long ch))
    :escape

    (= 32 (long ch))
    :space

    (= \return ch)
    :return

    (char-in-range? 0 ch 31)
    (keyword (str "ctrl-" (char (+ (long ch) 64))))

    (char-in-range? 33 ch 126)
    (keyword (str ch))

    (= ch \u007F)
    :backspace

    :else
    nil))

(defn listener-handler [{:keys [listeners]}]
  (fn [{:keys [type command char] :as msg}]
    #_(prn msg)
    (let [emit (fn [message]
                 #_(prn message)
                 (let [m (assoc message :time (System/currentTimeMillis))]
                   (run! (fn [l] (l m))
                         (vals @listeners))))]
      (case type
        :telnet
        (when (= (take 3 command) [:IAC :SUBNEGOTIATION :NAWS])
          (let [raw-msg (:telnet/raw (meta command))
                [_ _ _ _ cols _ rows] raw-msg]
            (when (and (int? cols) (int? rows))
              (emit {:type :screen-size
                     :screen-size [rows cols]}))))

        :ansi
        (if-let [screen-size (parse-screen-size command)]
          (emit {:type :screen-size
                 :screen-size screen-size})
          (if-let [key (get ansi-key-commands command)]
            (emit {:type :input
                   :key key})))

        :input
        (emit (assoc msg :key (char->key (:char msg))))))))

(defn start-input-loop [{:keys [^InputStream in] :as client}]
  (assoc client
         :input-loop
         (future
           (loop [result (input-consumer-loop in (listener-handler client))]
             (when-not (= :done result)
               (recur (input-consumer-loop in (listener-handler client))))))))

(defn request-screen-size [^OutputStream out]
  (let [csi (fn [& args]
              (let [^String code (apply str ESC "[" args)]
                (.write out (.getBytes code))))
        buffer (byte-array 1024)]
    (csi "s") ;; save cursor position
    (csi "5000;5000H")
    (csi "6n")
    (csi "u")))

(def CSI-ALTERNATE-SCREEN (str ESC "[?1049h"))
(def CSI-REGULAR-SCREEN   (str ESC "[?1049l"))
(def CSI-CLEAR-SCREEN     (str ESC "[2J"))
(def CSI-UPPER-LEFT       (str ESC "[H"))
(def CSI-RESET-STYLES     (str ESC "[m"))
(def CSI-SHOW-CURSOR      (str ESC "[?25h"))
(def CSI-HIDE-CURSOR      (str ESC "[?25l"))

(defn start-client [{:keys [^OutputStream out] :as client}]
  (when (:socket client)
    (telnet/prep-telnet out))
  (.write out (.getBytes (str CSI-ALTERNATE-SCREEN
                              CSI-CLEAR-SCREEN
                              CSI-UPPER-LEFT
                              CSI-RESET-STYLES
                              CSI-HIDE-CURSOR)))
  (request-screen-size out))

(defn stop-client [{:keys [^OutputStream out ^Socket socket] :as client}]
  (try
    (.write out (.getBytes (str CSI-REGULAR-SCREEN
                                CSI-RESET-STYLES
                                CSI-SHOW-CURSOR)))
    (when socket
      (.close socket))
    (catch IOException _)))

(defn add-shutdown-hook [^clojure.lang.IFn f]
  (.addShutdownHook (java.lang.Runtime/getRuntime)
                    (Thread. f)))

(defn make-client [in out & [socket]]
  (let [size (atom nil)
        client {:socket    socket
                :in        in
                :out       out
                :size      size
                :screen    (atom nil)
                :listeners (atom {:resize (fn [e]
                                            (when-let [s (:screen-size e)]
                                              (reset! size (with-meta s {:trikl/message e}))))})}]
    (start-client client)
    (let [client (start-input-loop client)]
      (add-shutdown-hook #(stop-client client))
      client)))

(defn ^"[Ljava.lang.String;" string-array [args]
  (into-array String args))

(defn exec-stty [& args]
  (let [^Process
        process (-> (ProcessBuilder. (string-array (cons "stty" args)))
                    (.redirectInput (ProcessBuilder$Redirect/from (io/file "/dev/tty")))
                    (.start))

        ^StringWriter err (StringWriter.)]
    {:exit (.waitFor process)
     :err @(future
             (with-open [^StringWriter err (StringWriter.)]
               (io/copy (.getErrorStream process) err)
               (.toString err)))
     :out @(future
             (with-open [^StringWriter out (StringWriter.)]
               (io/copy (.getInputStream process) out)
               (.toString out)))}))

(defn stdio-client []
  (exec-stty "-echo" "-icanon")
  (let [client (make-client System/in System/out)]
    (add-shutdown-hook (fn []
                         (stop-client client)
                         (exec-stty "+echo" "+icanon")))
    (Signal/handle (Signal. "WINCH")
                   (reify SignalHandler
                     (^void handle [this ^Signal s]
                      (let [out (:out (exec-stty "size"))]
                        (when-let [[_ rows cols] (re-find #"(\d+) (\d+)" out)]
                          (run! #(% (with-meta {:type :screen-size
                                                :screen-size [(Long/parseLong rows) (Long/parseLong cols)]}
                                      {:trikl/source :SIGWINCH}))
                                (vals @(:listeners client))))))))
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
                  (try
                    (.close server)
                    (catch IOException _)))]
     (future
       (try
         (loop [^Socket client-sock (telnet/accept-connection server)]
           (let [client (make-client (.getInputStream client-sock)
                                     (.getOutputStream client-sock)
                                     client-sock)]
             (swap! clients conj client)
             (client-handler client)
             (recur (telnet/accept-connection server))))
         (catch IOException _
           ;; socket closed
           )
         (catch Throwable t
           (println "Exception in server loop" t))))
     stop!)))

(defn render [{:keys [^VirtualScreen screen ^OutputStream out size] :as client} element]
  (let [size @size]
    (assert (= 2 (count size)))
    (binding [*screen-size*  size
              *bounding-box* (into [0 0] size)]
      (let [sb           (StringBuilder. ^String CSI-RESET-STYLES)
            prev-screen  @screen
            styles       (:styles prev-screen)
            empty-screen (apply virtual-screen size)
            prev-screen  (if (= (:size prev-screen) size)
                           (or prev-screen empty-screen)
                           (do
                             (.append sb CSI-CLEAR-SCREEN)
                             empty-screen))
            next-screen  (draw element empty-screen)
            new-screen   (diff-screen sb prev-screen next-screen)
            commands     (str sb)]
        (.write out (.getBytes commands))
        (reset! screen new-screen))))
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
