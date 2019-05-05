(ns trikl.client
  "Create and handle Trikl clients, i.e. a target for rendering and a source for
  input events. Clients can be constructed by hooking up STDIN/STDOUT, or by
  creating a Telnet server and waiting for connections, in which case each
  connection yields a new client instance.

  This namespace also contains most of the input handling and parsing, which
  could potentially be split out."
  (:require [clojure.java.io :as io]
            [trikl.telnet :as telnet])
  (:import [java.io InputStream IOException OutputStream StringWriter]
           [java.lang Integer ProcessBuilder$Redirect]
           [java.net ServerSocket Socket]
           [java.nio ByteBuffer CharBuffer]
           [java.nio.charset Charset CharsetDecoder CodingErrorAction]
           [sun.misc Signal SignalHandler]))

(comment
  {;; Network socket for Telnet based clients
   :socket    socket

   ;; InputStream for events
   :in        in

   ;; OutputStream for drawing
   :out       out

   ;; Current screen size as queried from the client
   :size      (atom [width height])

   ;; The VirtualScreen, i.e. a representation of the current terminal state.
   :screen    (atom screen)

   ;; Event listeners
   :listeners (atom {:listener-key (fn [event])})})

(def ESC \u001b)

(defn parse-screen-size [csi]
  (when csi
    (when-let [[_ row col] (re-find #"(\d+);(\d+)R" csi)]
      [(Integer/parseInt col) (Integer/parseInt row)])))

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

(defn listener-handler [{:keys [listeners] :as client}]
  (fn [{:keys [type command char] :as msg}]
    #_(prn msg)
    (let [emit (fn [message]
                 #_(prn message)
                 (let [m (assoc message :time (System/currentTimeMillis))]
                   (run! (fn [l] (l (vary-meta m assoc :trikl/client client)))
                         (vals @listeners))))]
      (case type
        :telnet
        (when (= (take 3 command) [:IAC :SUBNEGOTIATION :NAWS])
          (let [raw-msg (:telnet/raw (meta command))
                [_ _ _ cx cols rx rows] raw-msg]
            (when (and (int? cx) (int? rx) (int? cols) (int? rows))
              (emit (with-meta
                      {:type :screen-size
                       :screen-size [(+ (* 256 cx) cols) (+ (* 256 rx) rows)]}
                      {:telnet/command command
                       :telnet/raw raw-msg})))))

        :ansi
        (if-let [screen-size (parse-screen-size command)]
          (emit (with-meta
                  {:type :screen-size
                   :screen-size screen-size}
                  {:trikl/command command}))
          (if-let [key (get ansi-key-commands command)]
            (emit (with-meta
                    {:type :input
                     :key key}
                    {:trikl/command command}))))

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
                :ui-tree   (atom nil)
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

(defn add-listener
  "Add an event listener to the client, identified with the given key.

  The key must be unique, reusing a key will cause the listener to be replaced.
  The key can be used to remove the listener with [[remove-listener]].

  The listener is a function which takes a single argument, the event. The
  client where the event originated from is available as :trikl/client on the
  event's metadata."
  [client key listener]
  (swap! (:listeners client) assoc key listener))

(defn remove-listener
  "Remove an event listener previously added with [[add-listener]].

  This removes the listener that has the given identifying key."
  [client key]
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
   (start-server (atom []) client-handler port))
  ([clients client-handler port]
   (let [^ServerSocket server (telnet/server-socket port)
         stop!                (fn []
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
     {:clients clients
      :stop-fn stop!})))

(defn stop-server [{stop-fn :stop-fn}]
  (stop-fn))
