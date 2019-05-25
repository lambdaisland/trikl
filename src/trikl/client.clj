(ns trikl.client
  "Create and handle Trikl clients, i.e. a target for rendering and a source for
  input events. Clients can be constructed by hooking up STDIN/STDOUT, or by
  creating a Telnet server and waiting for connections, in which case each
  connection yields a new client instance.

  This namespace also contains most of the input handling and parsing, which
  could potentially be split out."
  (:require [clojure.java.io :as io]
            [trikl.telnet :as telnet]
            [trikl.tree :as tree])
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

   ;; Everything mutable
   :state (atom {
                 ;; Current screen size as queried from the client
                 :size [height width]

                 ;; The VirtualScreen, i.e. a representation of the current terminal state.
                 :screen screen

                 ;; The last rendered UI tree
                 :ui-tree ui-tree

                 ;; ID of the element that has focus
                 :focus "foo"

                 ;; Event listeners
                 :listeners {:listener-key (fn [event])}})})

(def ESC \u001b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State

(defn make-state
  "Create an empty client state."
  []
  (atom {:ui-tree nil
         :screen nil
         :focus nil
         :size nil
         :listeners {}}))

(defn add-listener
  "Add an event listener to the client, identified with the given key.

  The key must be unique, reusing a key will cause the listener to be replaced.
  The key can be used to remove the listener with [[remove-listener]].

  The listener is a function which takes a single argument, the event. The
  client where the event originated from is available as :trikl/client on the
  event's metadata."
  [client key listener]
  (swap! (:state client) assoc-in [:listeners key] listener)
  client)

(defn remove-listener
  "Remove an event listener previously added with [[add-listener]].

  This removes the listener that has the given identifying key."
  [client key]
  (swap! (:state client) update :listeners dissoc key)
  client)

(defn listeners
  "Return all registered listener functions without keys."
  [client]
  (-> client :state deref :listeners vals))

(defn parse-screen-size
  "Given a screen size control sequence as sent from the client, like ESC[80;20R,
  return [row-count column-count]"
  [csi]
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

(defn input-consumer-loop
  "Consume the input stream coming from the terminal, handling terminal commands,
  telnet commands, and regular key input.

  The handler is called for each received event with a map of the form

      {:type :input :char character}
      {:type :telnet :command [telnet-command]}
      {:type :ansi :command [ansi-command]}
  "
  [^InputStream in handler]
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
        ;; Prevent the loop from restarting.
        :done)
      (catch Exception e
        (println "Error during input loop" e)
        ;; Return nil, the loop will restart.
        ))))

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

(defn listener-handler
  "Input loop handlers which converts events to more semantically meaningful
  ones (:screen-size, :input), and propagates them to all listeners of the
  client.

  Messages are augmented with metadata, this allow you to find the client the
  message came from (:trikl/client), the time the event was emitted (:time), and
  the original source event (:telnet/command, :telnet/raw, :ansi/command)."
  [client]
  (fn [{:keys [type command char] :as msg}]
    #_(prn msg)
    (let [emit (fn [message]
                 #_(prn message)
                 (let [message (assoc message :time (System/currentTimeMillis))
                       listeners (listeners client)]
                   (run! (fn [l] (l (vary-meta message assoc :trikl/client client)))
                         listeners)))]
      (case type
        :telnet
        (when (= (take 3 command) [:IAC :SUBNEGOTIATION :NAWS])
          (let [raw-msg (:telnet/raw (meta command))
                [_ _ _ cx cols rx rows] raw-msg]
            (when (and (int? cx) (int? rx) (int? cols) (int? rows))
              (emit (with-meta
                      {:type :screen-size
                       :screen-size [(+ (* 256 rx) rows) (+ (* 256 cx) cols)]}
                      {:telnet/command command
                       :telnet/raw raw-msg})))))

        :ansi
        (if-let [screen-size (parse-screen-size command)]
          (emit (with-meta
                  {:type :screen-size
                   :screen-size screen-size}
                  {:ansi/command command}))
          (if-let [key (get ansi-key-commands command)]
            (emit (with-meta
                    {:type :input
                     :key key}
                    {:ansi/command command}))))

        :input
        (emit (assoc msg :key (char->key (:char msg))))))))

(defn start-input-loop
  "Starts the input loop in a separate thread (future). The loop will keep running until "
  [{:keys [^InputStream in] :as client}]
  (assoc client
    :input-loop
    (future
      (loop [result (input-consumer-loop in (listener-handler client))]
        ;; If this returns it means we got an exception that broke the loop. If
        ;; this was an IOException then we'll assume the input stream was
        ;; closed (i.e. the client went away or we shut it off), and we'll stop
        ;; retrying. Other exceptions are more likely due to faulty event
        ;; listeners. In these case we restart the input loop so events continue
        ;; to be delivered.
        (when-not (= :done result)
          (recur (input-consumer-loop in (listener-handler client))))))))

(defn request-screen-size [^OutputStream out]
  (let [csi (fn [& args]
              (let [^String code (apply str ESC "[" args)]
                (.write out (.getBytes code)))) buffer (byte-array 1024)]
    (csi "s") ;; save cursor position
    (csi "5000;5000H") ;; move to position outside of screen, this will actually move the cursor to the bottom right
    (csi "6n") ;; ask the client to send us the position of the cursor
    (csi "u"))) ;; restore cursor position

(def CSI-ALTERNATE-SCREEN (str ESC "[?1049h"))
(def CSI-REGULAR-SCREEN   (str ESC "[?1049l"))
(def CSI-CLEAR-SCREEN     (str ESC "[2J"))
(def CSI-UPPER-LEFT       (str ESC "[H"))
(def CSI-RESET-STYLES     (str ESC "[m"))
(def CSI-SHOW-CURSOR      (str ESC "[?25h"))
(def CSI-HIDE-CURSOR      (str ESC "[?25l"))

(defn start-client
  "Put the client's terminal in a good state: switch to alternate screen, clear
  the screen, reset the styles, hide the cursor, then request the screen size so
  we have a size to work with when we start rendering."
  [{:keys [^OutputStream out] :as client}]
  (when (:socket client)
    (telnet/prep-telnet out))
  (.write out (.getBytes (str CSI-ALTERNATE-SCREEN
                              CSI-CLEAR-SCREEN
                              CSI-UPPER-LEFT
                              CSI-RESET-STYLES
                              CSI-HIDE-CURSOR)))
  (request-screen-size out))

(defn stop-client
  "Restore the terminal state, then close the client socket so the user gets their
  terminal back."
  [{:keys [^OutputStream out ^Socket socket] :as client}]
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

(defn resize-listener [e]
  (when-let [s (:screen-size e)]
    (let [state (-> e meta :trikl/client :state)]
      (swap! state assoc :size (with-meta s {:trikl/message e})))))

(defn make-client [in out & [socket]]
  (let [client (-> {:socket socket
                    :in     in
                    :out    out
                    :state  (make-state)}
                   (add-listener :resize resize-listener))]
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

(defmacro time-info [expr desc]
  `(let [start# (System/nanoTime)
         ret# ~expr]
     (println (str ~desc ": " (/ (double (- (. System (nanoTime)) start#)) 1000000.0) " ms"))
     ret#))

(defn parents-by-id [node id]
  (if (= (:id node) id)
    [node]
    (some (fn [child]
            (when-let [child (parents-by-id child id)]
              (conj child node)))
          (:children node))))

(defn focus-dispatch-handler [e]
  (let [client (:trikl/client (meta e))
        {:keys [ui-tree focus] :as client-state} @(:state client)]
    (when focus
      (when-let [parent-chain (parents-by-id ui-tree focus)]
        (when-let [node (some (fn [node]
                                (when (get-in node [:widget :on-key])
                                  node))
                              parent-chain)]
          (binding [tree/*component-state* (:state node)]
            ((:on-key (:widget node)) e)))))))

(defn start-server
  ([]
   (start-server identity))
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
             (add-listener client ::focus-event-dispatch focus-dispatch-handler)
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
