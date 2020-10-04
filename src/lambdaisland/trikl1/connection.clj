(ns lambdaisland.trikl1.connection
  "Create and handle Trikl clients, i.e. a target for rendering and a source for
  input events. Clients can be constructed by hooking up STDIN/STDOUT, or by
  creating a Telnet server and waiting for connections, in which case each
  connection yields a new client instance.

  This namespace also contains most of the input handling and parsing, which
  could potentially be split out."
  (:require [clojure.java.io :as io]
            [lambdaisland.trikl1.input-events :as input-events]
            [lambdaisland.trikl1.io :as trikl-io]
            [lambdaisland.trikl1.util :as util])
  (:import [java.io InputStream IOException OutputStream StringWriter]
           [java.lang ProcessBuilder$Redirect]
           [java.net ServerSocket]
           [java.nio ByteBuffer CharBuffer]
           [java.nio.charset Charset CharsetDecoder]
           [sun.misc Signal SignalHandler]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* #_true :warn-on-boxed)

(defprotocol TerminalConnection
  (init [conn])
  (process-bytes [conn ctx])
  (process-chars [conn ctx])
  (shutdown [conn])
  (write [conn s]))

(def ESC \u001b)

(def CSI-ALTERNATE-SCREEN (str ESC "[?1049h"))
(def CSI-REGULAR-SCREEN   (str ESC "[?1049l"))
(def CSI-CLEAR-SCREEN     (str ESC "[2J"))
(def CSI-UPPER-LEFT       (str ESC "[H"))
(def CSI-RESET-STYLES     (str ESC "[m"))
(def CSI-SHOW-CURSOR      (str ESC "[?25h"))
(def CSI-HIDE-CURSOR      (str ESC "[?25l"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State

(defn make-state
  "Create an empty connection state."
  []
  (atom {:ui-tree nil
         :screen nil
         :focus nil
         :size nil
         :listeners {}}))

(defn add-listener
  "Add an event listener to the connection, identified with the given key.

  The key must be unique, reusing a key will cause the listener to be replaced.
  The key can be used to remove the listener with [[remove-listener]].

  The listener is a function which takes a single argument, the event. The
  connection where the event originated from is available as :trikl/connection on the
  event's metadata."
  [connection key listener]
  (swap! (:state connection) assoc-in [:listeners key] listener)
  connection)

(defn remove-listener
  "Remove an event listener previously added with [[add-listener]].

  This removes the listener that has the given identifying key."
  [connection key]
  (swap! (:state connection) update :listeners dissoc key)
  connection)

(defn listeners
  "Return all registered listener functions without keys."
  [conn]
  (-> conn :state deref :listeners vals))

(defn input-loop [{:keys [^InputStream in ^CharsetDecoder decoder] :as conn}]
  (try
    (loop [{:keys [^ByteBuffer byte-buf
                   ^CharBuffer char-buf]
            :as   ctx}
           {:byte-buf (doto (ByteBuffer/allocate 8192) (.limit 0))
            :char-buf (doto (CharBuffer/allocate 8192) (.limit 0))}]
      (cond
        (.hasRemaining char-buf)
        (recur (process-chars conn ctx))

        (not (.hasRemaining byte-buf))
        (let [byte-buf (trikl-io/read-to-byte-buffer in byte-buf)
              ctx      (assoc ctx :byte-buf byte-buf)]
          (recur (process-bytes conn ctx)))

        :else
        (let [char-buf (trikl-io/charset-decode decoder byte-buf)
              ctx (assoc ctx :char-buf char-buf)]
          (recur ctx))))

    (catch IOException e
      (println "Input loop closed")
      ;; Prevent the loop from restarting.
      :done)
    (catch Exception e
      (println "Error during input loop" e)
      ;; Return nil, the loop will restart.
      )))

(defn start-input-loop
  "Starts the input loop in a separate thread (future). The loop will keep running
  until an IOException occurs or it is cancelled."
  [{:keys [state] :as conn}]
  (swap! state
         assoc
         ::input-loop
         (future
           (loop [result (input-loop conn)]
             ;; If this returns it means we got an exception that broke the loop. If
             ;; this was an IOException then we'll assume the input stream was
             ;; closed (i.e. the client went away or we shut it off), and we'll stop
             ;; retrying. Other exceptions are more likely due to faulty event
             ;; listeners. In these case we restart the input loop so events continue
             ;; to be delivered.
             (when-not (= :done result)
               (recur (input-loop conn)))))))

(defn cancel-input-loop
  "Cancel the input loop future, this will possibly interrupt the thread."
  [{:keys [state] :as conn}]
  (swap! state (fn [{::keys [input-loop] :as state}]
                 (future-cancel input-loop)
                 (dissoc state ::input-loop))))

(defn request-screen-size
  "Request the screen size from the terminal, by requesting to move the cursor out
  of bounds, followed by a request for the cursor position."
  [^OutputStream out]
  (let [csi (fn [& args]
              (let [^String code (apply str ESC "[" args)]
                (.write out (.getBytes code)))) buffer (byte-array 1024)]
    (csi "s") ;; save cursor position
    (csi "5000;5000H") ;; move to position outside of screen, this will actually move the cursor to the bottom right
    (csi "6n") ;; ask the client to send us the position of the cursor
    (csi "u"))) ;; restore cursor position

(defn resize-listener
  "Keep the connection (client terminal) its :size value up to date by listening
  for :screen-size events. These can have multiple sources, SIGWINCH, telnet, or
  from the above [[request-screen-size]] hack."
  [e]
  (when-let [s (:screen-size e)]
    (let [state (-> e meta :trikl/connection :state)]
      (swap! state assoc :size (with-meta s {:trikl/message e})))))

(defn default-dispatch
  "Default dispatch function for connections, dispatch events to all current
  listeners."
  [conn event]
  (let [event (-> event
                  (assoc :time (System/currentTimeMillis))
                  (vary-meta assoc :trikl/connection conn))]
    (doseq [listener (listeners conn)]
      (listener event))))

(defn exec-stty
  "Excute the `stty(1)` command to configure TTY options like input echoing, and
  line handling."
  [& args]
  (let [^Process
        process (-> (ProcessBuilder. (util/string-array (cons "stty" args)))
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

(defn wrap-connection
  "Decorate a connection factory function so it calls the connection's [[init]]
  method, and calls its shutdown upon application shutdown."
  [conn-fn]
  (fn [opts]
    (let [conn (init (conn-fn opts))]
      (util/add-shutdown-hook (fn [] (shutdown conn)))
      conn)))

(defrecord StdioConnection [^InputStream in
                            ^OutputStream out
                            ^Charset charset
                            ^CharsetDecoder decoder
                            dispatch state stty?]
  TerminalConnection
  (init [this]
    (when stty?
      (exec-stty "-echo" "-icanon"))
    (write this (str CSI-ALTERNATE-SCREEN
                     CSI-CLEAR-SCREEN
                     CSI-UPPER-LEFT
                     CSI-RESET-STYLES
                     CSI-HIDE-CURSOR))
    (request-screen-size out)
    (Signal/handle (Signal. "WINCH")
                   (reify SignalHandler
                     (^void handle [this ^Signal s]
                      (let [out (:out (exec-stty "size"))]
                        (when-let [[_ rows cols] (re-find #"(\d+) (\d+)" out)]
                          (dispatch this
                                    (-> {:type        :screen-size
                                         :screen-size [(Long/parseLong rows) (Long/parseLong cols)]}
                                        (with-meta {:trikl/source :SIGWINCH}))))))))
    (start-input-loop this)
    this)
  (process-bytes [_ ctx]
    ctx)
  (process-chars [this ctx]
    (input-events/ansi-process-chars
     ctx
     (fn [event] (->> event
                      input-events/semantic-message
                      (dispatch this)))))
  (write [this s]
    (.write out (.getBytes ^String s charset)))
  (shutdown [this]
    (try
      (when stty?
        (exec-stty "+echo" "+icanon"))
      (write this (str CSI-REGULAR-SCREEN
                       CSI-RESET-STYLES
                       CSI-SHOW-CURSOR))
      (cancel-input-loop this)
      (.close out)
      (catch Exception e))))

(def stdio-connection
  "Create a TerminalConnection using stdin/stdout, i.e. use the terminal that this
  JVM process is running in. Use this in your main method if you are building
  command line tools directly (as opposed to something accessed over a
  socket/telnet)."
  (wrap-connection
   (fn [{:keys [in out charset stty?]
         :or   {in      System/in
                out     System/out
                charset "UTF-8"
                stty?   true}}]
     (map->StdioConnection {:in       in
                            :out      out
                            :charset  (Charset/forName charset)
                            :decoder  (trikl-io/charset-decoder charset)
                            :dispatch default-dispatch
                            :state    (make-state)
                            :stty?    stty?}))))

(def socket-connection
  "Create a connection based on a socket, e.g. a TCPSocket connected to with
  `nc(1)`. In this case we don't have access to the TTY, so it's up to the user
  to make sure it's in raw mode: `stty -echo -icanon`."
  (wrap-connection
   (fn [{:keys [socket] :as opts}]
     (stdio-connection (assoc opts
                              :in (.getInputStream cs)
                              :out (.getOutputStream cs)
                              :stty? false)))))

(comment
  (def ss (java.net.ServerSocket. 8889))

  (def sc (socket-connection (.accept ss)))

  (.shutdown sc)

  (add-listener sc ::foo #(prn [:got %]))

  (listeners sc)


  ((:dispatch sc) sc [:foo "bar"])

  )



;; ;; gnome-terminal <-> kernel? <-> nc <-> socket

;; #_
;; (input-loop sc)
