(ns lambdaisland.trikl1.connection
  "Create and handle Trikl clients, i.e. a target for rendering and a source for
  input events. Clients can be constructed by hooking up STDIN/STDOUT, or by
  creating a Telnet server and waiting for connections, in which case each
  connection yields a new client instance.

  This namespace also contains most of the input handling and parsing, which
  could potentially be split out."
  (:require [clojure.java.io :as io]
            [lambdaisland.trikl1.input-events :as input-events]
            [lambdaisland.trikl1.term :as term]
            [lambdaisland.trikl1.io :as trikl-io]
            [lambdaisland.trikl1.util :as util])
  (:import (java.io InputStream IOException OutputStream StringWriter)
           (java.lang ProcessBuilder$Redirect)
           (java.net ServerSocket Socket)
           (java.nio ByteBuffer CharBuffer)
           (java.nio.charset Charset CharsetDecoder)
           (sun.misc Signal SignalHandler)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* #_true :warn-on-boxed)

(defprotocol TerminalConnection
  (init [conn])
  (process-bytes [conn ctx])
  (process-chars [conn ctx])
  (shutdown [conn])
  (write [conn s]))

(def schema
  [:map
   [:in some?]
   [:out some?]
   [:charset some?]
   [:decoder some?]
   [:dispatch fn?]
   [:init-sequence string?]
   [:reset-sequence string?]])

(def default-init-sequence
  "Default sequence we send to the terminal upon initializing a new connection"
  (str term/ALTERNATE-SCREEN ; Switch to alternate screen, so we can revert back to what was on the screen later
       term/CLEAR-SCREEN     ; Clear the terminal
       term/UPPER-LEFT       ; Move the cursor to 0,0 so we know where we are
       term/RESET-STYLES     ; Reset styles (colors etc)
       term/HIDE-CURSOR      ; Hide the cursor
       ))

(def default-reset-sequence
  "Default sequence we send to the terminal when cleaning up a connection before
  closing it"
  (str term/REGULAR-SCREEN ; Switch back from alternate to regular screen, should restore whatever was on the screen
       term/RESET-STYLES   ; Make sure we don't leave any lingering style flags
       term/SHOW-CURSOR    ; Show the cursor again
       ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State

#_(defn make-state
    "Create an empty connection state."
    [{:keys [listeners make-state]
      :or {listeners {}}
      :as opts}]
    (if make-state
      (make-state opts)
      (atom {:screen nil
             :size nil
             :listeners listeners})))

#_(defn add-listener
    "Add an event listener to the connection, identified with the given key.

  The key must be unique, reusing a key will cause the listener to be replaced.
  The key can be used to remove the listener with [[remove-listener]].

  The listener is a function which takes a single argument, the event. The
  connection where the event originated from is available as :trikl/connection on the
  event's metadata."
    [connection key listener]
    (swap! (:state connection) assoc-in [:listeners key] listener)
    connection)

#_(defn remove-listener
    "Remove an event listener previously added with [[add-listener]].

  This removes the listener that has the given identifying key."
    [connection key]
    (swap! (:state connection) update :listeners dissoc key)
    connection)

#_(defn listeners
    "Return all registered listener functions without keys."
    [conn]
    (-> conn :state deref :listeners vals))

(defn input-loop
  "Read bytes from the input stream, decode to characters, and let the Connection
  handle them. Connections can implement both [[process-bytes]]
  and [[process-chars]], to handle byte-based (e.g. telnet) as well as character
  based (e.g. ANSI) commands."
  [{:keys [^InputStream in ^CharsetDecoder decoder] :as conn}]
  (try
    (loop [{:keys [^ByteBuffer byte-buf
                   ^CharBuffer char-buf]
            :as   ctx}
           {:byte-buf (doto (ByteBuffer/allocate 8192) (.limit 0))
            :char-buf (doto (CharBuffer/allocate 8192) (.limit 0))}]
      (cond
        ;; If there are any characters left to process, then do that.
        (.hasRemaining char-buf)
        (recur (process-chars conn ctx))

        ;; Else we need to fill the character buffer by first reading bytes...
        (not (.hasRemaining byte-buf))
        (let [byte-buf (trikl-io/read-to-byte-buffer in byte-buf)
              ctx      (assoc ctx :byte-buf byte-buf)]
          (recur (process-bytes conn ctx)))

        ;; And decoding them to characters.
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
#_
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
#_
(defn cancel-input-loop
  "Cancel the input loop future, this will possibly interrupt the thread."
  [{:keys [state] :as conn}]
  (swap! state (fn [{::keys [input-loop] :as state}]
                 (future-cancel input-loop)
                 (dissoc state ::input-loop))))

(def screen-size-request-sequence
  (.getBytes
   (str term/SAVE-CURSOR-POS
        (term/move-to 5000 5000)
        term/REQUEST-POSITION
        term/RESTORE-CURSOR-POS)
   "UTF-8"))

(defn request-screen-size
  "Request the screen size from the terminal, by requesting to move the cursor out
  of bounds, followed by a request for the cursor position."
  [^OutputStream out]
  (.write out ^bytes screen-size-request-sequence))

#_
(defn resize-listener
  "Keep the connection (client terminal) its :size value up to date by listening
  for :screen-size events. These can have multiple sources, SIGWINCH, telnet."
  [e]
  (when-let [s (:screen-size e)]
    (let [state (-> e meta :trikl/connection :state)]
      (swap! state assoc :size (with-meta s {:trikl/message e})))))
#_
(defn cursor-pos-resize-listener
  "Keep the connection (client terminal) its :size value up to date by listening
  for :cursor-pos events, use in combination with the
  above [[request-screen-size]]."
  [e]
  (when-let [[^long w ^long h] (:cursor-pos e)]
    (let [state (-> e meta :trikl/connection :state)]
      (swap! state assoc :size (with-meta [(inc w) (inc h)] {:trikl/message e})))))
#_
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
                            dispatch state stty?
                            init-sequence
                            reset-sequence]
  TerminalConnection
  (init [this]
    (when stty?
      (exec-stty "-echo" "-icanon"))
    (when init-sequence
      (write this init-sequence))
    (request-screen-size out)
    (Signal/handle
     (Signal. "WINCH")
     (reify SignalHandler
       (^void handle [this ^Signal s]
        (let [out (:out (exec-stty "size"))]
          (when-let [[_ rows cols] (re-find #"(\d+) (\d+)" out)]
            (dispatch this
                      (-> {:type        :screen-size
                           :screen-size [(Long/parseLong rows) (Long/parseLong cols)]}
                          (with-meta {:trikl/source :SIGWINCH}))))))))
    #_(start-input-loop this)
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
      (when reset-sequence
        (write this reset-sequence))
      #_(cancel-input-loop this)
      (.close out)
      (catch Exception e))))

(def stdio-connection
  "Create a TerminalConnection using stdin/stdout, i.e. use the terminal that this
  JVM process is running in. Use this in your main method if you are building
  command line tools directly (as opposed to something accessed over a
  socket/telnet)."
  (wrap-connection
   (fn [{:keys [in out charset stty? dispatch]
         :or   {in       System/in
                out      System/out
                charset  "UTF-8"
                stty?    true
                dispatch identity}
         :as   opts}]
     (map->StdioConnection
      (update
       (merge
        {:in             in
         :out            out
         :charset        charset
         :stty?          stty?
         :decoder        (trikl-io/charset-decoder charset)
         :dispatch       dispatch
         :init-sequence  default-init-sequence
         :reset-sequence default-reset-sequence}
        (dissoc opts :listeners))
       :charset
       #(Charset/forName %))))))

(def socket-connection
  "Create a connection based on a socket, e.g. a TCPSocket connected to with
  `nc(1)`. In this case we don't have access to the TTY, so it's up to the user
  to make sure it's in raw mode: `stty -echo -icanon`."
  (wrap-connection
   (fn [{:keys [^Socket socket] :as opts}]
     (stdio-connection (assoc opts
                              :in (.getInputStream socket)
                              :out (.getOutputStream socket)
                              :stty? false)))))
