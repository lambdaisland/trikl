(ns lambdaisland.trikl1.telnet
  "Telnet connection implementation"
  (:require [lambdaisland.trikl1.connection :as conn]
            [lambdaisland.trikl1.io :as io]
            [lambdaisland.trikl1.input-events :as input-events])
  (:import (java.net SocketOutputStream ServerSocket Socket)
           (javax.net ServerSocketFactory)
           (java.io InputStream IOException OutputStream StringWriter)
           (java.nio ByteBuffer CharBuffer)
           (java.nio.charset Charset CharsetDecoder CodingErrorAction)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* #_true :warn-on-boxed)

(defn ^ServerSocket server-socket [^long port]
  (.createServerSocket (ServerSocketFactory/getDefault) port))

(defn accept-connection ^Socket [^ServerSocket server-socket]
  (doto (.accept server-socket)
    (.setTcpNoDelay true)))

(def TELNET
  {:SE   0xf0 ; SUBNEGOTIATION_END
   :SB   0xfa ; SUBNEGOTIATION
   :WILL 0xfb
   :WONT 0xfc
   :DO   0xfd
   :DONT 0xfe
   :IAC  0xff

   :TRANSMIT_BINARY   0x00
   :ECHO              0x01
   :RECONNECTION      0x02
   :SUPPRESS_GO_AHEAD 0x03
   :NAWS              0x1f
   :LINEMODE          0x22

   ;; https://tools.ietf.org/html/rfc884
   :TERMINAL_TYPE 24

   ;; https://tools.ietf.org/html/rfc1572
   :NEW_ENVIRONMENT_OPTION 39
   })

(defn telnet-command-bytes ^bytes [args]
  (->> args
       (map #(TELNET % %))
       (map unchecked-byte)
       (into-array Byte/TYPE)))

(defn send-telnet-command [^OutputStream out & args]
  (.write out (telnet-command-bytes args)))

(defn prep-telnet
  "Send some telnet initialization commands upon connection."
  [out]
  (send-telnet-command out
                       :IAC :DO :LINEMODE
                       :IAC :SB :LINEMODE
                       (byte 1) (byte 0)
                       :IAC :SE
                       :IAC :WILL :ECHO
                       :IAC :DO :NAWS))

(defn filter-commands
  "Copy buffer `src` to `dest`, filtering out any Telnet protocol commands along
  the way. These are returned separately."
  [{:keys [^ByteBuffer src ^ByteBuffer dest]}]
  (let [IAC          (unchecked-byte (TELNET :IAC))
        subneg-start (unchecked-byte (TELNET :SB))
        subneg-end   (unchecked-byte (TELNET :SE))
        subcmd-min   (unchecked-byte (TELNET :WILL))
        subcmd-max   (unchecked-byte (TELNET :DONT))
        cmd->kw      (into {} (map (juxt (comp unchecked-byte val) key)) TELNET)
        byte->long   #(if (< (long %) 0)
                        (+ 255 (long %))
                        (long %))]
    (loop [state {:commands []}]
      (let [{:keys [cmd commands]} state]
        (if (not (.hasRemaining src))
          commands
          (let [b (.get src)
                {:keys [cmd cmd? subneg?]} state]
            (cond
              (and subneg? (not= subneg-end b))
              (recur (-> state
                         (update :cmd conj b)))

              (= IAC b)
              (recur (-> state
                         (assoc :cmd? true
                                :cmd [b])))

              (and cmd? (= subneg-start b))
              (recur (-> state
                         (update :cmd conj b)
                         (assoc :cmd? false
                                :subneg? true)))

              (and cmd? (<= subcmd-min b subcmd-max))
              (recur (-> state
                         (update :cmd conj b)))

              (or cmd? (and subneg? (= subneg-end b)))
              (let [command (conj cmd b)]
                (recur (-> state
                           (dissoc :cmd :cmd? :subneg?)
                           (update :commands
                                   conj (with-meta
                                          (vec
                                           (concat
                                            (map #(cmd->kw % (byte->long %)) (take 3 command))
                                            (map byte->long (butlast (drop 3 command)))
                                            (map #(cmd->kw % (byte->long %)) [(last command)])))
                                          {:telnet/raw (mapv byte->long command)})))))

              :else
              (do
                (.put dest b)
                (recur state)))))))))

(defrecord TelnetConnection [^Socket client-socket
                             ^InputStream in
                             ^OutputStream out
                             ^Charset charset
                             ^CharsetDecoder decoder
                             dispatch
                             state
                             init-sequence
                             reset-sequence]
  conn/TerminalConnection
  (init [this]
    (prep-telnet out)
    (when init-sequence
      (conn/write this init-sequence))
    ;; Might be preferable to use telnet commands for this, but this'll do.
    #_(conn/request-screen-size out)
    (conn/start-input-loop this)
    this)
  (process-bytes [this {:keys [^ByteBuffer byte-buf ^ByteBuffer next-byte-buf]
                        ;; We allocate a second buffer the first time we've called,
                        ;; and toggle back and forth between the two on each
                        ;; invocation.
                        :or {next-byte-buf (ByteBuffer/allocate 8192)}
                        :as ctx}]
    ;; Clear the buffer we're going to write to
    (if (.hasRemaining byte-buf)
      (do
        (.clear next-byte-buf)
        (let [commands (filter-commands {:src byte-buf
                                         :dest next-byte-buf})]
          (.flip next-byte-buf)
          (doseq [cmd commands]
            (dispatch
             this
             (if (= [:IAC :SB :NAWS] (take 3 cmd))
               (let [[_ _ _ ^long cx ^long cols ^long rx ^long rows] cmd]
                 (with-meta
                   {:type :screen-size
                    :screen-size [(+ (* 256 cx) cols) (+ (* 256 rx) rows)]}
                   (meta cmd)))
               {:type :telnet :command cmd})))
          (assoc ctx
                 :byte-buf next-byte-buf
                 :next-byte-buf byte-buf)))
      ctx))
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
      (when reset-sequence
        (conn/write this reset-sequence))
      (conn/cancel-input-loop this)
      (.close out)
      (catch Exception e))))

(def telnet-connection
  (conn/wrap-connection
   (fn [{:keys [^Socket client-socket
                charset]
         :or {charset "UTF-8"}
         :as opts}]
     (map->TelnetConnection
      (update
       (merge
        {:charset charset
         :in (.getInputStream client-socket)
         :out (.getOutputStream client-socket)
         :decoder (io/charset-decoder charset)
         :init-sequence conn/default-init-sequence
         :reset-sequence conn/default-reset-sequence
         :state (conn/make-state opts)
         :dispatch conn/default-dispatch}
        opts)
       :charset
       #(Charset/forName %))))))
