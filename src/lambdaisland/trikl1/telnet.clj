(ns lambdaisland.trikl1.telnet
  (:import java.net.ServerSocket
           javax.net.ServerSocketFactory
           [java.io InputStream IOException OutputStream StringWriter]
           [java.nio ByteBuffer CharBuffer]
           [java.nio.charset Charset CharsetDecoder CodingErrorAction]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* #_true :warn-on-boxed)

(defn ^ServerSocket server-socket [^long port]
  (.createServerSocket (ServerSocketFactory/getDefault) port))

(defn accept-connection [^ServerSocket server-socket]
  (doto (.accept server-socket)
    (.setTcpNoDelay true)))

(def TELNET
  {:SUBNEGOTIATION_END 0xf0
   :SUBNEGOTIATION     0xfa
   :WILL               0xfb
   :WONT               0xfc
   :DO                 0xfd
   :DONT               0xfe
   :IAC                0xff

   :TRANSMIT_BINARY   0x00
   :ECHO              0x01
   :RECONNECTION      0x02
   :SUPPRESS_GO_AHEAD 0x03
   :NAWS              0x1f
   :LINEMODE          0x22})

(defn send-telnet-command [^java.net.SocketOutputStream out & args]
  (->> args
       (map #(TELNET % %))
       (map unchecked-byte)
       ^bytes (into-array Byte/TYPE)
       (.write out)))

(defn prep-telnet [out]
  (send-telnet-command out
                       :IAC :DO :LINEMODE
                       :IAC :SUBNEGOTIATION :LINEMODE
                       (byte 1) (byte 0)
                       :IAC :SUBNEGOTIATION_END
                       :IAC :WILL :ECHO
                       :IAC :DO :NAWS))

(defn filter-commands
  "Copy buffer `src` to `dest`, filtering out any Telnet protocol commands along
  the way. These are returned separately."
  [{:keys [^bytes src ^bytes dest ^long end]}]
  #_(prn (take end (seq src)))
  (let [IAC          (unchecked-byte (TELNET :IAC))
        subneg-start (unchecked-byte (TELNET :SUBNEGOTIATION))
        subneg-end   (unchecked-byte (TELNET :SUBNEGOTIATION_END))
        subcmd-min   (unchecked-byte (TELNET :WILL))
        subcmd-max   (unchecked-byte (TELNET :DONT))
        cmd->kw      (into {} (map (juxt (comp unchecked-byte val) key)) TELNET)
        byte->long   #(if (< % 0)
                        (+ 255 (long %))
                        (long %))]
    (loop [state {:src-pos  0
                  :dest-pos 0
                  :commands []}]
      (let [{:keys [src-pos dest-pos cmd commands]} state]
        (if (= src-pos end)
          {:src-pos  (- src-pos (count cmd)) ;; if a command is in flight then
           ;; don't consume the last bytes of
           ;; the buffer
           :dest-end dest-pos
           :commands commands}
          (let [b                          (aget src src-pos)
                {:keys [cmd cmd? subneg?]} state]
            (cond
              (and subneg? (not= subneg-end b))
              (recur (-> state
                         (update :src-pos inc)
                         (update :cmd conj b)))

              (= IAC b)
              (recur (-> state
                         (update :src-pos inc)
                         (assoc :cmd? true
                                :cmd [b])))

              (and cmd? (= subneg-start b))
              (recur (-> state
                         (update :src-pos inc)
                         (update :cmd conj b)
                         (assoc :cmd? false
                                :subneg? true)))

              (and cmd? (<= subcmd-min b subcmd-max))
              (recur (-> state
                         (update :src-pos inc)
                         (update :cmd conj b)))

              (or cmd? (and subneg? (= subneg-end b)))
              (let [command (conj cmd b)]
                (recur (-> state
                           (assoc :src-pos (inc src-pos))
                           (dissoc :cmd :cmd? :subneg?)
                           (update :commands
                                   conj (with-meta
                                          (mapv #(cmd->kw % (byte->long %)) command)
                                          {:telnet/raw (mapv byte->long command)})))))

              :else
              (do
                (aset dest dest-pos b)
                (recur (-> state
                           (update :src-pos inc)
                           (update :dest-pos inc)))))))))))

(defn ^CharsetDecoder charset-decoder
  ([]
   (charset-decoder "UTF-8"))
  ([encoding]
   (-> (Charset/forName encoding)
       .newDecoder
       (.onMalformedInput CodingErrorAction/IGNORE)
       (.onUnmappableCharacter CodingErrorAction/IGNORE))))

(defn charset-decode ^CharBuffer [^CharsetDecoder decoder ^bytes buffer ^long length]
  (.decode decoder (ByteBuffer/wrap buffer 0 length)))

(defn char-telnet-reader
  "Returns a function which reads a chunk of bytes off the input stream, filters
  out telnet commands, converts what remains into characters (currently assumes
  UTF-8), and returns the commands and char-buffer."
  [^InputStream in]
  (let [buf-size 8192
        buffer   (byte-array buf-size)
        buffer'  (byte-array buf-size)
        decoder  (charset-decoder)]
    (fn []
      (let [end (.read in buffer 0 buf-size)
            {:keys [dest-end commands]} (filter-commands {:src buffer :dest buffer' :end end})]
        [commands (charset-decode decoder buffer' dest-end)]))))

#_
(let [src-buf (byte-array 10)
      dest-buf (byte-array 10)]
  (aset src-buf 0 (byte 60))
  (aset src-buf 1 (byte 61))
  (aset src-buf 2 (byte 62))
  (aset src-buf 3 (unchecked-byte (TELNET :IAC)))
  (aset src-buf 4 (unchecked-byte (TELNET :WILL)))
  (aset src-buf 5 (unchecked-byte (TELNET :LINEMODE)))
  (aset src-buf 6 (byte 70))
  (-> (buffer-filter-commands {:src src-buf :dest dest-buf :end 7})
      :commands
      first
      meta)
  #_(seq dest-buf)
  )


;; InputStream == byte based stream
;; read into byte-array
;; strip out low level commands (e.g. telnet)
;; decode to Character -> CharBuffer
;; process CharBuffer, call handler

#_
(let [[commands char-buf] (read-input-stream!)]
  (run! #(handler {:type :telnet :command %}) commands)
  (recur (assoc state :char-buf char-buf)))

#_#_
:telnet
(when (= (take 3 command) [:IAC :SUBNEGOTIATION :NAWS])
  (let [raw-msg (:telnet/raw (meta command))
        [_ _ _ cx cols rx rows] raw-msg]
    (when (and (int? cx) (int? rx) (int? cols) (int? rows))
      (let [rx (int rx)
            cx (int cx)
            rows (int rows)
            cols (int cols)]
        (emit (with-meta
                {:type :screen-size
                 :screen-size [(+ (* 256 rx) rows) (+ (* 256 cx) cols)]}
                {:telnet/command command
                 :telnet/raw raw-msg}))))))
