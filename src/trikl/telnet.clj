(ns trikl.telnet
  (:import java.net.ServerSocket
           javax.net.ServerSocketFactory))

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

   :TRANSMIT_BINARY 0x00
   :ECHO            0x01
   :NAWS            0x1f
   :LINEMODE        0x22})

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
                       ;;:IAC :DO :NAWS
                       ))
