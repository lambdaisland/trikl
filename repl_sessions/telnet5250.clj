(ns telnet5250
  (:require [lambdaisland.trikl1.telnet :as tn]
            [clojure.java.io :as io])
  (:import [java.net Socket ServerSocket]))

(do
  (reset! done? true)
  (.close s)
  (.close client)
  (def s (Socket. "192.168.1.87" 23)))

(def telnet-socket (ServerSocket. 1234))

(long (first "\200"))

(do
  (def client (.accept telnet-socket))

  (def client-in (.getInputStream client))
  (def client-out (.getOutputStream client))

  (def done? (atom false))

  (defn copy-loop [prefix in out]
    (try
      (loop []
        (let [src (byte-array 4096)
              dest (byte-array 4096)
              limit (.read in src)
              _ (prn prefix (str "read " limit " bytes"))
              {:keys [src-pos dest-end commands]} (tn/filter-commands {:src src :dest dest :end limit})]
          (doseq [cmd commands]
            (println prefix cmd))
          (println prefix (.toString (tn/charset-decode chardec dest dest-end)))
          (.write out src 0 limit)
          (prn prefix (str "wrote" limit " bytes"))
          (when-not @done?
            (recur))))
      (catch Exception e
        (println prefix e))))

  (future
    (copy-loop ">" client-in (.getOutputStream s)))

  (future
    (copy-loop "<" (.getInputStream s) client-out)))

(reset! done? false)



(binding [*out* (io/writer (.getOutputStream s))]
  (println)
  #_(println "EXTBRASSEU")
  #_(println "abc123"))

(def buff (byte-array 4096))

(def limit (.read (.getInputStream s) buff))
;; => 806

(def dst (byte-array 4096))

(tn/filter-commands {:src buff :dest dst :end limit})
;; => {:src-pos 775, :dest-end 769, :commands [[:IAC :DO 39] [:IAC :DO 24]]}
;; => {:src-pos 806, :dest-end 800, :commands [[:IAC :DO 39] [:IAC :DO 24]]}

(def chardec (tn/charset-decoder "Cp273"))

(println (.toString (tn/charset-decode chardec dst 804)))

[:IAC :SUBNEGOTIATION 39 :ECHO :SUPPRESS_GO_AHEAD 73 66 77 82 83 69 69 68 5 157 191 32 205 129 174 211 :TRANSMIT_BINARY :SUPPRESS_GO_AHEAD :IAC :SUBNEGOTIATION_END]
[:IAC :SUBNEGOTIATION 24 :ECHO :IAC :SUBNEGOTIATION_END]

[:IAC :SUBNEGOTIATION 39 :TRANSMIT_BINARY :SUPPRESS_GO_AHEAD 75 66 68 84 89 80 69 :ECHO 65 71 66 :SUPPRESS_GO_AHEAD 67 79 68 69 80 65 71 69 :ECHO 50 55 51 :SUPPRESS_GO_AHEAD 67 72 65 82 83 69 84 :ECHO 54 57 55 :IAC :SUBNEGOTIATION_END]
[:IAC :SUBNEGOTIATION 24 :TRANSMIT_BINARY 73 66 77 45 51 49 55 57 45 50 :IAC :SUBNEGOTIATION_END]

[:IAC :SUBNEGOTIATION 39 :ECHO :SUPPRESS_GO_AHEAD 73 66 77 82 83 69 69 68 132 137 161 232 181 83 180 66 :TRANSMIT_BINARY :SUPPRESS_GO_AHEAD :IAC :SUBNEGOTIATION_END]
[:IAC :SUBNEGOTIATION 39 :TRANSMIT_BINARY :SUPPRESS_GO_AHEAD 75 66 68 84 89 80 69 :ECHO 65 71 66 :SUPPRESS_GO_AHEAD 67 79 68 69 80 65 71 69 :ECHO 50 55 51 :SUPPRESS_GO_AHEAD 67 72 65 82 83 69 84 :ECHO 54 57 55 :IAC :SUBNEGOTIATION_END]
