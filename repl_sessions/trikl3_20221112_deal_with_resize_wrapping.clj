(ns trikl3-20221112-deal-with-resize-wrapping
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.telnet :as telnet])
  (:import
   (java.io Closeable)))

(defmacro let-def [bindings & body]
  `(do
     ~@(for [binding (partition 2 bindings)]
         `(def ~@binding))
     ~@body))

(let-def [port 9999
          mode :inline
          ss    (telnet/server-socket port)
          conns (atom [])
          stop? (volatile! false)
          stop! (fn []
                  (run! conn/shutdown @conns)
                  (.close ^Closeable ss)
                  (vreset! stop? true))
          conn-loop
          (future
            (while (not @stop?)
              (let [cs   (telnet/accept-connection ss)
                    conn (telnet/telnet-connection (case mode
                                                     :inline
                                                     {:client-socket  cs
                                                      :init-sequence  "" #_conn/default-inline-init-sequence
                                                      :reset-sequence "" #_conn/default-inline-reset-sequence}
                                                     :fullscreen
                                                     {:client-socket cs}))]
                (swap! conns conj conn)
                )))])

;; (stop!)

(def conn (last @conns))
(def conn (last (butlast @conns)))

(conn/write
 conn
 (apply str (repeat 100 "x")))

"\u0008"
(conn/write
 conn
 (apply str (repeat 100 "x")))

(defn rep-char [n ch]
  (apply str (repeat n ch)))

(def modes
  {:wraparound 7
   :alternate-basic 47
   :alternate-with-reset 1049
   :cursor-origin 6
   :inhibit-scroll-on-output 1010})

(enable! :alternate-basic)
(enable! :inhibit-scroll-on-output)
(disable! :inhibit-scroll-on-output)

(defn enable! [mode]
  (conn/write conn (str "\u001B[?" (get modes mode) "h")))
(defn disable! [mode]
  (conn/write conn (str "\u001B[?" (get modes mode) "l")))
(conn/write conn (rep-char 50 "x"))
(conn/write conn (rep-char 50 "y"))
(conn/write conn "\n")
(conn/write conn (rep-char 50 "\u0008"))
(conn/write conn "\u000B")
(conn/write conn "\u007F")
(conn/write conn "\r")
(conn/write conn "\u001B[A")
(conn/write conn "\u001B[B")
(conn/write conn "\r\u001B[90Cyxxxxxxx")
(conn/write conn "\u001B[2K")
(conn/write conn "\u001B[J")
(conn/write conn "\u001B[F")
(conn/write conn "\u001B[?T")
(conn/write conn "\u001B[?1h")
(conn/write conn "\u001B[?6h")
(conn/write conn "\u001B[?1010h")
(conn/write conn "\u001B[?7h")

(conn/write conn "\u001B[?1049l")
(conn/write conn "\u001B7")
(conn/write conn "\u001B8")
(conn/write conn "\u001B[s")
(conn/write conn "\u001B[u")

(conn/write conn (apply str
                        (for [x (range 50)]
                          (str (apply str (repeat 20 (str x " ")))
                               "\n"))))


(conn/write conn "\u001B[1;2r")
(conn/write conn "\u001B[9999;0H")
