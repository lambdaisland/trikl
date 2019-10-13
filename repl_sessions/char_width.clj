(ns char-width
  (:require [trikl.client :as c])
  (:import java.util.concurrent.LinkedBlockingQueue
           java.util.LinkedList))

(def s (c/start-server))
#_(c/stop-server s)

(defn client []
  (last @(:clients s)))

(def queue (LinkedBlockingQueue.))
(def results (LinkedList.))

(c/add-listener (client) ::queue #(.put queue %))

(defn cursor-position []
  (.write (:out (client)) (.getBytes (str c/ESC "[6n")))
  (loop [event (.take queue)]
    (if (= :screen-size (:type event))
      (:screen-size event)
      (do
        (println event)
        (recur (.take queue))))))

(doseq [i (range #_(apply max (map first results))
                 255
                 (* 256 256))]
  (when (= (mod i 1000) 0)
    (println i))
  (.write (:out (client)) (.getBytes (str c/ESC "[1;1H" (char i))))
  (.add results [i (dec (second (cursor-position)))]))

(count results)

(apply max (map first results))

(frequencies (map second results))

(def compacted
  (reduce (fn [[coll [start end width]] [cp w]]
            (if (nil? start)
              [[] [cp cp w]]
              (if (and end (= cp (inc end)) (= w width))
                [coll [start cp width]]
                [(conj coll [start end width]) [cp cp w]])))
          []
          results))

(count (first compacted))

(map (fn [[a b c]]
       [a b c (- b a)])
     (first compacted))

(char 42125)

(spit "resources/char_widths.edn"
      (with-out-str (clojure.pprint/pprint (first compacted))))

(.write (:out (client)) (.getBytes (str c/ESC "[1;1H")))
(.write (:out (client)) (.getBytes "🎉"))
(.write (:out (client)) (.getBytes "福"))
(.write (:out (client)) (.getBytes "x"))
(.write (:out (client)) (.getBytes (str c/ESC "[6n")))

;; https://stackoverflow.com/questions/3634627/how-to-know-the-preferred-display-width-in-columns-of-unicode-characters#9145712
;; http://unicode.org/reports/tr14/
;; https://www.cl.cam.ac.uk/~mgk25/ucs/wcwidth.c
;; https://github.com/foliojs/linebreak
