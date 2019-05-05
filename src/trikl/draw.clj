(ns trikl.draw
  (:require [clojure.string :as str]
            [trikl.screen :as screen :refer [->Charel]])
  (:import [clojure.lang IPersistentMap PersistentVector]
           trikl.screen.Charel))

(set! *warn-on-reflection* true)

(defrecord DrawingContext [;; position
                           ^long x ^long y ;; [col row]

                           ;; size
                           ^long width ^long height

                           ;; bounding pox
                           ^long min-x ^long min-y
                           ^long max-x ^long max-y

                           ;; actual content (vector of vector of screen/Charel)
                           ;; careful: vector of rows, so lookup are [y x], not [x y]
                           ^PersistentVector charels

                           ;; current foreground/background colors in use
                           ^IPersistentMap styles])

(defn draw-line [ctx line]
  (let [^long max-x (:max-x ctx)
        ^long x     (:x ctx)
        ^long y     (:y ctx)
        styles      (:styles ctx)
        line        (remove #{\return} ;; don't want no carriage returns
                            (->> (- max-x x)
                                 (min (count line))
                                 (max 0)
                                 (subs line 0)))]
    (reduce (fn [ctx char]
              (-> ctx
                  (update :x inc)
                  (update-in [:charels y (:x ctx)]
                             (fn [^Charel ch]
                               (->Charel char
                                         (:fg styles (:fg ch))
                                         (:bg styles (:bg ch)))))))
            ctx
            line)))
#_
(defn draw-string [^DrawingContext ctx s]
  (let [^long max-y (:max-y ctx)]
    (let [lines (str/split s #"(?<=\n)")]
      (reduce (fn [{y :y :as ctx} line]
                (if (>= y max-y)
                  (reduced ctx)
                  (let [nl?  (= \newline (last line))
                        line (cond->> line nl? butlast nl? (apply str))
                        ctx  (draw-line ctx line)]
                    (if nl?
                      (assoc ctx
                             :x (:min-x ctx)
                             :y (inc y))
                      ctx))))
              ctx
              lines))))
