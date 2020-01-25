(ns lambdaisland.trikl1.input-events-test
  (:require [clojure.spec-alpha2 :as s]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop :refer [for-all]]
            [lambdaisland.trikl1.input-events :as input-events :refer [ESC]])
  (:import java.nio.CharBuffer))

(defn event->chars [{:keys [type] :as event}]
  (case type
    :input (str (:char event))))

(defn chars->events [chars]
  (let [results (volatile! [])
        ctx {:char-buf (CharBuffer/wrap (apply str chars))}]
    (loop [ctx ctx]
      (prn ctx)
      (when (.hasRemaining (:char-buf ctx))
        (recur (input-events/ansi-process-chars ctx #(vswap! results conj %)))))
    @results))

(defspec ansi-process-chars-spec
  (for-all [events (s/gen (s/coll-of :lambdaisland.trikl1.input-events/input-event))]
    (= events (chars->events (map event->chars events)))))
