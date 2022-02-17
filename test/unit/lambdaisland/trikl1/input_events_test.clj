(ns lambdaisland.trikl1.input-events-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.test :refer :all]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop :refer [for-all]]
            [lambdaisland.trikl1.input-events :as input-events :refer [ESC]])
  (:import java.nio.CharBuffer))

(require 'lambdaisland.trikl1.specs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

(defn event->chars [{:keys [type] :as event}]
  (case type
    :input (str (:char event))))

(defn chars->events [chars]
  (let [results (volatile! [])
        ctx {:char-buf (CharBuffer/wrap (apply str chars))}]
    (loop [ctx ctx]
      #_(prn ctx)
      (when (.hasRemaining (:char-buf ctx))
        (recur (input-events/ansi-process-chars ctx #(vswap! results conj %)))))
    @results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(deftest parse-screen-size-test
  (is (= [10 20]
         (input-events/parse-screen-size "\u001b[10;20R"))))

#_(defspec ansi-process-chars-spec
    (for-all [events (s/gen (s/coll-of :lambdaisland.trikl1.input-events/input-event))]
             (= events (chars->events (map event->chars events)))))


(comment

  (input-events/semantic-message )

  (map (juxt identity input-events/semantic-message) (sgen/sample (s/gen :lambdaisland.trikl1.input-events/input-event)))

  (require 'kaocha.repl)

  (kaocha.repl/run)
  )
