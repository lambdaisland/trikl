(ns lambdaisland.trikl1.specs
  (:require [clojure.spec-alpha2 :as s]))

(def NUL \u0000)
(def ESC \u001b)

(defmulti input-event-spec :type)

(defmethod input-event-spec :input [_]
  (s/keys :req-un [:lambdaisland.trikl1.input-events/char]))

(s/def :lambdaisland.trikl1.input-events/char
  (s/and char?
         #(> 31 (long %))))

(s/def :lambdaisland.trikl1.input-events/input-event
(s/multi-spec input-event-spec :type))
