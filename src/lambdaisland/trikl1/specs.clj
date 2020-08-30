(ns lambdaisland.trikl1.specs
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as sgen]
            [clojure.test.check.generators :as gen]
            [lambdaisland.regal.spec-alpha :refer [spec] :rename {spec regal-spec}]))

(def NUL \u0000)
(def ESC \u001b)

(defmulti input-event-spec :type)

(defmethod input-event-spec :input [_]
  (s/keys :req-un [:lambdaisland.trikl1.input-events/char]))

(defmethod input-event-spec :ansi [_]
  (s/keys :req-un [:lambdaisland.trikl1.input-events/command]))

(s/def :lambdaisland.trikl1.input-events/char
  (regal-spec
   [:class [\space Character/MAX_VALUE]]))

(s/def ::param-byte (regal-spec [:class [\0 \?]]))
(s/def ::interm-byte (regal-spec [:class [\space \/]]))
(s/def ::final-byte (regal-spec [:class [\@ \~]]))

(s/def ::ansi-command
  (regal-spec
   [:cat
    ESC "["
    [:* ::param-byte]
    [:* ::interm-byte]
    ::final-byte]))

(sgen/sample (s/gen ::ansi-command))
;; =>
("[C" "[,w" "[9!v" "[+$d" "[9?96 'l" "[971<3w" "[:?$%(&,~" "[1:3=($.%l" "[',.)\"-D" "[;6<4649<1\",$'%/)f")

;; The ESC [ is followed by any number (including none) of "parameter bytes" in
;; the range 0x30–0x3F (ASCII 0–9:;<=>?), then by any number of "intermediate
;; bytes" in the range 0x20–0x2F (ASCII space and !"#$%&'()*+,-./), then finally
;; by a single "final byte" in the range 0x40–0x7E (ASCII @A–Z[\]^_`a–z{|}~).

(s/def :lambdaisland.trikl1.input-events/input-event
  (s/multi-spec input-event-spec :type))
