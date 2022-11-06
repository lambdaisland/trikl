(ns lambdaisland.trikl1.log
  (:require [clojure.walk :as walk])
  (:import
   (org.apache.logging.log4j Logger LogManager)
   (org.apache.logging.log4j.message MapMessage)))

(set! *warn-on-reflection* true)

(defmacro logger
  (^Logger []
   `(logger ~(str *ns*)))
  (^Logger [name]
   `(LogManager/getLogger ^String ~name)))

(def non-printable-characters
  ["\\x00"
   "\\x01"
   "\\x02"
   "\\x03"
   "\\x04"
   "\\x05"
   "\\x06"
   "\\a"
   "\\b"
   "\\t"
   "\\n"
   "\\v"
   "\\f"
   "\\r"
   "\\x0E"
   "\\x0F"
   "\\x10"
   "\\x11"
   "\\x12"
   "\\x13"
   "\\x14"
   "\\x15"
   "\\x16"
   "\\x17"
   "\\x18"
   "\\x19"
   "\\x1A"
   "\\e"
   "\\x1C"
   "\\x1D"
   "\\x1E"
   "\\x1F"])

(defn format-string [s]
  (str "\""
       (apply str (map (fn [char]
                         (if (< (long char) 33)
                           (get non-printable-characters (long char))
                           char))
                       s))
       "\""))

(defn map-message ^MapMessage [m]
  (let [^java.util.Map m
        (->> m
             (walk/prewalk
              (fn [o]
                (if-let [klass (:sos/klass (meta o))]
                  {:klass klass
                   :state @o}
                  o)))
             (walk/postwalk
              (fn [o]
                (if (string? o)
                  (format-string o)
                  o))))]
    (MapMessage. m)))

(defmacro trace [& {:as msg}] `(.trace (logger) (map-message ~msg)))
(defmacro debug [& {:as msg}] `(.debug (logger) (map-message ~msg)))
(defmacro info [& {:as msg}] `(.info (logger) (map-message ~msg)))
(defmacro warn [& {:as msg}] `(.warn (logger) (map-message ~msg)))
(defmacro error [& {:as msg}] `(.error (logger) (map-message ~msg)))
