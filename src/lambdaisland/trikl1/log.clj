(ns lambdaisland.trikl1.log
  (:require [clojure.walk :as walk])
  (:import
   (org.apache.logging.log4j Logger LogManager)
   (org.apache.logging.log4j.message MapMessage)))

(set! *warn-on-reflection* true)

(def ^:dynamic *enable-logging*
  "If this is false, logging is completely elided. Can be set globally through a
  property, or per-file by wrapping `load` in a `binding`."
  (= "true"
     (System/getenv "lambdaisland.trikl.enable-logging")))

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
                         (if (< (long char) 32)
                           (get non-printable-characters (long char))
                           char))
                       s))
       "\""))

(defn map-message ^MapMessage [m]
  (let [^java.util.Map m
        (walk/postwalk
         (fn [o]
           (if (string? o)
             (format-string o)
             o))
         (update-vals m (fn [o]
                          (if-let [klass (:sos/klass (meta o))]
                            {:klass klass
                             :state @o}
                            o))))

        #_(->> m
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

(defmacro trace [& {:as msg}] (when *enable-logging* `(.trace (logger) (map-message ~msg))))
(defmacro debug [& {:as msg}] (when *enable-logging* `(.debug (logger) (map-message ~msg))))
(defmacro info [& {:as msg}] (when *enable-logging* `(.info (logger) (map-message ~msg))))
(defmacro warn [& {:as msg}] (when *enable-logging* `(.warn (logger) (map-message ~msg))))
(defmacro error [& {:as msg}] (when *enable-logging* `(.error (logger) (map-message ~msg))))
