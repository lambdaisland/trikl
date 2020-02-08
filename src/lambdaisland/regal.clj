(ns lambdaisland.regal
  (:require [lambdaisland.regal.pattern :as pattern]
            [lambdaisland.regal.gen :as gen]))

(defn compile [r]
  (pattern/compile r))

(defn gen [r]
  (gen/gen r))

(defn sample [r]
  (gen/sample r))
