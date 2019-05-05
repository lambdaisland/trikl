(ns trikl.main
  (:gen-class)
  (:require [trikl.core :as t]))

(defn -main [& _]
  (let [client (t/stdio-client)]
    (t/add-listener client ::viz-msg (fn [m]
                                       (t/render client (prn-str m))))))
