(ns lambdaisland.trikl1.util)

(defn add-shutdown-hook [^clojure.lang.IFn f]
  (.addShutdownHook (java.lang.Runtime/getRuntime)
                    (Thread. f)))

(defn ^"[Ljava.lang.String;" string-array [args]
  (into-array String args))

(defn char-in-range? [min ch max]
  (<= (long min) (long ch) (long max)))
