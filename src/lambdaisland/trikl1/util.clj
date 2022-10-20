(ns lambdaisland.trikl1.util)

(defn add-shutdown-hook [^clojure.lang.IFn f]
  (.addShutdownHook (java.lang.Runtime/getRuntime)
                    (Thread. f)))

(defn ^"[Ljava.lang.String;" string-array [args]
  (into-array String args))

(defn char-in-range? [min ch max]
  (<= (long min) (long ch) (long max)))

(defn reduce-idx [f init coll]
  (let [idx (volatile! 0)]
    (reduce (fn [acc x]
              (f (vswap! idx inc) acc x))
            init
            coll)))
