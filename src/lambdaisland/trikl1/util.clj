(ns lambdaisland.trikl1.util
  (:import
   (java.util.concurrent Delayed TimeUnit)))

(defn add-shutdown-hook [^clojure.lang.IFn f]
  (.addShutdownHook (java.lang.Runtime/getRuntime)
                    (Thread. f)))

(defn ^"[Ljava.lang.String;" string-array [args]
  (into-array String args))

(defn char-in-range? [min ch max]
  (<= (long min) (long ch) (long max)))

(defn reduce-idx
  "Like reduce, but passes an additional leading index argument to the reducing
  function."
  [f init coll]
  (let [idx (volatile! -1)]
    (reduce (fn [acc x]
              (f (vswap! idx inc) acc x))
            init
            coll)))

(defn delayed
  "Create a new java.util.concurrent.Delayed, which can be enqueued in a
  DelayQueue. Will be delayed by `ms` milliseconds. Deref to get the value
  back."
  [val ms]
  (let [target-ms (+ ms (System/currentTimeMillis))]
    (reify
      Delayed
      (getDelay [this unit]
        (.convert unit (- target-ms (System/currentTimeMillis))
                  TimeUnit/MILLISECONDS))
      (compareTo [this that]
        (- (.getDelay ^Delayed this TimeUnit/MILLISECONDS)
           (.getDelay ^Delayed that TimeUnit/MILLISECONDS)))
      clojure.lang.IDeref
      (deref [this]
        val))))

(defmacro thread
  {:style/indent [1]}
  [name & body]
  `(Thread. (fn [] ~@body) ~name))

(defn atom? [a]
  (instance? clojure.lang.IAtom a))
