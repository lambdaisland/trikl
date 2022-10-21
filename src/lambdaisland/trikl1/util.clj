(ns lambdaisland.trikl1.util
  (:import (clojure.lang IAtom IDeref)))

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

(defn cursor [the-atom path]
  (reify
    IAtom
    (swap [this f]
      (swap! the-atom update-in path f))
    (swap [this f arg]
      (swap! the-atom update-in path f arg))
    (swap [this f arg1 arg2]
      (swap! the-atom update-in path f arg1 arg2))
    (swap [this f x y args]
      (apply swap! the-atom update-in path f x y args))
    (reset [this newval]
      (get-in (swap! the-atom assoc-in path newval) path))
    (compareAndSet [this oldv newv]
      (if (= (get-in @the-atom path) oldv)
        (swap! the-atom assoc-in path newv)))

    IDeref
    (deref [this] (get-in @the-atom path))))
