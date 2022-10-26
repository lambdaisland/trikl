(ns lambdaisland.trikl1.util
  (:import (clojure.lang IAtom IAtom2 IDeref IRef)))

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
      (get-in
       (swap! the-atom update-in path f)
       path))
    (swap [this f arg]
      (get-in
       (swap! the-atom update-in path f arg)
       path))
    (swap [this f arg1 arg2]
      (get-in
       (swap! the-atom update-in path f arg1 arg2)
       path))
    (swap [this f x y args]
      (get-in
       (apply swap! the-atom update-in path f x y args)
       path))
    (reset [this newval]
      (get-in (swap! the-atom assoc-in path newval) path))
    (compareAndSet [this oldv newv]
      (try
        (swap! the-atom
               (fn [v]
                 (if (= oldv (get-in v path))
                   (assoc-in v path newv)
                   (throw (Exception.)))))
        true
        (catch Exception _
          false)))

    IAtom2
    (swapVals [this f]
      (mapv #(get-in % path)
            (.swapVals ^IAtom2 the-atom update-in path f)))
    (swapVals [this f arg]
      (mapv #(get-in % path)
            (.swapVals ^IAtom2 the-atom update-in path f (list arg))))
    (swapVals [this f arg1 arg2]
      (mapv #(get-in % path)
            (.swapVals ^IAtom2 the-atom update-in path f (list arg1 arg2))))
    (swapVals [this f x y args]
      (mapv #(get-in % path)
            (.swapVals ^IAtom2 the-atom update-in path f (concat [x y] args))))
    (resetVals [this newv]
      (mapv #(get-in % path)
            (.swapVals ^IAtom2 the-atom assoc-in path newv)))

    IDeref
    (deref [this] (get-in @the-atom path))))

(def ^:dynamic *tracing-context* nil)

(defn ratom [value]
  (let [the-atom (atom value)]
    (reify
      IAtom
      (swap [this f]
        (.swap ^IAtom the-atom f))
      (swap [this f arg]
        (.swap ^IAtom the-atom f arg))
      (swap [this f arg1 arg2]
        (.swap ^IAtom the-atom f arg1 arg2))
      (swap [this f x y args]
        (.swap ^IAtom the-atom f x y args))
      (reset [this newval]
        (.reset ^IAtom the-atom newval))
      (compareAndSet [this oldv newv]
        (.compareAndSet ^IAtom the-atom oldv newv))

      IAtom2
      (swapVals [this f]
        (.swapVals ^IAtom2 the-atom f))
      (swapVals [this f arg]
        (.swapVals ^IAtom2 the-atom f arg))
      (swapVals [this f arg1 arg2]
        (.swapVals ^IAtom2 the-atom f arg1 arg2))
      (swapVals [this f x y args]
        (.swapVals ^IAtom2 the-atom f x y args))
      (resetVals [this newv]
        (.resetVals ^IAtom2 the-atom newv))

      IRef
      (setValidator [this f]
        (.setValidator the-atom this f))
      (getValidator [this]
        (.getValidator the-atom this))
      (getWatches [this]
        (.getWatches the-atom this))
      (addWatch [this key callback]
        (.addWatch the-atom key callback))
      (removeWatch [this key]
        (.removeWatch the-atom key))

      IDeref
      (deref [this]
        (when *tracing-context*
          (vswap! *tracing-context* conj this))
        @the-atom))))


(defn reaction [f]
  (binding [*tracing-context* (volatile! #{})]
    (let [the-atom (ratom (f))
          dependencies @*tracing-context*]
      (doseq [a dependencies]
        (add-watch a the-atom (fn [_ _ _ _]
                                (reset! the-atom (f)))))
      the-atom)))

(atom {})
(def a (ratom {:x 1}))
(def b (ratom {:y 1}))
a
(add-watch a :x prn)

(def c (cursor a [:x]))
(swap! c inc)
(swap! a update :x inc)

a
@r
(def r
  (reaction
   (fn []
     (+ (:x @a) (:y @b)))))
