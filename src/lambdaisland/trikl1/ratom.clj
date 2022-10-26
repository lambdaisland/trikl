(ns lambdaisland.trikl1.ratom
  "Reagent-esque implementations of reactive atoms and cursors."
  (:import (clojure.lang IAtom IAtom2 IDeref IMeta IRef IReference)))

(set! *warn-on-reflection* true)

(deftype Cursor [the-atom path ^:volatile-mutable -meta]
  IAtom
  (swap [this f]
    (get-in
     (.swap ^IAtom the-atom update-in path f)
     path))
  (swap [this f arg]
    (get-in
     (.swap ^IAtom the-atom update-in path f (list arg))
     path))
  (swap [this f arg1 arg2]
    (get-in
     (.swap ^IAtom the-atom update-in path f (list arg1 arg2))
     path))
  (swap [this f x y args]
    (get-in
     (.swap ^IAtom the-atom update-in path f (concat (list x y) args))
     path))
  (reset [this newval]
    (get-in
     (.swap ^IAtom the-atom assoc-in path newval)
     path))
  (compareAndSet [this oldv newv]
    (try
      (.swap ^IAtom the-atom
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

  IRef
  ;; (setValidator [this f]
  ;;   (.setValidator the-atom f))
  ;; (getValidator [this]
  ;;   (.getValidator the-atom))
  ;; (getWatches [this]
  ;;   (.getWatches the-atom))
  (addWatch [this key callback]
    (.addWatch ^IRef the-atom [this key] (fn [k _ old new]
                                           (callback k this
                                                     (get-in old path)
                                                     (get-in new path)))))
  (removeWatch [this key]
    (.removeWatch ^IRef the-atom [this key]))

  IMeta
  (meta [^Cursor this]
    (._meta this))

  IReference
  (alterMeta [this alter args]
    (set! (._meta this) (apply alter @this args)))
  (resetMeta [this m]
    (set! (._meta this) m))

  IDeref
  (deref [this] (get-in @the-atom path)))

(defn cursor [the-atom path]
  (->Cursor the-atom path nil))

(def ^:dynamic *tracing-context* nil)

(deftype RAtom [^clojure.lang.Atom the-atom]
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
    (.setValidator the-atom f))
  (getValidator [this]
    (.getValidator the-atom))
  (getWatches [this]
    (.getWatches the-atom))
  (addWatch [this key callback]
    (.addWatch the-atom key callback))
  (removeWatch [this key]
    (.removeWatch the-atom key))

  IMeta
  (meta [this]
    (.meta the-atom))

  IReference
  (alterMeta [this alter args]
    (.alterMeta the-atom alter args))
  (resetMeta [this m]
    (.resetMeta the-atom m))

  IDeref
  (deref [this]
    (when *tracing-context*
      (vswap! *tracing-context* conj this))
    @the-atom))

(defn ratom [value]
  (->RAtom (atom value)))

(definterface IReaction
  (stopReaction []))

(deftype Reaction [^clojure.lang.Atom the-atom dependencies]
  IRef
  (setValidator [this f]
    (.setValidator the-atom f))
  (getValidator [this]
    (.getValidator the-atom))
  (getWatches [this]
    (.getWatches the-atom))
  (addWatch [this key callback]
    (.addWatch the-atom key callback))
  (removeWatch [this key]
    (.removeWatch the-atom key))

  IMeta
  (meta [this]
    (.meta the-atom))

  IReference
  (alterMeta [this alter args]
    (.alterMeta the-atom alter args))
  (resetMeta [this m]
    (.resetMeta the-atom m))

  IDeref
  (deref [this]
    (when *tracing-context*
      (vswap! *tracing-context* conj this))
    @the-atom)

  IReaction
  (stopReaction [this]
    (doseq [dep dependencies]
      (remove-watch dep the-atom))))

(defn make-reaction [thunk]
  (binding [*tracing-context* (volatile! #{})]
    (let [the-atom (atom (thunk))
          dependencies @*tracing-context*]
      (doseq [dep dependencies]
        (add-watch dep the-atom (fn [_ _ _ _]
                                  (reset! the-atom (thunk)))))
      (->Reaction the-atom dependencies))))

(defmacro reaction [& body]
  `(make-reaction (fn* [] ~@body)))

(comment
  (def a (ratom {:x {:y {:z 1}}}))
  (def c (cursor a [:x :y]))
  (def r (reaction (get-in @a [:x :y :z])))
  (def r2 (reaction (inc @r)))
  @r
  @r2
  (swap! c update :z inc)
  @a

  (reset-meta! a {:a 1})
  (reset-meta! c {:c 2})
  (reset-meta! r {:r 3})
  (meta a)
  (meta c)
  (meta r))
