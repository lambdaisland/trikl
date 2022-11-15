(ns lambdaisland.trikl1.ratom
  "Reagent-esque implementations of reactive atoms and cursors."
  (:require [lambdaisland.trikl1.log :as log])
  (:import (clojure.lang IAtom IAtom2 IDeref IMeta IRef IReference ILookup)))

(set! *warn-on-reflection* true)
(def ^:dynamic *tracing-context* nil)

(deftype Cursor [the-atom path ^:volatile-mutable metadata]
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
  ;;; No good way to delegate the validator, since it would replace the validator
  ;;; on the atom. We could handle it ourselves but that gets tedious, so punting
  ;;; on it until it turns out there's a good use case.
  ;; (setValidator [this f]
  ;;   (.setValidator the-atom f))
  ;; (getValidator [this]
  ;;   (.getValidator the-atom))
  ;;; Seems this is only used internally in some STM code? YAGNI
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
    (.metadata this))

  IReference
  (alterMeta [this alter args]
    (set! (.metadata this) (apply alter @this args)))
  (resetMeta [this m]
    (set! (.metadata this) m))

  IDeref
  (deref [this] (get-in @the-atom path)))

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
    @the-atom)

  ;; cheeky extension, allow direct destructuring of `this` is sos objects. Do
  ;; make sure these are also tracked (so we deref `this` and not `the-atom`)
  ILookup
  (valAt [this k] (get @this k))
  (valAt [this k not-found] (get @this k not-found)))

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

(defn cursor
  "Creates a cursor into an atom, given a lookup path (as per [[get-in]]). Acts
  like an atom, including supporting updates, watches, and mutable metadata as
  per [[IReference]].

  Keyword opts:
  - :meta : supply initial metadata
  "
  ([the-atom path]
   (->Cursor the-atom path nil))
  ([the-atom path & {:keys [meta]}]
   (->Cursor the-atom path meta)))

(defn ratom
  "Reactive atom. Behaves like a stock [[atom]], but allows tracking of derefs for
  reactivity, see [[make-reaction]].

  Keyword opts:
  - :meta : supply initial metadata
  "
  ([value]
   (->RAtom (atom value)))
  ([value & {:keys [meta validator]}]
   (->RAtom (atom value
                  :meta meta
                  :validator validator))))

(defn make-reaction
  "Create a reaction based on a zero-arg function. The function will be called,
  and any dereferencing of [[ratom]] instances will be tracked. Subsequent
  updates to the dereferences ratoms will cause the reaction to recompute.

  Supports listeners and mutable metadata like regular atoms, but is read-only.
  The value can only be changed by updating the upstream ratoms.

  Keyword opts:
  - :meta : supply initial metadata
  "
  ([thunk & {:keys [meta]}]
   (binding [*tracing-context* (volatile! #{})]
     (let [the-atom (atom (thunk) :meta meta)
           dependencies @*tracing-context*]
       (doseq [dep dependencies]
         (add-watch dep the-atom (fn [_ _ _ _]
                                   (reset! the-atom (thunk)))))
       (->Reaction the-atom dependencies)))))

(defmacro reaction
  "Macro version of [[make-reaction]], saves a few characters by omitting
  the `(fn [])`"
  [& body]
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
