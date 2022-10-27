(ns lambdaisland.trikl1.simple-object-system
  "Clojure's missing object system

  An 'object' for us is an (r)atom which contains the object state, and metadata on
  that atom which contains the methods, keyed by symbol.

  A 'klass' is a map of methods, which can then be used as metadata on an object
  to 'instantiate' an object.

  [[call]] handles calling a method, passing it `this` (the atom) and any
  additional arguments.

  [[instance]] constructs an instance of a klass. There are two methods that can
  be implemented for constructor functionality. `prep` is called before the atom
  is created, it receives any options-map passed to `instance`, and the return
  value is used as the atom's initial value. Metadata on the return value is
  added to the metadata on the atom, and thus can be used to locally add or
  override instance methods.

  `init` is an actual contstructor, receiving `this` and again the options map
  supplied to `instance`. It can further initialize by swapping `this`. Its
  return value is ignored.

  The `defklass` macro provides a syntax that looks more like e.g. a `deftype`.
  The initial `this` argument is implicit when using `defklass`.

  Validation with malli schemas, and superclass method chain traversal are
  supported. The keys in the metadata map are `:malli/schema`, and
  `:sos/superklass` respectively.

  I'm sorry, Rich, but I really wanted concrete instantiation and derivation in
  this case. The Smalltalk people were right, these object things make a lot of
  sense for GUIs.
  "
  (:require [lambdaisland.trikl1.ratom :as ratom]
            [malli.core :as m]))

(defn- call-with-klass [klassname klass obj method args]
  (println (str klassname "#" method "/" (count args)))
  (try
    (let [f (get klass method)]
      (cond
        f
        (apply f obj args)
        (:sos/superklass klass)
        (call-with-klass klassname (:sos/superklass klass) obj method args)
        :else
        (throw (java.lang.UnsupportedOperationException.
                (str "Method " method " not found on " klassname "<" @obj ">")))))
    (catch clojure.lang.ArityException e
      (throw (clojure.lang.ArityException. (.-actual e)
                                           (str klassname "#" method)
                                           #_e)))
    (catch Exception e
      (println (str klassname "#" method) e)
      (throw e))))

(defn call
  "Object method call"
  [obj method & args]
  (let [klass (meta obj)]
    (call-with-klass (:sos/klass klass) klass obj method args)))

(defn- validate-schema-fn [schema]
  (let [schema (if (map? schema)
                 (into [:map] schema)
                 schema)]
    (fn [val]
      (when-not (m/validate schema val)
        (throw (ex-info "Invalid object state"
                        (m/explain schema val))))
      true)))

(defn has-method? [obj-or-klass method]
  (if (instance? clojure.lang.IAtom obj-or-klass)
    (has-method? (meta obj-or-klass) method)
    (or (contains? obj-or-klass method)
        (when-let [super (:sos/superklass obj-or-klass)]
          (has-method? super method)))))

(defn instance
  "Instantiate a new object"
  [klass opts]
  (let [state (if (has-method? klass 'prep)
                (call-with-klass (:sos/klass klass) klass opts 'prep nil)
                opts)
        klass (merge klass (meta state))]
    (cond->
        (ratom/ratom
         state
         (cond-> {:meta klass}
           (:malli/schema klass)
           (assoc :validator
                  (validate-schema-fn (:malli/schema klass)))))
      (has-method? klass 'init)
      (doto (call 'init opts)))))

(defmacro defklass
  "Just a boat full of sugar"
  {:style/indent [2 :defn]}
  [name supers & body]
  (let [[schema body] (if (= (first body) :-)
                        [(second body) (drop 2 body)]
                        [nil body])]
    `(def ~name
       ~(into (cond-> {:sos/klass `'~name}
                schema
                (assoc :malli/schema schema)
                (seq supers)
                (assoc :sos/superklass (first supers)))
              (map (fn [[sym argv & body]]
                     [`'~sym `(fn ~argv ~@body)]))
              body))))

(defn with
  "Derive a new object from an existing object by merging `m` into the object
  state."
  [obj m]
  (instance (meta obj) (merge @obj m)))

(defn setk [obj k v]
  (swap! obj assoc k v))

(comment
  (def MyObj
    {:malli/schema [:map [:x int?] [:y int?] [:z int?]]
     :to-string (fn [{:keys [x y z]}]
                  (let [ @this]
                    (str "x:" x " y:" y " z:" z)))})

  (defklass BaseObj []
    (do-thing [{:keys [x y z]}]
      (println "doing thing" x y z)))

  (defklass MyObj [BaseObj]
    :- [:map [:x int?] [:y int?] [:z int?]]
    (prep [opts]
      (merge {:x 1 :y 1 :z 1} opts))
    (init [$ opts]
      (swap! $ update :z + 3))
    (inc-x [$]
      (swap! $ inc :x)))

  yObj
  (call (instance MyObj {:x 2}) 'do-thing)

  (has-method? (instance MyObj {:x 2}) 'do-thing)

  (has-method? (:sos/superklass (meta (instance MyObj {:x 2}))) 'do-things)
  (def obj (instance MyObj { :y 2 :z 3}))

  (call obj :to-string))
