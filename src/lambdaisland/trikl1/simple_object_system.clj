(ns lambdaisland.trikl1.simple-object-system
  "Clojure's missing object system

  An 'objekt' for us is an (r)atom which contains the object state, and metadata on
  that atom which contains the methods, keyed by symbol.

  A 'klass' is a map of methods, which can then be used as metadata on an objekt
  to 'instantiate' an objekt.

  [[call]] handles calling a method, passing it `self` (the atom) and any
  additional arguments.

  [[create]] constructs an instance of a klass. There are two methods that can
  be implemented for constructor functionality. `prep` is called before the atom
  is created, it receives any options-map passed to `create`, and the return
  value is used as the atom's initial value. Metadata on the return value is
  added to the metadata on the atom, and thus can be used to locally add or
  override instance methods.

  `init` is an actual contstructor, receiving `self` and again the options map
  supplied to `create`. It can further initialize by swapping `self`. Its
  return value is ignored.

  The `defklass` macro provides a syntax that looks more like e.g. a `deftype`.
  The initial `self` argument is implicit when using `defklass`.

  Validation with malli schemas, and superclass method chain traversal are
  supported. The keys in the metadata map are `:malli/schema`, and
  `:sos/superklass` respectively.

  I'm sorry, Rich, but I really wanted concrete instantiation and derivation in
  self case. The Smalltalk people were right, these object things make a lot of
  sense for GUIs.
  "
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [lambdaisland.trikl1.ratom :as ratom]
   [malli.core :as m])
  (:import (clojure.lang IAtom IAtom2 IDeref IMeta
                         IRef IReference ILookup
                         IFn)))


(defn get-klass [o]
  (cond
    (var? o)
    (get-klass @o)
    (:sos/klass-var o)
    (merge (get-klass (dissoc @(:sos/klass-var o) :sos/klass-var))
           o)
    (:sos/klass o)
    o
    (nil? o)
    nil
    :else
    (get-klass (meta o))))

(defn- call-with-klass [klassname klass obj method args]
  (println (str klassname "#" method "/" (inc (count args))))
  (let [f (get klass method)]
    (cond
      f
      (try
        (apply f obj args)
        (catch clojure.lang.ArityException e
          (if (and f (= (.-name e) (.getName (.getClass f))))
            (throw (clojure.lang.ArityException. (.-actual e)
                                                 (str klassname "#" method)))
            (throw e)))
        (catch Throwable e
          (println (str klassname "#" method) e)
          #_(.setStackTrace
             e
             (into-array StackTraceElement
                         (cons (StackTraceElement.
                                (str klassname)
                                (str method)
                                (str (:ns (meta f)))
                                (:line (meta f) -1))
                               (.getStackTrace e))))
          (throw e)))
      (:sos/superklass klass)
      (call-with-klass klassname (get-klass (:sos/superklass klass)) obj method args)
      :else
      (throw (java.lang.UnsupportedOperationException.
              (str "Method " method " not found on " klassname "<" @obj ">"))))))

(defn call
  "Objekt method call"
  [obj method & args]
  (let [klass (get-klass obj)]
    (call-with-klass (:sos/klass klass) klass obj method args)))

(defmacro create-obj-type []
  (let [ratom-def (walk/postwalk-replace
                   {'*tracing-context* `ratom/*tracing-context*}
                   (some #(when (= (fnext %) 'RAtom)
                            %)
                         (read-string
                          (str "["
                               (slurp (io/resource (str (str/replace (namespace `ratom/_)
                                                                     #"\."
                                                                     "/")
                                                        ".clj")))
                               "]"))))]
    `(deftype ~'Objekt
         ~@(drop 2 ratom-def)
       IFn
       ~@(for [i (range 21)
               :let [argv `[~'this ~@(for [j (range i)]
                                       (symbol (str "arg" (inc j))))]]]
           `(~'invoke ~argv
             (call ~@argv)))
       (~'invoke [~'this ~@(map #(symbol (str "arg" (inc %))) (range 20))
                  ~'args]
        (apply call ~'this ~@(map #(symbol (str "arg" (inc %))) (range 20))
               ~'args)))))

(create-obj-type)

(defn new-objekt
  "Low level constructor of an instance, like an RAtom, but implements IFn"
  ([value]
   (->Objekt (atom value)))
  ([value & {:keys [meta validator]}]
   (->Objekt (atom value
                   :meta meta
                   :validator validator))))

(defn- validate-schema-fn [schema]
  (let [schema (if (map? schema)
                 (into [:map] schema)
                 schema)]
    (fn [val]
      (when-not (m/validate schema val)
        (throw (ex-info "Invalid objekt state"
                        (m/explain schema val))))
      true)))

(defn has-method? [obj-or-klass method]
  (let [k (get-klass obj-or-klass)]
    (or (contains? k method)
        (when-let [super (:sos/superklass k)]
          (has-method? super method)))))

(defn create
  "Instantiate a new objekt"
  ([klass]
   (create klass nil))
  ([klass opts]
   (let [[klass klass-var] (if (var? klass) [@klass klass] [klass nil])
         state (if (has-method? klass 'prep)
                 (call-with-klass (:sos/klass klass) klass opts 'prep nil)
                 opts)
         klass (merge klass (meta state))]
     (cond->
         (new-objekt
          state
          (cond-> {:meta (if klass-var
                           {:sos/klass (:sos/klass klass)
                            :sos/klass-var klass-var}
                           klass)}
            (:malli/schema klass)
            (assoc :validator
                   (validate-schema-fn (:malli/schema klass)))))
       (has-method? klass 'init)
       (doto (call 'init opts))))))

(defn- method-impl [klassname [sym argv & body :as form]]
  [`'~sym
   `(with-meta (fn ~(symbol (str klassname "#" sym)) ~argv
                 ~@(walk/postwalk
                    (fn [f]
                      (if (and (list? f)
                               (qualified-symbol? (first f))
                               (= "self" (namespace (first f))))
                        `(call ~(first argv)
                               '~(symbol (name (first f)))
                               ~@(rest f))
                        f))
                    body))
      '~(assoc (meta form)
               :ns (symbol (str *ns*))))])

(defmacro defklass
  "Just a boat full of sugar"
  {:style/indent [2 :defn]}
  [name supers & methods]
  (let [[schema methods] (if (= (first methods) :-)
                           [(second methods) (drop 2 methods)]
                           [nil methods])
        klass-var-name (symbol (str (ns-name *ns*)) (str name))]
    `(def ~name
       ~(into (cond-> {:sos/klass `'~klass-var-name
                       :sos/klass-var `(var ~klass-var-name)}
                schema
                (assoc :malli/schema schema)
                (seq supers)
                (assoc :sos/superklass `(var ~(first supers))))
              (map (partial method-impl name))
              methods))))

(defmacro specify [obj & impls]
  `(let [o# ~obj]
     (new-objekt @o# :meta (assoc (meta o#)
                                  ~@(mapcat (partial method-impl "specify")
                                            impls)))))

(defmacro specify! [obj & impls]
  `(alter-meta! ~obj #(assoc % ~@(mapcat (partial method-impl "specify!") impls))))

(defn with
  "Derive a new objekt from an existing objekt by merging `m` into the objekt
  state."
  [obj m]
  (create (meta obj) (merge @obj m)))

(defn setk [obj k v]
  (swap! obj assoc k v))

(defn klass? [klass obj]
  (= (:sos/klass klass)
     (:sos/klass (meta obj))))

(comment
  (def MyObj
    {:malli/schema [:map [:x int?] [:y int?] [:z int?]]
     :to-string (fn [{:keys [x y z]}]
                  (str "x:" x " y:" y " z:" z))})

  (defklass BaseObj []
    (do-thing [{:keys [x y z]}]
      (println "doing thing" x y z)
      (throw (ex-info "uh oh" {}))))

  (defklass MyObj [BaseObj]
    :- [:map [:x int?] [:y int?] [:z int?]]
    (prep [opts]
      (merge {:x 1 :y 1 :z 1} opts))
    (init [self opts]
      (swap! self update :z + 3))
    (inc-x [self]
      (swap! self inc :x))
    (self-call [self]
      (self 'do-thing)))
  (get-klass BaseObj)
  (def obj (create MyObj {:x 2}))
  (meta (get (meta obj) 'prep))

  (meta (get (get MyObj :sos/superklass) 'do-thing))
  (meta (get BaseObj 'do-thing))
  (obj 'self-call)

  (call obj 'self-call)
  (specify! obj
    (do-thing [_]
      (println "hhaha")))

  (call (create MyObj {:x 2}) 'self-call)

  (call (specify (create MyObj {:x 2})
          (do-thing [self]
            (prn "new version" @self)))
        'self-call)

  (has-method? (create MyObj {:x 2}) 'do-thing)

  (has-method? (:sos/superklass (meta (create MyObj {:x 2}))) 'do-things)
  (def obj (create MyObj { :y 2 :z 3}))

  (call obj :to-string))
