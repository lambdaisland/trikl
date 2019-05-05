(ns trikl.render
  "First attempt at providing a higher level API, abandoned."
  )

(def ^:dynamic *render-stack* [])

(declare render)

(defn render-children [children]
  (doall
   (map-indexed
    (fn [i c]
      (binding [*render-stack* (conj *render-stack* (or (:key (meta c)) i))]
        (render c)))
    children)))

(defmulti render-vector (fn [e]
                          (type (first e))))

(defmethod render-vector clojure.lang.Keyword [e]
  (let [[t props children] (trikl.core/split-el e)]
    (binding [*render-stack* (conj *render-stack* t)]
      {:tag t
       :key *render-stack*
       :props props
       :children (render-children children)})))

(defmethod render-vector clojure.lang.Fn [e]
  (let [[f props children] (trikl.core/split-el e)
        props+ (assoc props :children children)]
    (binding [*render-stack* (conj *render-stack* f)]
      (let [result (f props+)]
        (if (fn? result)
          {:tag f
           :component? true
           :key *render-stack*
           :props props
           :render result
           :children [(render (result props+))]}
          {:tag f
           :component? true
           :key *render-stack*
           :props props
           :render f
           :children [(render result)]})))))

(defmulti render type)

(defmethod render :default [e] e)

(defn render-map [e]
  (assert (fn? (:render e)))
  (merge {:tag (:render e)
          :props {}}
         e))

(defmethod render clojure.lang.PersistentHashMap [e]
  (render-map e))

(defmethod render clojure.lang.PersistentArrayMap [e]
  (render-map e))

(defmethod render clojure.lang.PersistentVector [e]
  (render-vector e))

(defn my-comp [props]
  [:foo 123])

(defn state-comp [props]
  (let [s (atom 0)]
    (fn [props]
      [:span "State: " @s])))

(defn element-index
  ([tree]
   (element-index {} tree))
  ([index tree]
   (let [index (if-let [k (:key tree)] (assoc index k tree) index)]
     (reduce element-index index (:children tree)))))

(def ^:dynamic *prev-tree*
  (render [:box {:x :y}
           [:span "hello"]
           [my-comp]
           [state-comp]
           {:render (fn []
                      [:box 123])}]))

(element-index *prev-tree*)

;; Component local state
;; - Find previous component, reuse render function
;; - Got render stack, needs element-index from previous render tree

;; Event dispatch
;; - Given key-vector is easy to find in element-index and even to bubble
;; - Setting focus based on id? need to track id, can also go in element index
