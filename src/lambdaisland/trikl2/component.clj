(ns lambdaisland.trikl2.component
  (:require
   [clojure.string :as str]
   [lambdaisland.trikl1.ratom :as ratom]
   [lambdaisland.trikl1.simple-object-system :as obj]
   [lambdaisland.trikl1.util :as util]))

(obj/defklass Visitable []
  (visit [{:keys [children] :as self} visitor]
    (let [res (visitor self)]
      (when-not (= :prune-children res)
        (reduce
         (fn [_ child]
           (child 'visit visitor))
         nil
         children)))))

(obj/defklass Component [Visitable]
  (preferred-size [self] [0 0])
  (minimum-size [self] (self 'preferred-size))
  (maximum-size [self] (self 'preferred-size))
  (-draw [{:keys [window]} surface])
  (draw [self surface]
    (let [trace (volatile! #{})
          result (binding [ratom/*tracing-context* trace]
                   (self '-draw surface))]
      (swap! self assoc
             :trace (disj @trace self surface)
             :dirty? false
             :x (:x surface)
             :y (:y surface)
             :width (:width surface)
             :height (:height surface))
      result)))

(obj/defklass TextLine [Component]
  :- {:text string? :fg any? :bg any?}
  (preferred-size [{:keys [text]}]
    [1 (.length text)])

  (-draw [{:keys [text fg bg]} surface]
    (surface 'write-line 0 0 text fg bg)))

(obj/defklass Text [Component]
  (prep [{:keys [fg bg children]}]
    (let [s (str/join children)
          lines (str/split s #"\R")]
      {:fg fg
       :bg bg
       :lines lines}))
  (preferred-size [{:keys [lines]}]
    [(count lines)
     (apply max (map #(.length %) lines))])
  (-draw [{:keys [fg bg lines]} surface]
    (util/reduce-idx
     (fn [idx _ line]
       (surface 'write-line 0 idx line fg bg))
     nil
     lines)))

(obj/defklass ThunkComponent [Component]
  (init [self _]
    (self 'render))
  (preferred-size [{:keys [component]}]
    (component 'preferred-size))
  (render [{:keys [thunk] :as self}]
    (swap! self
           assoc :children
           [(thunk)]))
  (-draw [self surface]
    (self 'render)
    (binding [ratom/*tracing-context* nil]
      #_(prn "children:" (:children self))
      (when (seq (:children self))
        ((first (:children self)) 'draw surface)))))

(defn hiccup->component [form]
  (if-not (vector? form)
    form
    (let [[component & args] form
          [props & children] (if (map? (first args))
                               args
                               (cons nil args))]

      (cond
        (obj/derives-from? component Component)
        (obj/create component
                    (assoc props
                           :children
                           (doall (map hiccup->component children))))
        (fn? component)
        (obj/create ThunkComponent
                    {:thunk (fn []
                              (hiccup->component (apply component args)))})))))



(obj/defklass Stack [Component]
  )

(def state (ratom/ratom {:count 0}))

(obj/defklass StatefulComponent [Component]
  (-draw [_ surface]
    (surface 'write-line 0 0 (str "Count: " (:count @state))
             [0 200 0]
             [255 255 255])))
