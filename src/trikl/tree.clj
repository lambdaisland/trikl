(ns trikl.tree
  "Trikl's UI tree, with partial updates and local state."
  (:require [clojure.string :as str]
            [trikl.screen :as screen]
            [trikl.util :as util]))

;; - state
;; - focus and event handling
;; - event bubbling
;; - diffing and selective redraw

;; - element vs component
;;   - render
;;   - draw
;;   - mount
;;   - receive props

;; - layout
;;   - flow
;;   - stack
;;   - fixed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Terminology

;; markup::
;; Hiccup style UI description

;; tag::
;; first element in a markup vector

;; ui-tree::
;; Persistent data structure that is the result of rendering markup

;; node::
;; Node in the UI tree, i.e. map with :tag, :children, :props, :widget

;; Element::
;; Node where the :tag is a keyword

;; Component
;; Node where the :tag is a function

;; Widget::
;; Encapsulates the behavior of a component, map with at a minimum a :render function

(def ^:dynamic *atom-factory* nil)

(defn new-context [height width]
  {;; (requested) location/size of the current node
   ;; location is absolute
   :x 0
   :y 0
   :height height
   :width width

   ;; area currently available for drawing
   :min-x 0
   :min-y 0
   :max-x (dec width)
   :max-y (dec height)

   :styles {}})

(defn markup-type [markup]
  (if (vector? markup)
    (let [tag (first markup)]
      (if (keyword? tag)
        tag
        (type tag)))
    (type markup)))

(defn markup-props [markup]
  (when (vector? markup)
    (let [[_ props & children] markup]
      (if (map? props)
        (assoc props :children children)
        {:children (cons props children)}))))

(defn clip [mini v maxi]
  (cond
    (< v mini) mini
    (< maxi v) maxi
    :else v))

(def ^:dynamic *component-state* nil)
(def ^:dynamic *node-path* [])

(defn string-node [text]
  {:tag    :text
   :path   *node-path*
   :markup text
   :text   (str/split text #"(\r\n|\r|\n)")})

(defn cstate []
  *component-state*)

(declare init)

(defn component-node [tag props]
  (let [widget (tag props)]
    (if (map? widget)
      (let [state (if-let [init-fn (:init-state widget)]
                    (*atom-factory* (init-fn props))
                    nil)
            child (binding [*component-state* state]
                    ((:render widget) props))]
        {:tag        tag
         :props      props
         :id         (:id widget)
         :path       *node-path*
         :widget     widget
         :prev-state @state
         :state      state
         :children   [(binding [*node-path* (conj *node-path* :children 0)]
                        (init child))]})
      {:tag      tag
       :props    props
       :path     *node-path*
       :widget   {:render tag}
       :children [(binding [*node-path* (conj *node-path* :children 0)]
                    (init (tag props)))]})))

(defn make-node [markup]
  (let [tag   (first markup)
        props (markup-props markup)]
    (cond
      (keyword? tag)
      {:tag      tag
       :props    props
       :path     *node-path*
       :children (into []
                       (comp
                        (map-indexed
                         (fn [idx node]
                           (binding [*node-path* (conj *node-path* :children idx)]
                             (if (seq? node)
                               (map init node)
                               [(init node)]))))
                        cat)
                       (:children props))}

      (fn? tag)
      (component-node tag props)

      (var? tag)
      (component-node tag props)

      :else
      (throw (ex-info "Markup not understood" {:markup markup})))))

(defn init
  "Construct an initial UI tree based on the provided markup."
  [markup]
  ;; string
  (cond
    (string? markup)
    (string-node markup)

    (vector? markup)
    (make-node markup)

    :else
    (throw (ex-info "Markup not understood" {:markup markup}))))

(declare receive-markup)

(defn receive-child-markup [nodes markups]
  (loop [[m & ms] markups
         [c & cs] nodes
         children []]
    (if m
      (if c
        (recur ms cs (conj children (receive-markup c m)))
        (recur ms cs (conj children (init m))))
      children)))

(defn receive-props [node props]
  (let [render (get-in node [:widget :render])]
    (if render ;; component?
      (let [inner-markup (binding [*component-state* (:state node)]
                           (render props))]
        (-> node
            (assoc :props props)
            (update :children
                    (fn [[c]]
                      [(receive-markup c inner-markup)]))))
      (-> node
          (assoc :props props)
          (update :children receive-child-markup (:children props))))))

(defn receive-markup [node markup]
  (binding [*node-path* (:path node)]
    (if (string? markup)
      (if (= markup (:markup node))
        node
        (string-node markup))
      (let [tag   (first markup)
            props (markup-props markup)
            state (when (contains? node :state)
                    @(:state node))
            state-changed? (and state (not= state (:prev-state node)))]
        (if (= tag (:tag node))
          (if (and (= props (:props node)) (not state-changed?))
            node
            (assoc (receive-props node props)
                   :prev-state state))
          (make-node markup))))))

(def layout nil)
(def draw nil)

(defn clip-bounding-box [{:keys [min-x max-x min-y max-y] :as ctx}
                         {:keys [x y width height overflow] :as props}]
  (prn ctx)
  (let [ctx (cond-> ctx
              x (assoc :x (clip min-x (+ (:x ctx) x) max-x))
              y (assoc :y (clip min-y (+ (:y ctx) y) max-y)))
        width (if width
                (clip 0 width (- max-x (:x ctx)))
                (:width ctx))
        height (if height
                 (clip 0 height (- max-y (:y ctx)))
                 (:height ctx))]
    (cond-> (assoc ctx
                   :min-x (:x ctx)
                   :min-y (:y ctx)
                   :width width
                   :height height)
      (= :hidden overflow) (assoc :max-x (+ (:x ctx) width)
                                  :max-y (+ (:y ctx) height)))))

(defn style-from-props [ctx props]
  (if-let [styles (:styles props)]
    (update ctx :styles merge styles)
    ctx))

(defn apply-props [ctx props]
  (-> ctx
      (clip-bounding-box props)
      (style-from-props props)))

(defmulti -layout (fn [node ctx] (:tag node)))

(defn layout [node ctx]
  (-layout node (apply-props ctx (:props node))))

(defmulti -draw (fn [node charels] (:tag node)))

(defn draw [node charels]
  (-draw node charels))

(defn fit-to-children [{:keys [children props] :as node} ctx]
  (if (seq children)
    (let [min-x (apply min (map :x children))
          min-y (apply min (map :y children))
          max-x (apply max (map #(+ (:x %) (:width %))
                                children))
          max-y (apply max (map #(+ (:y %) (:height %))
                                children))

          child-width  (- max-x min-x)
          child-height (- max-y min-y)
          overflow     (:overflow props :visible)]

      (assoc node
             :x (min (:x ctx) min-x)
             :y (min (:y ctx) min-y)
             :width (if (:width props)
                      (case overflow
                        :visible (max (:width ctx) child-width)
                        :hidden (:width ctx))
                      child-width)
             :height (if (:height props)
                       (case overflow
                         :visible (max (:height ctx) child-height)
                         :hidden (:height ctx))
                       child-height)))
    node))

(defmethod -layout :default [node ctx]
  (if (seq (:children node))
    (-> node
        (update :children
                (partial mapv (fn [node]
                                (layout node ctx))))
        (fit-to-children ctx))
    (assoc node
           :x (:x ctx) :y (:y ctx)
           :width 0 :height 0)))

(defn stack-layout [children ctx]
  (second
   (reduce (fn [[ctx children] node]
             (let [node (layout node ctx)]
               [(assoc ctx
                       :x (:min-x ctx)
                       :y (+ (:y node)
                             (:height node)))
                (conj children node)]))
           [ctx []]
           children)))

(defmethod -layout :stack [node ctx]
  (if (contains? node :children)
    (-> node
        (update :children stack-layout ctx)
        (fit-to-children ctx))
    (assoc node
           :x (:x ctx)
           :y (:y ctx)
           :width 0
           :height 0)))

(defmethod -layout :fill [node ctx]
  (merge node (select-keys ctx [:x :y :width :height]) ))

(defmethod -layout :text [node ctx]
  (let [lines (:text node)
        height (count lines)
        width (apply max (map count lines))
        {:keys [x y min-x max-x min-y max-y]} ctx]
    (assoc node
           :x x
           :y y
           :styles (:styles ctx)
           :width (clip 0 width (- max-x min-x))
           :height (clip 0 height (- max-y min-y)))))

(defn draw-nodes [charels nodes]
  (reduce #(draw %2 %1) charels nodes))

(defmethod -draw :default [node charels]
  (prn (select-keys node [:x :y :width :height :props]))
  (cond-> charels
    (:bg (:props node))
    (screen/update-bounding-box node assoc :bg (:bg (:props node)))

    :then
    (draw-nodes (:children node))))

(defn- draw-line [charels line x y-idx styles]
  (loop [charels        charels
         x-idx          x
         [char & chars] line]
    (if char
      (do
        (recur (update-in charels [y-idx x-idx]
                          (fn [ch]
                            (assoc ch
                                   :char char
                                   :fg (:fg styles)
                                   :bg (or (:bg styles) (:bg ch)))))
               (inc x-idx)
               chars))
      charels)))


(defmethod -draw :text [{:keys [x y width height styles] :as node} charels]
  (loop [charels        charels
         y-idx          y
         [line & lines] (take height (:text node))]
    (if line
      (let [charels (draw-line charels (util/strunc line width) x y-idx styles)]
        (recur charels (inc y-idx) lines))
      charels)))

(defmethod -draw :fill [node charels]
  (-> charels
      (screen/update-bounding-box node assoc :bg (:bg (:props node)))
      (draw-nodes (:children node))))
