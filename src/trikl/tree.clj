(ns trikl.tree
  (:require [clojure.string :as str]))

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




(defn new-context [width height]
  {:x 0
   :y 0
   :width width
   :height height

   :min-x 0
   :min-y 0
   :max-x width
   :max-y height

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

(defn clip-bounding-box [{:keys [min-x max-x min-y max-y] :as ctx}
                         {:keys [x y width height] :as props}]
  (let [ctx (cond-> ctx
              x (assoc :x (clip min-x (+ (:x ctx) x) max-x))
              y (assoc :y (clip min-x (+ (:y ctx) y) max-y)))]
    (cond-> (assoc ctx :min-x (:x ctx) :min-y (:y ctx))
      width  (assoc :width (clip 0 width (- max-x (:x ctx))))
      height (assoc :height (clip 0 height (- max-y (:y ctx)))))))

(defn style-from-props [ctx props]
  (if-let [styles (:styles props)]
    (update ctx :styles merge styles)
    ctx))

(defn apply-props [ctx props]
  (-> ctx
      (clip-bounding-box props)
      (style-from-props props)))

(defn node [type props]
  {:type type
   :props props})

(defn markup->node [markup]
  (if (string? markup)
    markup
    (node (markup-type markup)
          (update (markup-props markup) :children (partial map markup->node)))))

(defn string-node [text]
  {:tag    :text
   :markup text
   :text   (str/split text #"(\r\n|\r|\n)")})

(def ^:dynamic *component-state* nil)

(declare init)

(defn component-node [tag props]
  (let [widget (tag props)]
    (if (map? widget)
      (let [state (if-let [init-fn (:init-state widget)]
                    (atom (init-fn props))
                    nil)
            child (binding [*component-state* state]
                    ((:render widget) props))]
        {:tag      tag
         :props    props
         :widget   widget
         :state    *component-state*
         :children [(init child)]})
      {:tag      tag
       :props    props
       :widget   {:render tag}
       :children [(init (tag props))]})))

(defn make-node [markup]
  (let [tag   (first markup)
        props (markup-props markup)]
    (cond
      (keyword? tag)
      {:tag      tag
       :props    props
       :children (map init (:children props))}

      (fn? tag)
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
  (if (string? markup)
    (if (= markup (:markup node))
      node
      (string-node markup))
    (let [tag   (first markup)
          props (markup-props markup)]
      (if (= tag (:tag node))
        (if (= props (:props node))
          node
          (receive-props node props))
        (make-node markup)))))

(def layout nil)
(def draw nil)

(defmulti -layout (fn [node ctx] (:tag node)))

(defn layout [node ctx]
  (-layout node (apply-props ctx (:props node))))

(defmulti -draw (fn [node charels] (:tag node)))

(defn draw [node charels]
  (-draw node charels))

(defn fit-to-children [{children :children :as node}]
  (if (seq children)
    (let [min-x (apply min (map :x children))
          min-y (apply min (map :y children))
          max-x (apply max (map #(+ (:x %) (:width %))
                                children))
          max-y (apply max (map #(+ (:y %) (:height %))
                                children))]
      (assoc node
             :x min-x
             :y min-y
             :width (- max-x min-x)
             :height (- max-y min-y)))
    node))

(defmethod -layout :default [node ctx]
  (if (seq (:children node))
    (-> node
        (update :children
                (partial map (fn [node]
                               (layout node ctx))))
        fit-to-children)
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
        (fit-to-children))
    node))

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

(defmethod -draw :default [node charels]
  (reduce #(draw %2 %1) charels (:children node)))

(defn- draw-line [charels line x y-idx styles]
  (loop [charels        charels
         x-idx          x
         [char & chars] line]
    (if char
      (recur (update-in charels [y-idx x-idx]
                        assoc
                        :char char
                        :fg (:fg styles)
                        :bg (:bg styles))
             (inc x-idx)
             chars)
      charels)))

(defmethod -draw :text [{:keys [x y styles] :as node} charels]
  (loop [charels        charels
         y-idx          y
         [line & lines] (:text node)]
    (if line
      (let [charels (draw-line charels line x y-idx styles)]
        (recur charels (inc y-idx) lines))
      charels)))