(ns trikl.render
  "Render markup, and re-render components."
  (:require [trikl.screen :as screen]
            [trikl.tree :as tree]
            [trikl.util :refer [par]]
            #_[lambdaisland.ansi :as ansi]))

(defn- bounding-box [component]
  (select-keys component [:x :y :width :height]))

(defn- grow-bounding-box [prev-state next-state path]
  (let [prev-bb (bounding-box (get-in prev-state path))
        next-bb (bounding-box (get-in prev-state path))]
    (if (or (= prev-bb next-bb) (= 1 (count path)))
      path
      (grow-bounding-box prev-state next-state (butlast path)))))

;; screen resize -> full redraw = draw on blank, diff with blank, start with a CLEAR SCREEN
;; top-level component resize -> draw on blank, diff with prev-screen, no CLS

(defn render-component [state out cpath]
  (let [cpath     (grow-bounding-box state state cpath)
        component (get-in state cpath)

        prev-screen    (:screen state)
        screen-resize? (not (and prev-screen (= (:size prev-screen) (:size state))))
        full-tree?     (= cpath [:ui-tree])

        prev-matrix (:charels prev-screen)
        draw-base   (if (or full-tree? screen-resize?)
                      (apply screen/charel-matrix (:size state))
                      (screen/blank-bounding-box prev-matrix component))
        next-matrix (tree/draw component draw-base)

        sb (StringBuilder.)
        _  (when screen-resize?
             (.append sb screen/CSI-RESET-STYLES)
             (.append sb screen/CSI-CLEAR-SCREEN))

        next-styles (screen/diff sb
                                 (if screen-resize?
                                   {}
                                   (:styles prev-screen))
                                 (if screen-resize?
                                   draw-base
                                   (:charels prev-screen))
                                 next-matrix)]

    #_(prn (ansi/token-stream (str sb)))

    (.write out (.getBytes (str sb)))
    (update state :screen
            assoc
            :size (:size state)
            :styles next-styles
            :charels next-matrix)))

(declare atom-factory)

(defn render-component! [client cpath]
  (swap! (:state client)
         (fn [state]
           (binding [tree/*atom-factory* (atom-factory client)]
             (let [cpath (cons :ui-tree cpath)
                   component (get-in state cpath)]
               (-> state
                   (update-in cpath
                              (comp (par tree/layout (apply tree/new-context (:size state)))
                                    (par tree/receive-props (:props component))))
                   (render-component (:out client) cpath))))))
  client)

(declare client-render)

(defn wait-for-size-and-render [client markup]
  (let [try-render (fn [_ _ _ state]
                     (when (:size state)
                       (remove-watch (:state client) ::deferred-render)
                       (client-render client markup)))]
    (add-watch (:state client) ::deferred-render try-render)
    (try-render nil nil nil @(:state client))))

(defn- client-render [client markup]
  (swap! (:state client)
         (fn [{:keys [size] :as state}]
           (if (nil? size)
             (wait-for-size-and-render client markup)
             (binding [tree/*atom-factory* (atom-factory client)]
               (-> state
                   (update :screen
                           (fn [screen]
                             (if screen
                               screen
                               (apply screen/new-screen size))))
                   (update :ui-tree
                           (fn [ui-tree]
                             (tree/layout
                              (if ui-tree
                                (tree/receive-markup ui-tree markup)
                                (tree/init markup))
                              (apply tree/new-context size))))
                   (render-component (:out client) [:ui-tree])))))))

(defn render! [target markup]
  (if-let [clients (:clients target)]
    (run! (par client-render markup) @clients)
    (client-render target markup))
  target)

(defn atom-factory [client]
  (fn [init]
    (let [cpath tree/*node-path*
          cstate (atom init)]
      (add-watch cstate
                 ::reactive-render
                 (fn [k ref old new]
                   (render-component! client cpath)))
      cstate)))

(defn client-focus [client id]
  (swap! (:state client) assoc :focus id))

(defn focus! [target id]
  (if-let [clients (:clients target)]
    (run! (par client-focus id) @clients)
    (client-focus target id)))
