(ns trikl.core2
  "The new UI-tree based API"
  (:require [trikl.screen :as screen]
            [trikl.tree :as tree]
            [trikl.util :refer [par]]))

(defn commit [client]
  (let [sb                      (StringBuilder.)
        node                    @(:ui-tree client)
        [width height :as size] @(:size client)
        prev-screen             @(:screen client) ;; :styles :size :charels
        empty-matrix            (screen/charel-matrix width height)
        prev-screen             (if (and prev-screen (= (:size prev-screen) size))
                                  prev-screen
                                  (do
                                    (.append sb screen/CSI-RESET-STYLES)
                                    (.append sb screen/CSI-CLEAR-SCREEN)
                                    {:styles  {}
                                     :size    size
                                     :charels empty-matrix}))
        charels                 (tree/draw node empty-matrix)
        styles                  (screen/diff sb (:styles prev-screen) (:charels prev-screen) charels)]
    (swap! (:screen client)
           assoc
           :styles styles
           :size size
           :charels charels)
    (.write (:out client) (.getBytes (str sb)))))

(defn- client-render [client markup]
  (let [[width height] @(:size client)]
    (swap! (:ui-tree client) (fn [ui-tree]
                               (tree/layout
                                (if ui-tree
                                  (tree/receive-markup ui-tree markup)
                                  (tree/init markup))
                                (tree/new-context width height))))
    (commit client)))

(defn render [target markup]
  (if-let [clients (:clients target)]
    (run! (par client-render markup) @clients)
    (client-render target markup)))
