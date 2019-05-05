(ns trikl.demo2
  (:require [trikl.tree :as tree]
            [trikl.client :as client]
            [trikl.screen :as screen]
            [trikl.core2 :as core2]))

(defn state []
  (atom {}))

(defn C1 [props]
  (into [:stack {:styles {:fg [50 50 200]
                          :bg [255 100 100]}}
         "C1"]
        (:children props)))


(defn C2 [props]
  {:init-state
   (fn [props]
     {:count 0})

   :render
   (fn [props]
     (let [state (state)]
       (into [:stack {:styles {:fg [150 50 200]
                               :bg [255 100 100]}}
              "C2"]
             (:children props))))})

(def +clients+ (atom []))
(def +server+ (client/start-server (partial swap! +clients+ conj)))

(def +c+ #(last @+clients+))

(+c+)

(-> (tree/init [C1 "foo" "bar"]))

(let [ui-tree (-> [:stack {:x 3 :y 3}
                   [C2 "foo"
                    [C1 "bar"]]]
                  tree/init
                  (tree/layout (apply tree/new-context @(:size (+c+)))))
      ]
  (core2/commit (+c+) ui-tree)

  )

(core2/render +server+
              [:stack {:x 3 :y 3}
               "xxx"
               ])

(client/stop-server +server+
                    )

(tree/receive-markup @(:ui-tree(+c+))
                     [:stack {:x 3 :y 3}
                      [C2 "fooxxxx"
                       [C1 "b"]
                       "xx"]] )
