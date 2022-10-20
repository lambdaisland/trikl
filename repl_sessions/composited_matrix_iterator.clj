(ns composited-matrix-iterator
  (:require [lambdaisland.trikl1.screen :as screen]))

(defn composite-matrix [[width height] components]
  (reify Iterable
    (iterator [this]
      (let [row (volatile! 0)]
        (reify java.util.Iterator
          (hasNext [this] (< @row height))
          (next [this]
            (let [row-idx @row
                  col (volatile! 0)]
              (vswap! row inc)
              (reify Iterable
                (iterator [_]
                  (reify java.util.Iterator
                    (hasNext [this] (< @col width))
                    (next [this]
                      (let [col-idx @col]
                        (vswap! col inc)
                        (reduce
                         (fn [_ {:keys [size position matrix]}]
                           (let [[w h] size
                                 [x y] position]
                             (if (and (<= x row-idx (+ x w -1))
                                      (<= y col-idx (+ y h -1)))
                               (reduced (get-in matrix [(- row-idx x) (- col-idx y)]))
                               nil)))
                         nil
                         components)))))))))))))



(time (doall (for [row
                   (iterator-seq
                    (.iterator
                     (composite-matrix
                      [10 10]
                      [{:size [5 5]
                        :position [2 2]
                        :matrix (-> (screen/charel-matrix 5 5)
                                    (screen/update-matrix
                                     (fn [col row charel]
                                       (if (and (<= 2 col 5)
                                                (<= 1 row 3))
                                         (assoc charel :char \x)
                                         charel))))}
                       {:size [10 10]
                        :position [0 0]
                        :matrix (-> (screen/charel-matrix 10 10)
                                    (screen/update-matrix
                                     (fn [col row charel]
                                       (assoc charel :char \#))))}])))]
               (doall (for [col (iterator-seq (.iterator row))]
                        col)))))
