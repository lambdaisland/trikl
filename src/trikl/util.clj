(ns trikl.util)

(defn reduce-for* [binding-pairs init body]
  (if (next binding-pairs)
    (let [[[v val]] binding-pairs
          res (gensym "res")]
      `(reduce (fn [~res ~v]
                 ~(reduce-for* (next binding-pairs) res body))
               ~init
               ~val))
    (let [[[v val]] binding-pairs]
      `(reduce (fn [~'% ~v]
                 ~@body)
               ~init
               ~val))))

(defmacro reduce-for
  {:style/indent 2}
  [init bindings & body]
  (let [binding-pairs (partition 2 bindings)]
    (reduce-for* binding-pairs init body)))

(comment
  (reduce-for [] [x (range 3)
                  y (range 3)]
    (conj % [x y])))
