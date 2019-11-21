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

(def pal
  "Partial-left (same as clojure.core/partial)"
  partial)

(defn par
  "Partial-right, partially apply rightmost function arguments."
  [f & xs]
  (fn [& ys]
    (apply f (concat ys xs))))

(defn strunc [s n]
  (if (> (count s) n)
    (subs s 0 n)
    s))

(comment
  (reduce-for [] [x (range 3)
                  y (range 3)]
    (conj % [x y])))
