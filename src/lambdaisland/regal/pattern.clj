(ns lambdaisland.regal.pattern
  "Compile Regal syntax to regex patterns.

     >>> (compile-str [:cat
                       :start
                       [:class [\\a \\z] [\\A \\Z] [\\0 \\9] \\_ \\-]
                       \"@\"
                       [:repeat [:range \\0 \\9] 3 5]
                       [:* [:not \\.]]
                       [:alt \"com\" \"org\" \"net\"]
                       :end])
     \"\\\\A[a-zA-Z0-9_-]\\\\Q@\\\\E[0-9]{3,5}([^.]*)(\\\\Qcom\\\\E|\\\\Qorg\\\\E|\\\\Qnet\\\\E)\\\\Z\" "
  (:import java.util.regex.Pattern))

;; - Do we need escaping inside [:class]? caret/dash?

(set! *warn-on-reflection* true)

(def tokens
  {:start "\\A"
   :end "\\Z"
   :any "."})

(declare regal->grouped)

(defmulti -regal->grouped first)

(defmethod -regal->grouped :cat [[_ & rs]]
  (map regal->grouped rs))

(defmethod -regal->grouped :alt [[_ & rs]]
  (interpose \| (map regal->grouped rs)))

(defmethod -regal->grouped :* [[_ r]]
  (list (regal->grouped r) \*))

(defmethod -regal->grouped :+ [[_ r]]
  (list (regal->grouped r) \+))

(defmethod -regal->grouped :? [[_ r]]
  (list (regal->grouped r) \?))

(defmethod -regal->grouped :range [[_ from to]]
  `^::grouped (\[ ~from \- ~to \]))

(defn- compile-class [cs]
  (reduce (fn [r c]
            (cond
              (string? c)
              (conj r c)

              (char? c)
              (conj r c)

              (vector? c)
              (conj r (first c) "-" (second c))))
          []
          cs))

(defmethod -regal->grouped :class [[_ & cs]]
  `^::grouped (\[ ~@(compile-class cs) \]))

(defmethod -regal->grouped :not [[_ & cs]]
  `^::grouped (\[ \^ ~@(compile-class cs) \]))

(defmethod -regal->grouped :repeat [[_ r & ns]]
  `^::grouped (~(regal->grouped r) \{ ~@(interpose \, (map str ns)) \} ))

(defn regal->grouped [r]
  (cond
    (string? r)
    (Pattern/quote r)

    (char? r)
    (Pattern/quote (str r))

    (keyword? r)
    (get tokens r)

    :else
    (-regal->grouped r)))

(defn simplify [g]
  (if (or (string? g) (char? g) (::grouped (meta g)))
    g
    (let [g (map simplify g)]
      (if (and (= 1 (count g))
               (string? (first g)))
        (first g)
        g))))

(defn grouped->re-str
  ([g]
   (let [sb (StringBuilder.)]
     (run! #(grouped->re-str % sb) g)
     (str sb)))
  ([g ^StringBuilder sb]
   (cond
     (string? g)
     (.append sb ^String g)

     (char? g)
     (.append sb ^Character g)

     (seq? g)
     (if (::grouped (meta g))
       (run! #(grouped->re-str % sb) g)
       (do
         (.append sb "(")
         (run! #(grouped->re-str % sb) g)
         (.append sb ")")))
     :else
     (assert false g))))

(defn compile-str [r]
  (-> r
      regal->grouped
      simplify
      grouped->re-str))

(defn compile [r]
  (Pattern/compile (compile-str r)))
