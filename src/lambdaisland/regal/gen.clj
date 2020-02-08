(ns lambdaisland.regal.gen
  (:require [lambdaisland.regal.pattern :as pattern]
            [clojure.test.check.generators :as gen]))

(declare generator)

(defmulti -generator first)

(defmethod -generator :cat [[_ & rs]]
  (apply gen/tuple (map generator rs)))

(defmethod -generator :alt [[_ & rs]]
  (gen/one-of (map generator rs)))

(defmethod -generator :* [[_ r]]
  (gen/bind gen/pos-int
            (fn [i]
              (apply gen/tuple (repeat i (generator r))))))

(defmethod -generator :+ [[_ r]]
  (gen/bind gen/s-pos-int
            (fn [i]
              (apply gen/tuple (repeat i (generator r))))))

(defmethod -generator :? [[_ r]]
  (gen/one-of [(gen/return "")
               (generator r)]))

(defmethod -generator :range [[_ from to]]
  (gen/fmap char (gen/choose (long from) (long to))))

(defmethod -generator :class [[_ & cs]]
  (gen/one-of (for [c cs]
                (if (vector? c)
                  (gen/fmap char (gen/choose (long (first c)) (long (second c))))
                  (gen/return c)))))

(defmethod -generator :not [r]
  (let [pattern (pattern/compile r)]
    (gen/such-that #(re-find pattern (str %)) gen/char)))

(defmethod -generator :repeat [[_ r min max]]
  (gen/bind (gen/choose min max)
            (fn [i]
              (apply gen/tuple (repeat i (generator r))))))

(defn flatten
  ([v]
   (let [sb (StringBuilder.)]
     (run! #(flatten % sb) v)
     (str sb)))
  ([v ^StringBuilder sb]
   (cond
     (string? v)
     (.append sb v)

     (char? v)
     (.append sb v)

     (int? v)
     (assert false v)

     :else
     (run! #(flatten % sb) v))))

(defn generator [r]
  (cond
    (string? r)
    (gen/return r)

    (char? r)
    (gen/return r)

    (keyword? r)
    (case r
      :any gen/char
      (gen/return ""))

    :else
    (-generator r)))

(defn gen [r]
  (gen/fmap flatten (generator r)))

(defn sample [r]
  (gen/sample (gen r)))

(sample
 [:cat
  :start
  [:class [\a \z] [\A \Z] [\0 \9] \_ \-]
  "@"
  [:repeat [:range \0 \9] 3 5]
  [:* [:not \.]]
  [:alt "com" "org" "net"]
  :end])
