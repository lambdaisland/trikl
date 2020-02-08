(ns string-regex-specs
  (:require [clojure.core :as c]
            [clojure.spec-alpha2 :as s]
            [clojure.spec-alpha2.protocols :as proto]
            [clojure.test.check.generators :as gen])
  (:import java.util.regex.Pattern))

;; conform
;; unform
;; explain
;; gen
;; with-gen
;; describe

(defn literal
  [s]
  (let [pattern (Pattern/quote s)]
    ^{`proto/conform*  (fn [_ x settings-key settings]
                         (if (= x s) s ::s/invalid))
      `proto/unform*   (fn [_ x]
                         x)
      `proto/explain*  (fn [_ path via in x settings-key settings]
                         (when (not= x s)
                           [{:path path :pred `(~'= ~s ~x) :val x :via via :in in}]))
      `proto/gen*      (fn [spec overrides path rmap]
                         ((::gen spec)))
      `proto/with-gen* (fn [spec gfn]
                         (assoc spec ::gen gfn))
      `proto/describe* (fn [_]
                         s)}
    {::op      ::literal
     ::form    s
     ::pattern pattern
     ::regex   (Pattern/compile pattern)
     ::gen     (gen/return s)}))

(defn string-specize [s]
  (cond
    (::form s)             s
    (string? s)            (literal s)
    (qualified-keyword? s) (s/resolve-spec s)
    :else                  (throw (ex-info (c/str "Not allowed inside string-regex spec: " s)
                                           {:value s}))))

(defn proto-impl [spec]
  {`proto/conform*  (fn [spec x settings-key settings]
                      (def regex (::regex spec))
                      (def x x)
                      (if (re-matches (::regex spec) x) x ::s/invalid))
   `proto/unform*   (fn [_ x]
                      x)
   `proto/explain*  (fn [spec path via in x settings-key settings]
                      (when-not (re-matches (::regex spec) x)
                        [{:path path :pred (::form spec) :val x :via via :in in}]))
   `proto/gen*      (fn [spec overrides path rmap]
                      (::gen spec))
   `proto/with-gen* (fn [spec gfn]
                      (assoc spec ::gen gfn))
   `proto/describe* (fn [_]
                      (::form spec))})

(defn grouped-pattern [{p ::pattern}]
  (c/str "(" p ")"))

(defn string-spec [op form pattern gen]
  (let [spec {::op      op
              ::form    form
              ::pattern pattern
              ::regex   (Pattern/compile pattern)}]
    (-> spec
        (with-meta (proto-impl spec))
        (proto/with-gen* gen))))

(defn alternation
  [form args]
  (let [args (map string-specize args)]
    (string-spec ::alternation
                 `(| ~@(map ::form args))
                 (apply str (interpose "|" (map grouped-pattern args)))
                 (gen/one-of (map ::gen args)))))

(defn concatenation
  [form args]
  (let [args (map string-specize args)]
    (string-spec ::concatenation
                 `(cat ~@(map ::form args))
                 (apply c/str (map grouped-pattern args))
                 (gen/fmap #(apply c/str %) (apply gen/tuple (map ::gen args))))))

(defmacro | [& forms]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(| ~@forms))))

(defmacro scat [& forms]
  `(s/resolve-spec '~(s/explicate (ns-name *ns*) `(scat ~@forms))))

(defmethod s/expand-spec `|
  [[_ & forms]]
  {:clojure.spec/op `|
   :spec forms})

(defmethod s/create-spec `|
  [{:keys [spec]}]
  (alternation spec (s/resolve-spec spec)))

(defmethod s/expand-spec `scat
  [[_ & forms]]
  {:clojure.spec/op `scat
   :spec forms})

(defmethod s/create-spec `scat
  [{:keys [spec]}]
  (concatenation spec (s/resolve-spec spec)))




(comment
  (let [specs [::yyy (scat "ba" "r")] #_(| "foo" "bar")
        vals ["bar" "baq"]]
    (for [spec specs]
      [spec
       '=>
       [(gen/sample (s/gen spec))
        (s/describe spec)
        (for [val vals]
          [val '-> [(s/valid? spec val)
                    (s/conform spec val)
                    (s/explain-str spec val)]])
        #_(gen/sample (s/gen (s/with-gen ::xxx (gen/return "xxx"))))]]))

  (s/def ::xxx (| "foo" "bar"))
  (s/def ::yyy (| ::xxx "zzz"))
  (s/def ::zzz (scat ::xxx "zzz"))

  (s/explain-str (s/spec string?) "foo")

  (re-matches (::regex (| "foo" "bar")) "foos")

  x
  regex

  xpand
  args

  (s/conform
   (string-spec "bar")
   "foo")

  (proto/gen* (string-spec "bar") nil nil nil)

  (s/gen (string-spec "bar"))

  (gen/sample #_(proto/gen* (string-spec "bar") nil nil nil)
              (s/gen (string-spec "bar")))

  (#'s/spec-name (s/spec (s/cat :x #{\x} :y #{\y} :z #{\z})))

  (s/valid? (string-spec "bar") "bar")

  gen/char
  )
