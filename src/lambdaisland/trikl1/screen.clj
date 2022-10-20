(ns lambdaisland.trikl1.screen
  "Representation of terminal state, and screen-level diffing (committing)."
  (:require [lambdaisland.trikl1.term :as term])
  (:import (clojure.lang PersistentVector)
           (java.lang StringBuilder)
           (java.util Iterator)))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(defrecord Charel [char fg bg])

(def BLANK (->Charel \space nil nil))

(def blank-line
  (memoize #(vec (repeat % BLANK))))

(defn charel-matrix
  ([screen]
   (charel-matrix (first (:size screen))
                  (second (:size screen))))
  ([height width]
   (vec (repeat height (blank-line width)))))

(defn new-screen [height width]
  {:term-state {:cursor [0 0]}
   :size       [height width]
   :charels    (charel-matrix height width)})

(def NULL_ITERATOR
  (reify Iterator
    (hasNext [_] false)
    (next [_] nil)))

(defn term-move-relative
  "Write commands to move the cursor to a new location, and update the cursor
  state accordingly."
  [^StringBuilder sb {:keys [cursor] :as term-state} target]
  (println "Moving to" target)
  (.append sb (term/move-relative cursor target))
  (assoc term-state :cursor target))

(defn diff-row [^StringBuilder sb row-idx term-state ^PersistentVector old ^PersistentVector new]
  (let [^Iterator old-it (if old (.iterator old) NULL_ITERATOR)
        ^Iterator new-it (if new (.iterator new) NULL_ITERATOR)]
    (loop [term-state  term-state
           col-idx 0
           streak? false
           old-ch  (and (.hasNext old-it) (.next old-it))
           new-ch  (and (.hasNext new-it) (.next new-it))]
      (let [old-next? (.hasNext old-it)
            new-next? (.hasNext new-it)]
        (if (= old-ch new-ch)
          ;; no difference, move on to next charel
          (if new-next?
            (recur term-state
                   (inc col-idx)
                   false
                   (when old-next? (.next old-it))
                   (when new-next? (.next new-it)))
            term-state)
          ;; charel difference
          (let [{:keys [char fg bg] :or {char \space}} new-ch
                fg? (not= fg (:fg term-state))
                bg? (not= bg (:bg term-state))
                {:keys [cursor] :as term-state} (if streak?
                                                  term-state
                                                  (term-move-relative sb term-state [col-idx row-idx]))]
            (println "new cursor" cursor)
            (when fg? (.append sb (term/foreground-color-rgb fg)))
            (when bg? (.append sb (term/background-color-rgb bg)))
            (.append sb char)
            (let [new-term-state (assoc (if (or fg? bg?) new-ch term-state)
                                        :cursor
                                        (update cursor 0 inc))]
              (println "new-term-state" new-term-state)
              (if new-next?
                (recur new-term-state
                       (inc col-idx)
                       true
                       (when old-next? (.next old-it))
                       (when new-next? (.next new-it)))
                new-term-state))))))))

(defn diff
  "Perform a screen diff, generating the necessary terminal commands to turn the
  old terminal state into the new one.

  The terminal state is given by a map of `:fg`, `:bg` and `:cursor` (`[col
  row]`) representing the current drawing styles and cursor position of the
  terminal, and a vector of vector of character elements.

  The terminal commands are added side-effectfully to the StringBuilder, the
  return value is the updated terminal drawing state. (actually it is the last
  Charel that was drawn, but it should be treated as a map with :fg and :bg
  keys.)
  "
  [^StringBuilder sb term-state ^PersistentVector old ^PersistentVector new]
  (let [^Iterator old-row-it (.iterator old)
        ^Iterator new-row-it (.iterator new)]
    (loop [term-state term-state
           row-idx 0
           old-row (and (.hasNext old-row-it) (.next old-row-it))
           new-row (and (.hasNext new-row-it) (.next new-row-it))]
      (let [old-next? (.hasNext old-row-it)
            new-next? (.hasNext new-row-it)]
        (let [term-state (diff-row sb row-idx term-state old-row new-row)]
          (if new-next?
            (recur term-state
                   (inc row-idx)
                   (when old-next? (.next old-row-it))
                   (when new-next? (.next new-row-it)))
            term-state))))))

(defn resize [{:keys [size charels] :as screen} [height width]]
  (let [[orig-height orig-width] size
        resize-line (if (<= width orig-width)
                      #(vec (take width %))
                      #(into % (repeat (- orig-width width) BLANK)))
        matrix (if (<= height orig-height)
                 (mapv resize-line (take height charels))
                 (into (mapv resize-line charels)
                       (charel-matrix (- height orig-height) width)))]
    (assoc screen
           :size [height width]
           :charels matrix)))

#_
(defn update-bounding-box [charels {:keys [^long x ^long y ^long width ^long height]} f & args]
  (let [max-y    (count charels)
        max-x    (count (first charels))
        row-idxs (range (min max-y y)
                        (min max-y (+ y height)))
        col-idxs (range (min max-x x)
                        (min max-x (+ x width)))]
    (reduce (fn [charels y]
              (update charels y
                      (fn [row]
                        (reduce (fn [row x]
                                  (let [row (apply update row x f args)]
                                    (prn x y (get row x))
                                    row
                                    ))
                                row
                                col-idxs))))
            charels
            row-idxs)))
#_
(defn blank-bounding-box [charels box]
  (update-bounding-box charels box (constantly BLANK)))


#_
(let [r (->Charel \r [255 0 0] nil)
      g (->Charel \g [0 255 0] nil)
      B (->Charel \B nil [0 0 255])
      old [[r r r]
           [r g g]
           [g B B]]
      new [[B r r]
           [g g g]
           [g B B]]
      sb (StringBuilder.)]
  [(diff sb {} old new) (seq (str sb))])
