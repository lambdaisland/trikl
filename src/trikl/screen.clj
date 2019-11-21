(ns trikl.screen
  "Representation of terminal state, and screen-level diffing (committing)."
  (:import clojure.lang.PersistentVector
           java.lang.StringBuilder
           java.util.Iterator))

(set! *warn-on-reflection* true)
(set! *unchecked-math* :warn-on-boxed)

(def ESC \u001b)

(def CSI-CLEAR-SCREEN     (str ESC "[2J"))
(def CSI-RESET-STYLES     (str ESC "[m"))

(defrecord Charel [char fg bg])

(def BLANK (->Charel \space nil nil))

(defn charel-matrix [height width]
  (vec (repeat height (vec (repeat width BLANK)))))

(defn new-screen [height width]
  {:styles  {}
   :size    [height width]
   :charels (charel-matrix height width)})

(def NULL_ITERATOR
  (reify Iterator
    (hasNext [_] false)
    (next [_] nil)))

(defn csi-move [[^long col ^long row]]
  (str ESC "[" (inc row) ";" (inc col) "H"))

(def csi-fg
  (memoize
   (fn [color]
     (if (nil? color)
       (str ESC "[39m")
       (let [[r g b] color]
         (str ESC "[38;2;" r ";" g ";" b "m"))))))

(def csi-bg
  (memoize
   (fn [color]
     (if (nil? color)
       (str ESC "[49m")
       (let [[r g b] color]
         (str ESC "[48;2;" r ";" g ";" b "m"))))))

(defn diff-row [^StringBuilder sb row-idx styles ^PersistentVector old ^PersistentVector new]
  (let [^Iterator old-it (if old (.iterator old) NULL_ITERATOR)
        ^Iterator new-it (if new (.iterator new) NULL_ITERATOR)]
    (loop [styles  styles
           col-idx 0
           streak? false
           old-ch  (and (.hasNext old-it) (.next old-it))
           new-ch  (and (.hasNext new-it) (.next new-it))]
      (let [old-next? (.hasNext old-it)
            new-next? (.hasNext new-it)]
        (if (= old-ch new-ch)
          (if new-next?
            (recur styles
                   (inc col-idx)
                   false
                   (when old-next? (.next old-it))
                   (when new-next? (.next new-it)))
            styles)
          (let [{:keys [char fg bg] :or {char \space}} new-ch
                fg? (not= fg (:fg styles))
                bg? (not= bg (:bg styles))]
            (when-not streak?
              (.append sb (csi-move [col-idx row-idx])))
            (when fg? (.append sb (csi-fg fg)))
            (when bg? (.append sb (csi-bg bg)))
            (.append sb char)
            (let [new-styles (if (or fg? bg?) new-ch styles)]
              (if new-next?
                (recur new-styles
                       (inc col-idx)
                       true
                       (when old-next? (.next old-it))
                       (when new-next? (.next new-it)))
                new-styles))))))))

(defn diff
  "Perform a screen diff, generating the necessary terminal commands to turn the
  old terminal state into the new one.

  The terminal state is given by a styles map (:fg and :bg), representing the
  current drawing styles of the terminal, and a vector of vector of character
  elements.

  The terminal commands are added side-effectfully to the StringBuilder, the
  return value is the updated terminal drawing state. (actually it is the last
  Charel that was drawn, but it should be treated as a map with :fg and :bg
  keys.)
  "
  [^StringBuilder sb styles ^PersistentVector old ^PersistentVector new]
  (let [^Iterator old-row-it (.iterator old)
        ^Iterator new-row-it (.iterator new)]
    (loop [styles  styles
           row-idx 0
           old-row (and (.hasNext old-row-it) (.next old-row-it))
           new-row (and (.hasNext new-row-it) (.next new-row-it))]
      (let [old-next? (.hasNext old-row-it)
            new-next? (.hasNext new-row-it)]
        (let [styles (diff-row sb row-idx styles old-row new-row)]
          (if new-next?
            (recur styles
                   (inc row-idx)
                   (when old-next? (.next old-row-it))
                   (when new-next? (.next new-row-it)))
            styles))))))

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
