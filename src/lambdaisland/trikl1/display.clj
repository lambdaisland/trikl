(ns lambdaisland.trikl1.display
  "Representation of terminal state, and display-level diffing (committing).

  Replaces trikl1.screen, but inverses the relationship between display/screen and connection."
  (:require [lambdaisland.trikl1.term :as term]
            [lambdaisland.trikl1.util :as util]
            [lambdaisland.trikl1.log :as log]
            [lambdaisland.trikl1.connection :as conn])
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
  ([display]
   (charel-matrix (first (:size display))
                  (second (:size display))))
  ([width height]
   (vec (repeat height (blank-line width)))))

(defn new-display
  ([{:keys [conn width height]
     :or {width 0 height 0}}]
   (let [matrix (charel-matrix width height)]
     {:cursor [0 0]
      :fg     nil
      :bg     nil
      :size   [width height]
      :matrix matrix
      :conn   conn})))

(def NULL_ITERATOR
  (reify Iterator
    (hasNext [_] false)
    (next [_] nil)))

(defn term-move-relative
  "Write commands to move the cursor to a new location, and update the cursor
  state accordingly."
  [^StringBuilder sb {:keys [cursor] :as display} target]
  (log/trace :display/moving-relative {:from cursor :to target})
  (.append sb (term/move-relative cursor target))
  (assoc display :cursor target))

(defn diff-row [^StringBuilder sb row-idx display ^Iterable old ^Iterable new]
  (let [^Iterator old-it (if old (.iterator old) NULL_ITERATOR)
        ^Iterator new-it (if new (.iterator new) NULL_ITERATOR)]
    (loop [display display
           col-idx 0
           streak? false
           old-ch  (and (.hasNext old-it) (.next old-it))
           new-ch  (and (.hasNext new-it) (.next new-it))]
      (log/trace :diff-row/char-diff {:old old-ch :new new-ch})
      (let [old-next? (.hasNext old-it)
            new-next? (.hasNext new-it)]
        (if (= old-ch new-ch)
          ;; no difference, move on to next charel
          (if new-next?
            (recur display
                   (inc col-idx)
                   false
                   (when old-next? (.next old-it))
                   (when new-next? (.next new-it)))
            display)
          ;; charel difference
          (let [{:keys [char fg bg] :or {char \space}} new-ch
                fg?                                    (not= fg (:fg display))
                bg?                                    (not= bg (:bg display))
                {:keys [cursor] :as display}           (if streak?
                                                         display
                                                         (term-move-relative sb display [col-idx row-idx]))]
            (when fg? (.append sb (term/foreground-color-rgb fg)))
            (when bg? (.append sb (term/background-color-rgb bg)))
            (.append sb char)
            (let [new-display (assoc (if (or fg? bg?) new-ch display)
                                     :cursor
                                     (doto (update cursor 0 inc)
                                       (#(log/trace :new-cursor %))))]
              (if new-next?
                (recur new-display
                       (inc col-idx)
                       true
                       (when old-next? (.next old-it))
                       (when new-next? (.next new-it)))
                new-display))))))))

(defn diff
  "Perform a display diff, generating the necessary terminal commands to turn the
  old terminal state into the new one.

  The terminal state is given by a map of `:fg`, `:bg` and `:cursor` (`[col
  row]`) representing the current drawing styles and cursor position of the
  terminal, and a vector of vector of character elements.

  The terminal commands are added side-effectfully to the StringBuilder, the
  return value is the updated terminal drawing state. (actually it is the last
  Charel that was drawn, but it should be treated as a map with :fg and :bg
  keys.)
  "
  [^StringBuilder sb display ^Iterable old ^Iterable new]
  (let [^Iterator old-row-it (.iterator old)
        ^Iterator new-row-it (.iterator new)]
    (loop [display display
           row-idx 0
           old-row (and (.hasNext old-row-it) (.next old-row-it))
           new-row (and (.hasNext new-row-it) (.next new-row-it))]
      (let [old-next? (.hasNext old-row-it)
            new-next? (.hasNext new-row-it)]
        (let [display (diff-row sb row-idx display old-row new-row)]
          (if new-next?
            (recur display
                   (inc row-idx)
                   (when old-next? (.next old-row-it))
                   (when new-next? (.next new-row-it)))
            display))))))

(defn resize [{:keys [size matrix] :as display} [^long width ^long height]]
  (log/info :display/resizing {:from size :to [width height]})
  (let [[^long orig-width ^long orig-height] size

        resize-line (if (<= width orig-width)
                      #(vec (take width %))
                      #(into % (repeat (- width orig-width) BLANK)))
        matrix (if (<= height orig-height)
                 (mapv resize-line (take height matrix))
                 (into (mapv resize-line matrix)
                       (charel-matrix width (- height orig-height))))]
    (assoc display
           :size [width height]
           :matrix matrix)))

(defn flush!
  "Flushes the contents of !matrix to the display, and updates the connection state.
  To use this first initialize the !matrix atom to a matrix of the same size as
  the display, apply changes to it, and then call [[flush!]]."
  [{:keys [matrix conn] :as display} new-matrix]
  (let [sb (StringBuilder.)
        new-display (diff sb display matrix new-matrix)]
    (conn/write conn (str sb))
    (assoc new-display
           :matrix new-matrix)))
