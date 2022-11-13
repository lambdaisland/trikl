(ns lambdaisland.trikl1.term
  "Terminal (VT100/ANSI/...) commands

  Not being very rigorous about this stuff, just hard coding the bits that we
  need and that seem to be broadly supported. This is a stamp collection."
  (:require [lambdaisland.trikl1.log :as log]))

(def ESC
  "The escape byte (27) as a char"
  \u001b)

(defn control-sequence ^String [& args]
  (apply str ESC \[ args))

(def ALTERNATE-SCREEN (control-sequence "?1049h"))
(def REGULAR-SCREEN (control-sequence "?1049l"))
(def CLEAR-SCREEN (control-sequence "2J"))
(def UPPER-LEFT (control-sequence "H"))
(def RESET-STYLES (control-sequence "m"))
(def SHOW-CURSOR (control-sequence "?25h"))
(def HIDE-CURSOR (control-sequence "?25l"))
(def SAVE-CURSOR-POS (control-sequence "s"))
(def RESTORE-CURSOR-POS (control-sequence "u"))
(def REQUEST-POSITION (control-sequence "6n"))
(def LINEWRAP-OFF (control-sequence "?7l"))
(def LINEWRAP-ON (control-sequence "?7l"))

(defn move-to
  "Move cursor to absolute screen position"
  [col row]
  (control-sequence (inc row) ";" (inc col) "H"))

(def relative-moves {:up \A :down \B :right \C :left \D})

(defn move-relative
  "Move cursor from current position to new position via relative commands"
  [[^long from-col ^long from-row] [^long to-col ^long to-row]]
  (log/trace :term/moving-relative {:from [from-col from-row] :to [to-col to-row]})
  (str
   (when (< to-col from-col)
     (control-sequence (- from-col to-col) (:left relative-moves)))
   (when (< from-col to-col)
     (control-sequence (- to-col from-col) (:right relative-moves)))
   (when (< to-row from-row)
     (control-sequence (- from-row to-row) (:up relative-moves)))
   (when (< from-row to-row)
     (control-sequence (- to-row from-row) (:down relative-moves)))))

(def foreground-color-rgb
  (memoize
   (fn [color]
     (if (nil? color)
       (str ESC "[39m")
       (let [[r g b] color]
         (str ESC "[38;2;" r ";" g ";" b "m"))))))

(def background-color-rgb
  (memoize
   (fn [color]
     (if (nil? color)
       (str ESC "[49m")
       (let [[r g b] color]
         (str ESC "[48;2;" r ";" g ";" b "m"))))))
