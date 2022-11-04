(ns lambdaisland.trikl2.surface
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.display :as display]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl1.util :as util]
   [lambdaisland.trikl1.simple-object-system :as obj]
   [lambdaisland.trikl2.event-loop :as event-loop]))

(obj/defklass Surface []
  :- {:x int?
      :y int?
      :width int?
      :height int?
      :window some?}

  (put-char [{:keys [x y width height window]} x' y' ch fg bg]
    (if (and (< y' height)
             (< x' width))
      (swap! window
             update :canvas assoc-in [(+ y y') (+ x x')]
             (display/->Charel ch fg bg))
      (println "put-char out of bounds"
               y' '< height '/
               x' '< width)))

  (write-line [{:keys [x y width height window]} x' y' line fg bg]
    (if (and (< y' height)
             (< x' width))
      (swap! window
             update :canvas
             (fn [m]
               (util/reduce-idx
                (fn [idx m ch]
                  (prn '[x x' idx] [x x' idx])
                  (if (< (+ x' idx) width)
                    (assoc-in m [(+ y y') (+ x x' idx)]
                              (display/->Charel ch fg bg))
                    m))
                m
                line)))
      (println "write-line out of bounds"
               y' '< height '/
               x' '< width)))

  (subsurface [{:keys [x y height width] :as self} x' y' w h]
    (obj/with self {:x (+ x x')
                    :y (+ y y')
                    :width (min w (- width x'))
                    :height (min h (- height y'))})))
