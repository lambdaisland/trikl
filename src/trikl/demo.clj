(ns trikl.demo
  (:require [trikl.core :as t]
            [trikl.demo.world :as world]
            [trikl.util :refer [reduce-for]]))

(def clients (atom []))

(def stop-server (t/start-server #(swap! clients conj %) 1357))

(def stop-server (t/start-server #(t/render % [:cols
                                               [:line-box { :styles {:bg [50 50 200]}}]
                                               [:line-box { :styles {:bg [200 50 0]}}
                                                [:box {:x 3 :y 1}
                                                 [:span {:styles {:fg [30 30 150]}} "hello\n"]
                                                 [:span {:styles {:fg [100 250 100]}} \x "  world"]]]]) 1357))

(t/render (last @clients)
          [:cols
           [:line-box { :styles {:bg [50 50 200]}}]
           [:line-box { :styles {:bg [200 50 0]}}
            [:box {:x 3 :y 1}
             [:span {:styles {:fg [30 30 150]}} "hello\n"]
             [:span {:styles {:fg [100 250 100]}} \x "  world"]]]])

#_(stop-server)

(reset! (:size (last @clients)) [9 19])

(t/stop-client (last @clients))

(Math/cos Math/PI)
;; => 1.0


(Math/sin (* 2 Math/PI))

(def stop? (atom false))
(reset! stop? true)

(future
  (loop [step 0]
    (let [steps 30
          circle (* 2 Math/PI)
          radius 8
          x (Math/round (+ 44 (* 2 radius (Math/cos (/ (* circle step) steps)))))
          y (Math/round (+ 8 (* radius (Math/sin (/ (* circle step) steps)))))]
      (t/render (last @clients)
                [:line-box {:x x :y y :width 12 :height 5 :styles {:bg [50 50 200]}}
                 [:span {:x 5 :y 3} "Hix!!!!"]])
      (Thread/sleep 80)
      (when-not @stop?
        (recur (if (< step (dec steps))
                 (inc step)
                 0))))))

(last @clients)

(t/render (last @clients)
          [:cols
           [:line-box { :styles {:bg [50 50 200]}}]
           [:line-box { :styles {:bg [200 50 0]}}
            [:box {:x 3 :y 1}
             [:span {:styles {:fg [30 30 150]}} "hello\n"]
             [:span {:styles {:fg [100 250 100]}} \x "  world"]]]])

(t/render (last @clients)
          [:box
           [:line-box {:x 10 :y 6 :width 20 :height 12 :styles {:bg [50 50 200]}}]
           [:line-box {:x 5 :y 2 :width 18 :height 8 :styles {:bg [200 50 0]}}
            [:box {:x 3 :y 1}
             [:span {:styles {:fg [30 30 150]}} "hello\n"]
             [:span {:styles {:fg [100 250 100]}} \x "  world"]]]])

(t/render (last @clients)
          [:rows {:ratios [3 1]}
           [:cols
            [:line-box {:styles {:bg [0 0 200]}}]
            [:line-box {:styles {:bg [50 50 200]}}]]
           [:line-box {:styles {:bg [200 50 100]
                                :fg [255 80 130]}}
            [:box {:x 3 :y 1}
             [:span {:styles {:fg [30 30 150]}} "hello\n"]
             [:span {:styles {:fg [100 250 100]}} \x "  world"]]]])

(t/add-listener (last @clients)
                ::my-listener
                (fn [event]
                  (prn event)))


(t/render (last @clients)
          [:box
           (for [x (range 79)
                 y (range 53)
                 :let [page 2]]
             [:box {:x x :y y} (str (char (+ 255 (* page 79 53) x (* 53 y))))]
             )
           ]
          )

(defmethod t/draw :line-box [el screen]
  (let [[_ attrs children] (t/split-el el)
        [x y width height] (t/apply-bounding-box attrs screen)
        [tl t tr r br b bl l] (:lines attrs "╭─╮│╯─╰│")]
    (t/draw [:box attrs
             tl (repeat (- width 2) t) tr "\n"
             (repeat (- height 2)
                     (str l
                          (apply str (repeat (- width 2) \space))
                          r
                          "\n")
                     )
             bl (repeat (- width 2) b) br "\n"
             [:box (assoc attrs :x 1 :y 1 :width (- width 2) :height (- height 2))
              children]]
            screen)))

" ━│┃┄┅┆┇┈┉┊┋┌┍┎┏┐┑┒┓└┕┖┗┘┙┚┛├┝┞
┅┆┇┈┉┊┋┌┍┎┏┐┑┒┓└┕┖┗┘┙┚┛├┝┞┟┠┡┢┣┤┥┦┧┨┩┪┫┬┭┮┯┰┱┲┳┴┵┶┷┸┹┺┻┼┽┾┿╀╁╂╃╄╅╆╇╈╉╊╋╌╍╎╏═║╒╓
┺┻┼┽┾┿╀╁╂╃╄╅╆╇╈╉╊╋╌╍╎╏═║╒╓╔╕╖╗╘╙╚╛╜╝╞╟╠╡╢╣╤╥╦╧╨╩╪╫╬╭╮╯╰╱╲╳╴╵╶╷╸╹╺╻╼╽╾╿▀▁▂▃▄▅▆▇█
╯╰╱╲╳╴╵╶╷╸╹╺╻╼╽╾╿▀▁▂▃▄▅▆▇█▉▊▋▌▍▎▏▐░▒▓▔▕▖▗▘▙▚▛▜▝▞▟"

(apply str (map #(char %) (range 1100 1400)))

(range 30 110)

(char 100)

(defn app [{:keys [offset]} _]
  [:box
   (for [y (range 50)]
     [:span
      (for [x (range 80)]
        [:span {:styles {:bg [(+ offset (* x 2)) (+ offset (* y 2)) (+ offset x (* 4 y))]}} ({0 "-"
                                                                                              5 "\\"
                                                                                              10 "|"
                                                                                              15 "/"
                                                                                              20 "."
                                                                                              25 "*"} offset)])
      "\n"])])


(def state (atom {:size [300 300]
                  :pos [150 150]

                  :world (->> (world/random-tiles 300 300)
                              (iterate world/smooth)
                              (drop 3)
                              first)

                  }))

(defmethod t/draw :world-map [el {:keys [charels] :as screen}]
  (let [[el attrs] (t/split-el el)
        [^long x ^long y ^long width ^long height] (t/apply-bounding-box attrs screen)
        {:keys [world pos]} attrs
        [x-center y-center] pos
        x-offset (Math/round (double (- x-center (/ width 2))))
        y-offset (Math/round (double (- y-center (/ height 2))))]

    (assoc screen
           :charels
           (reduce-for charels [col (range width)
                                row (range height)]
             (assoc-in % [(+ y row) (+ x col) :char]
                       (first (get-in world [(+ y row y-offset) (+ x col x-offset) :entity/glyph])))))))

(first (get-in (world/random-tiles 3 3) [1 1 :entity/glyph]))

(t/render (last @clients) (repeat 20 "la"))
(t/render (last @clients)[:span {:styles {:fg [100 200 0] :bg [50 50 200]}} "Oh my!"])
(t/render (last @clients)[:box {:x 20 :y 10, :width 7, :height 3} "Hello, world!"])

(t/render (last @clients) (repeat 200 "*"))
(t/render (last @clients) [:line-box {}])
(t/time-info
 (t/render (last @clients) [:world-map (assoc @state :pos [152 148])])
 "render")
(t/render-watch! (last @clients) (fn [attrs _] [:world-map attrs]) state)

(t/render (last @clients)
          [:cols
           [:rows
            [:line-box "1"]
            [:line-box "2"]]
           [:rows
            [:line-box "3"]
            [:line-box "4"]]])

(t/split-el [:line-box ])

(do
  (swap! state update-in [:pos 0] dec)
  nil
  )


(unwatch! state)

(swap! state update :offset + 5)
(defn app [attrs children]
  [:rows
   [:line-box "Hiiiiii"]
   [:line-box {:height 15 :styles {:bg [100 50 50]}}]])

(t/render (last @clients) [app])

(def app-state (atom {:pos [10 10]}))

(t/render-watch! (last @clients)
                 (fn [{[x y] :pos} _]
                   [:box {:x x :y y} "X"])
                 app-state)

(swap! app-state update-in [:pos 0] inc)
(swap! app-state update-in [:pos 1] inc)

(t/unwatch! app-state)
