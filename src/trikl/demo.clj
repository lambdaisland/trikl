(ns trikl.demo
  (:require [trikl.core :as t]))

(def clients (atom []))

(def stop-server (t/start-server #(swap! clients conj %) 1357))

#_(stop-server)

(t/render (last @clients)
          [:cols
           [:line-box {:x 10 :y 5 :width 20 :height 10 :styles {:bg [50 50 200]}}
            [:line-box {:x 1 :y 1 :width 18 :height 8 :styles {:bg [200 50 0]}}
             [:box {:x 3 :y 1}
              [:span {:styles {:fg [30 30 150]}} "hello\n"]
              [:span {:styles {:fg [100 250 100]}} \x "  world"]]]]])

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
        [x y width height] (t/apply-viewport attrs screen)
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
