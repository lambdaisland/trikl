(ns repl-sessions.trikl3-20220511-coming-together
  (:require [lambdaisland.trikl2.ui :as ui]
            [lambdaisland.trikl2.component :as c]
            [lambdaisland.trikl1.ratom :as ratom]))

(def state (ratom/ratom {:count 0}))

(swap! state update :count inc)

(defn main []
  [c/Text {:fg [0 0 200]
           :bg [255 255 255]}  "count=" (:count @state)
   "\n" "hello"])

(def telnet (ui/telnet-ui
             main
             {:mode :inline}))

((:stop! telnet))

telnet


(require '[lambdaisland.ansi :as ansi])
(ansi/text->hiccup
 "\u001B[38;2;0;0;200m\u001B[48;2;255;255;255mhello=0\u001B[39m\u001B[49m")


(ansi/token-stream
 "\u001B[41D")

(supers (class (fn [])))
