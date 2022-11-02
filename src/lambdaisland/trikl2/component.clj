(ns lambdaisland.trikl2.component
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.display :as display]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl1.util :as util]
   [lambdaisland.trikl1.simple-object-system :as obj]
   [lambdaisland.trikl2.event-loop :as event-loop]))

(obj/defklass Visitable []
  (visit [{:keys [children] :as self} visitor]
    (reduce
     (fn [acc child]
       (visitor child))
     (visitor self)
     children)))

(obj/defklass Component [Visitable]
  (preferred-size [self] [0 0])
  (minimum-size [self] (self 'preferred-size))
  (maximum-size [self] (self 'preferred-size))
  (draw [{:keys [window]} surface]))

(obj/defklass TextLine [Component]
  :- {:text string? :fg any? :bg any?}
  (preferred-size [{:keys [text]}]
    [1 (.length text)])

  (draw [{:keys [window text fg bg]} surface]
    (surface 'write-line 0 0 text fg bg)))

(obj/defklass Stack [Component]
  )
