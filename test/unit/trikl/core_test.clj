(ns trikl.core-test
  (:require [trikl.core :as t]
            [clojure.test :refer :all]
            [lambdaisland.ansi :as ansi]))

(is (= [[{:row 1, :col 1} "x"] {}]
       (let [sb (StringBuilder.)
             styles (t/diff-row
                     sb
                     0
                     {}
                     [t/BLANK]
                     [(t/map->Charel {:char \x})])]
         [(ansi/token-stream (str sb)) styles])))


(is (=
     (let [sb (StringBuilder.)
           styles (t/diff-row
                   sb
                   0
                   {}
                   [(t/map->Charel {:char \x})]
                   [(t/map->Charel {:char \x})])]
       [(ansi/token-stream (str sb)) styles])
     ))

(+ 1 1)

(let [sb (StringBuilder.)
      styles (t/diff-row
              sb
              0
              {}
              [t/BLANK]
              [(t/map->Charel {:char \y :fg [10 20 30]})])]
  [(ansi/token-stream (str sb)) styles])
[[{:row 1, :col 1}
  {:foreground [:rgb 10 20 30]}
  "y"]
 {:char \y, :fg [10 20 30], :bg nil}]

(let [sb (StringBuilder.)
      styles (t/diff-row
              sb
              0
              {}
              [t/BLANK t/BLANK]
              [(t/map->Charel {:char \x :fg [10 20 30]})
               (t/map->Charel {:char \y :fg [10 20 30]})])]
  [(ansi/token-stream (str sb)) styles])
[[{:row 1, :col 1}
  {:foreground [:rgb 10 20 30]}
  "xy"]
 {:char \x, :fg [10 20 30], :bg nil}]


(let [sb (StringBuilder.)
      styles (t/diff-row
              sb
              0
              {}
              []
              [(t/map->Charel {:char \x :fg [10 20 30]})
               (t/map->Charel {:char \y :fg [10 20 30]})])]
  [(ansi/token-stream (str sb)) styles])

[[{:row 1, :col 1}
  {:foreground [:rgb 10 20 30]}
  "xy"]
 {:char \x, :fg [10 20 30], :bg nil}]



(let [sb (StringBuilder.)
      styles (t/diff-row
              sb
              0
              {}
              [(t/map->Charel {:char \x :fg [10 20 30]})
               (t/map->Charel {:char \y :fg [10 20 30]})]
              [])]
  [(ansi/token-stream (str sb)) styles])
[[{:row 1, :col 1} "  "] {}]


(t/time-info
 (let [sb (StringBuilder.)
       styles (t/diff-row
               sb
               0
               {}
               [t/BLANK t/BLANK t/BLANK]
               [(t/map->Charel {:char \x :fg [10 20 30]})
                t/BLANK
                (t/map->Charel {:char \y :fg [10 20 30]})])]
   [(ansi/token-stream (str sb)) styles])
 "")

[[{:row 1, :col 1}
  {:foreground [:rgb 10 20 30]}
  "x"
  {:row 1, :col 3}
  "y"]
 {:char \x, :fg [10 20 30], :bg nil}]
