(ns repl-sessions.event-loop-20221025
  (:import
   (java.util.concurrent Delayed
                         DelayQueue
                         PriorityBlockingQueue
                         TimeUnit)))



(def q
  (PriorityBlockingQueue.
   11
   (comparator (fn [this that]
                 (< (:priority this 0)
                    (:priority that 0))))))

(def dq (DelayQueue.))

(.offer q {:priority (rand-int 100)})

(.take q)

(defn delayed [val ms]
  (let [target-ms (+ ms (System/currentTimeMillis))]
    (reify
      Delayed
      (getDelay [this unit]
        (.convert unit (- target-ms (System/currentTimeMillis))
                  TimeUnit/MILLISECONDS))
      (compareTo [this that]
        (- (.getDelay this TimeUnit/MILLISECONDS)
           (.getDelay that TimeUnit/MILLISECONDS)))
      clojure.lang.IDeref
      (deref [this]
        val))))

(delayed {:x 1} 1000)

(def f
  (future
    (loop [v (.take dq)]
      (println @v)
      (when-not (= :stop @v)
        (recur (.take dq))))))

(do
  (.offer dq (delayed {:x 1} 2000))
  (.offer dq (delayed {:x 2} 1000)))
