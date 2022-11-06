(ns lambdaisland.trikl2.event-loop
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.log :as log]
   [lambdaisland.trikl1.screen :as screen]
   [lambdaisland.trikl1.simple-object-system :as obj]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl1.util :as util])
  (:import (java.util.concurrent Delayed
                                 DelayQueue
                                 PriorityBlockingQueue
                                 TimeUnit
                                 BlockingQueue
                                 LinkedBlockingQueue)))

(set! *warn-on-reflection* true)
(set! *math-context* :warn-on-boxed)

(obj/defklass EventLoop []
  (prep [_]
    {:event-queue (PriorityBlockingQueue.
                   11
                   (comparator (fn [this that]
                                 (< (:priority this 100)
                                    (:priority that 100)))))
     :delay-queue (DelayQueue.)
     :running?    false})

  (on-event [self e]
    ;; override
    )

  (enqueue [{:keys [^BlockingQueue event-queue
                    ^BlockingQueue delay-queue]} e]
    (log/trace :event-loop/enqueueing e)
    (if-let [ms (:delay-ms e)]
      (.offer delay-queue (util/delayed e ms))
      (.offer event-queue e)))

  (start! [{:keys [^BlockingQueue event-queue
                   ^BlockingQueue delay-queue
                   running?]
            :as   self}]
    (when-not running?
      (swap! self assoc :runnning? true)
      (let [delay-thread (util/thread "trikl-delay-handling"
                           (try
                             (loop [e (.take delay-queue)]
                               (.offer event-queue @e)
                               (when (not= ::stop (:type @e))
                                 (recur (.take delay-queue))))
                             (catch Throwable t
                               (println "delay-error:" t))))
            event-thread (util/thread "trikl-event-handling"
                           (loop [e (.take event-queue)]
                             (if (not= ::stop (:type e))
                               (do
                                 (try
                                   (self 'on-event e)
                                   (catch Throwable t
                                     (println "error handling event:" e "\n" t)))
                                 (recur (.take event-queue)))
                               (swap! self assoc :running? false))))]
        (.start ^Thread delay-thread)
        (.start ^Thread event-thread))))

  (stop! [self]
    (self 'enqueue {:type ::stop :delay-ms 0 :priority -1})))
