(ns lambdaisland.trikl2.event-loop
  (:require
   [lambdaisland.trikl1.connection :as conn]
   [lambdaisland.trikl1.screen :as screen]
   [lambdaisland.trikl1.telnet :as telnet]
   [lambdaisland.trikl1.term :as term]
   [lambdaisland.trikl1.util :as util]
   [lambdaisland.trikl1.simple-object-system :as obj])
  (:import (java.util.concurrent Delayed
                                 DelayQueue
                                 PriorityBlockingQueue
                                 TimeUnit
                                 BlockingQueue
                                 LinkedBlockingQueue)))

(set! *warn-on-reflection* true)
(set! *math-context* :warn-on-boxed)

(obj/defklass EventLoop []
  (prep [{:keys [on-event]}]
    ^{'on-event on-event}
    {:event-queue (PriorityBlockingQueue.
                   11
                   (comparator (fn [this that]
                                 (< (:priority this 100)
                                    (:priority that 100)))))
     :delay-queue (DelayQueue.)})

  (start! [{:keys [^BlockingQueue event-queue
                   ^BlockingQueue delay-queue
                   running?]}]
    (when-not running?
      (swap! this assoc :runnning? true)
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
                                   (obj/call this 'on-event e)
                                   (catch Throwable t
                                     (println "error handling event:" e "\n" t)))
                                 (recur (.take event-queue)))
                               (swap! this assoc :running? false))))]
        (.start ^Thread delay-thread)
        (.start ^Thread event-thread))))

  (stop! [$]
    (obj/call $ 'enqueue {:type ::stop
                          :delay-ms 0
                          :priority -1}))

  (enqueue [{:keys [^BlockingQueue event-queue
                    ^BlockingQueue delay-queue]} e]
    (if-let [ms (:delay-ms e)]
      (.offer delay-queue (util/delayed e ms))
      (.offer event-queue e))))
