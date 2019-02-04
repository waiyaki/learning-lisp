(ns cljapplied.ch5.stores)

(declare query)

;; Take a product and look up the price on two or more online stores.
(defn query-stores [product stores]
  (let [futures (doall
                 (for [store stores]
                   (future (query store product))))]
    (map deref futures)))

;; Time an async op and later get the beginning and ending time for its execution
(defn long-running-task []
  (Thread/sleep 2500))

(defn launch-timed []
  (let [begin (promise)
        end (promise)]
    (future (deliver begin (System/currentTimeMillis))
            (long-running-task)
            (deliver end (System/currentTimeMillis)))
    (println "Task began at" @begin)
    (println "Task ended at" @end)))

(launch-timed)
