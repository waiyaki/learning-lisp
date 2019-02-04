(ns cljapplied.ch5.stats)

(declare remote-send)

;; Hold the counter for a particular stat in an agent
(def pageview-stat (agent 0))

;; Call the remote service on every 10th update of the agent, as a form
;; of batching.
(add-watch
 pageview-stat
 :pageview
 (fn [key agent old new]
   (when (zero? (mod new 10))
     (remote-send key new))))

;; Update func to be executed asynchronously on the agent
(defn inc-stat [stat]
  (send-off stat inc))
