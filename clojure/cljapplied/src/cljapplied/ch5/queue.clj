(ns cljapplied.ch5.queue)

(defn queue
  "Create a new stateful queue"
  []
  (ref clojure.lang.PersistentQueue/EMPTY))


(defn enq
  "Enqueue and item in q"
  [q item]
  (dosync
   (alter q conj item)))


(defn deq
  "Dequeue an item from q (nil if none)"
  [q]
  (dosync
   (let [item (peek @q)]
     (alter q pop)
     item)))
