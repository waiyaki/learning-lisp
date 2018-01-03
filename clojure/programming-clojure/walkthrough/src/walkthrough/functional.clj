(defn stack-consuming-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else (+ (stack-consuming-fibo (- n 1))
             (stack-consuming-fibo (- n 2)))))


;; tail-fibo still stack overflows coz the JVM doesn't automatically do TCO
(defn tail-fibo [n]
  (letfn [(fib
            [current next n]
            (if (zero? n)
              current
              (fib next (+ current next) (dec n))))]
    (fib 0N 1N n)))

(defn recur-fibo [n]
  (letfn [(fib
            [current next n]
            (if (zero? n)
              current
              (recur next (+ current next) (dec n))))]
    (fib 0N 1N n)))


(defn lazy-seq-fibo
  ([]
   (concat [0 1] (lazy-seq-fibo 0N 1N)))
  ([a b]
   (let [n (+ a b)]
     (lazy-seq
      (cons n (lazy-seq-fibo b n))))))


(defn fibo []
  (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))


;; Given a sequence like
(def tosses [:h :t :t :h :h :h])

;; How many times in the seq does heads come up twice in a row?

;; Take 1 - use `recur`
(defn count-heads-pairs [coll]
  (loop [cnt 0 coll coll]
    (if (empty? coll)
      cnt
      (recur (if (= :h (first coll) (second coll))
               (inc cnt)
               cnt)
             (rest coll)))))

;; Take 2
(defn by-pairs [coll]
  (let [take-pair (fn [c]
                    (when (next c) (take 2 c)))]
    (lazy-seq
     (when-let [pair (seq (take-pair coll))]
       (cons pair (by-pairs (rest coll)))))))

(by-pairs tosses) ;; => ((:h :t) (:t :t) (:t :h) (:h :h) (:h :h))

(defn count-heads-pairs-2 [coll]
  (count (filter (fn [pair] (every? #(= :h %) pair))
                 (by-pairs coll))))

;; Implementing using partition
(partition 2 1 tosses) ;; => ((:h :t) (:t :t) (:t :h) (:h :h) (:h :h))

;; Encapsulate the count/filter idiom for counting pairs that are both heads
(def ^{:doc "Count items matching a filter"}
  count-if (comp count filter)) ; `comp` is compose

;; More general count-heads-pairs
(defn count-runs
  "Count runs of length n where pred is true in coll"
  [n pred coll]
  (count-if #(every? pred %)
            (partition n 1 coll)))

(count-runs 2 #(= % :h) tosses) ;; => 2
(count-runs 2 #(= % :t) tosses) ;; 1
(count-runs 3 #(= % :h) tosses) ;; 1

;; Implement `count-heads-pairs` in terms of `count-runs`
(def ^{:doc "Count runs of length 2 that are both heads"}
  count-heads-pairs-3 (partial count-runs 2 #(= % :h)))

(count-heads-pairs-3 tosses) ;; => 2
