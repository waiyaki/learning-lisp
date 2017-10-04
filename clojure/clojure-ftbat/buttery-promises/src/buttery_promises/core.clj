(ns buttery-promises.core
  (:gen-class))

(def yak-butter-international
  {:store "Yak Butter International"
   :price 90
   :smoothness 90})

(def butter-than-nothing
  {:store "Butter Than Nothing"
   :price 150
   :smoothness 83})

(def baby-got-yak
  {:store "Baby Got Yak"
   :price 94
   :smoothness 99})

(defn mock-api-call
  [result]
  (Thread/sleep 1000)
  result)

(defn satisfactory?
  "If the butter meets the requirements, return true, else return false"
  [butter]
  (and (<= (:price butter) 100)
       (>= (:smoothness butter) 97)
       butter))

(defn go-synchronous
  []
  (println "go-synchronous:")
  (println "And the winner is: "
           (time (some (comp satisfactory? mock-api-call)
                       [yak-butter-international butter-than-nothing baby-got-yak]))))

(defn go-asynchronous
  []
  (println "go-asynchronous:")
  (time (let [butter-promise (promise)]
          (doseq [butter [yak-butter-international butter-than-nothing baby-got-yak]]
            (future (if-let [satisfactory-butter (satisfactory? (mock-api-call butter))]
                      (deliver butter-promise satisfactory-butter))))
          (println "And the winner is: " @butter-promise))))

(defn -main
  "I don't do a whole lot ... yet."
  []
  (go-synchronous)
  (go-asynchronous))
