(ns cljapplied.ch4.shopping.family-async
  (:require [cljapplied.ch4.shopping.store :as store]
            [clojure.core.async :refer [>!! <! go-loop chan go]]))


(def shopping-list (ref #{}
                        :validator #(not (contains? % :candy)))) ;; Set of items to shop for

(def assignments (ref {})) ;; Track in-flight items assigned to each child

(def shopping-cart (ref #{}))

(def my-kids #{:alice :bobbi :cindy :donnie})


(defn notify-parent
  [k r _ nv]
  (if (contains? nv :candy)
    (println "There's candy in the cart!")))

(add-watch shopping-cart :candy notify-parent)

(defn init []
  (store/init {:eggs 2 :bacon 3 :apples 3
               :candy 5 :soda 2 :milk 1
               :bread 3 :carrots 1 :potatoes 1
               :cheese 3})
  (dosync
   (ref-set shopping-list #{:milk :butter :bacon :eggs
                            :carrots :potatoes :cheese :apples})
   (ref-set assignments {})
   (ref-set shopping-cart #{})))


(defn assignment
  [child]
  (get @assignments child))


(defn buy-candy []
  (dosync
   (commute shopping-cart conj (store/grab :candy))))


(defn collect-assignment
  "Add shopped-for item to the shopping cart and discard
  the child's assignment"
  [child]
  (let [item (assignment child)]
    (dosync
     (alter shopping-cart conj item)
     (alter assignments dissoc child)
     (ensure shopping-list) ;; not necessary, illustrative
     )
    item))


(defn assign-item-to-child [child]
  (let [item (first @shopping-list)]
    (dosync
     (alter assignments assoc child item)
     (alter shopping-list disj item))
    item))


(defn maybe? [f]
  (when (< 0.5 (rand))
    (f)))


(defn dawdle
  []
  (let [t (rand-int 5000)]
    (Thread/sleep t)
    (maybe? buy-candy)))


(defn send-child-for-item
  "Eventually shop for an item"
  [child item q]
  (println child "is searching for" item)
  (dawdle)
  (collect-assignment child)
  (>!! q child))


(defn report []
  (println "store inventory" @store/inventory)
  (println "shopping-list" @shopping-list)
  (println "assignments" @assignments)
  (println "shopping-cart" @shopping-cart))


(defn go-shopping []
  (init)
  (report)
  (let [kids (chan 10)]
    (doseq [k my-kids]
      (>!! kids k))
    (go-loop [kid (<! kids)]
      (if (seq @shopping-list)
        (do
          (go
            (send-child-for-item kid (assign-item-to-child kid) kids))
          (recur (<! kids)))
        (do
          (println "done shopping")
          (report))))))
