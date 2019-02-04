(ns cljapplied.ch4.shopping.store)


(declare sold-items)

(def inventory (atom {}))


(defn no-negative-values?
  "Check the values of a map for a negative value."
  [m]
  (not-any? neg? (vals m)))


(defn restock-order
  "A watch to restock an item"
  [k r ov nv] ;; watch-key, reference, old-value, new-value
  (doseq [item (for [kw (keys ov)
                     :when (not= (kw ov) (kw nv))] kw)]
    (swap! sold-items update-in [item] (fnil inc 0))
    (println "need to restock" item)))


(defn in-stock?
  "Check if an item is in stock"
  [item]
  (let [cnt (item @inventory)]
    (and (pos? cnt))))


(defn init
  "Set up store with inventory"
  [items]
  (set-validator! inventory no-negative-values?)
  (swap! inventory #(merge-with + %1 %2) items))


(defn init-with-restock
  "Set up store with inventory"
  [m]
  (def inventory (atom m))
  (def sold-items (atom {}))
  (set-validator! inventory no-negative-values?)
  (add-watch inventory :restock restock-order))


(defn grab
  "Grab an item from the shelves."
  [item]
  (if (in-stock? item)
    (swap! inventory update-in [item] dec)))


(defn stock
  "Stock an item on the shelves"
  [item]
  (swap! inventory update-in [item] inc))


(defn restock-all
  "Restock all items sold"
  []
  (swap! inventory #(merge-with + % @sold-items))
  (reset! sold-items {}))
