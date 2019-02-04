(ns cljapplied.ch2.search)

(def units [:lb :oz :kg])

;; Leveraging collection invocation style, invoking a set as a function
(some #{:oz} units)

;; Relatively efficient implementation of linear search that supports
;; search for logically false values and exits early.
(defn contains-val? [coll val]
  (reduce
   (fn [ret elem] (if (= val elem) (reduced true) ret))
   false
   coll))

(contains-val? [1 2 3 nil false] false)
(contains-val? [1 2 3 nil false] nil)
