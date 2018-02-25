;; Reimplement Clojure's .. macro
(defmacro chain [x form]
  (list '. x form))
