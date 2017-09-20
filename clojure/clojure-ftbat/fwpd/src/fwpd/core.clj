(ns fwpd.core)

(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns."
  [string]
  (map #(clojure.string/split % #",")
       (clojure.string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  "Filter out records with :glitter-index less than minimum-glitter."
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn extract-names
  [records]
  (map :name records))

(def not-empty (complement empty?))

(def validators
  "Validate a suspect record."
  {:name not-empty
   :glitter-index #(>= (str->int %) 0)})

(defn validate
  [validator-keys record]
  (reduce (fn [a [key value]]
            (and a
                 ((get validators key) value)))
          true
          record))

(defn append-
  "Append x to seq."
  [x seq]
  (reverse (cons x (reverse seq))))

(defn append
  [records record]
  (if (validate [:name :glitter-index] record)
    (append- record records)
    nil))

(defn to-csv
  [{:keys [name glitter-index]}]
  (clojure.string/join "," [name glitter-index]))

(defn csvify
  [records]
  (reduce #(clojure.string/join "\n" [%1 %2])
          (map to-csv records)))
