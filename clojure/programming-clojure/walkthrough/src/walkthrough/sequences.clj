(import 'java.io.File)
(require '[clojure.java.io :refer [reader]])
(require '[clojure.string :refer [blank?]])

;; Show files that have been modified recently
(defn minutes-to-millis [mins] (* mins 1000 60))

(defn recently-modified? [file]
  (> (.lastModified file)
     (- (System/currentTimeMillis) (minutes-to-millis 30))))

(filter recently-modified? (file-seq (File. ".")))

;; In Java, Reader provides a stream of characters.
;; You can use `line-seq` to seq over the lines of any Java Reader
;; Readers can represent nonmemory resources that need be closed,
;; so should be wrapped in a `with-open`
(with-open [rdr (reader "src/walkthrough/sequences.clj")]
  (count (line-seq rdr)))

(with-open [rdr (reader "src/walkthrough/sequences.clj")]
  (count (filter #(re-find #"\S" %) (line-seq rdr))))


(defn non-blank? [line] (not (blank? line)))

(defn non-svn? [file] (not (.contains (.toString file) ".svn")))

(defn clojure-source? [file] (.endsWith (.toString file) ".clj"))

(defn clojure-loc [base-file]
  (reduce
   +
   (for [file (file-seq base-file)
         :when (and (clojure-source? file) (non-svn? file))]
     (with-open [rdr (reader file)]
       (count (filter non-blank? (line-seq rdr)))))))

;;;; Functions on maps
(def song {:name "Agnus Dei"
           :artist "Krzysztof Penderecki"
           :album "Polish Requiem"
           :genre "Classical"})

(assoc song :kind "MPEG Audio File")

(dissoc song :genre)

(select-keys song [:name :artist])

(merge song {:size 8118923 :time 502323})

(merge-with concat
            {:rubble ["Barney"], :flintstone ["Fred"]}
            {:rubble ["Betty"], :flintstone ["Wilma"]}
            {:rubble ["Bam-Bam"], :flintstone ["Pebbles"]})
;; => {:rubble ("Barney" "Betty" "Bam-Bam"), :flintstone ("Fred" "Wilma" "Pebbles")}


;;;; Functions on Sets
(require '[clojure.set :refer :all])

(def languages #{"java" "c" "d" "clojure"})

(def beverages #{"java" "chai" "pop"})

(union languages beverages) ;; => #{"d" "clojure" "pop" "java" "chai" "c"}

(difference languages beverages) ;; => #{"d" "clojure" "c"}

(intersection languages beverages) ;; => #{"java"}

(select #(= 1 (count %)) languages) ;; => #{"d" "c"}

;; Sets and relational algebra
(def compositions
  #{{:name "The Art of the Fugue" :composer "J. S. Bach"}
    {:name "Musical Offering" :composer "J. S. Bach"}
    {:name "Requiem" :composer "Giuseppe Verdi"}
    {:name "Requiem" :composer "W. A. Mozart"}})

(def composers
  #{{:composer "J. S. Bach" :country "Germany"}
    {:composer "W. A. Mozart" :country "Austria"}
    {:composer "Giuseppe Verdi" :country "Italy"}})

(def nations
  #{{:nation "Germany" :language "German"}
    {:nation "Austria" :language "German"}
    {:nation "Italy" :language "Italian"}})

;;; `rename` renames keys (database columns) based on an original names => new names map
;; Rename compositions to use `title` instead of `name`
(rename compositions {:name :title})

;;; `select` returns a map for which a predicate is true. Analogous to WHERE in SQL
;; Find all compositions whose title is "Requiem"
(select #(= (:name %) "Requiem") compositions)

;;; `project` returns only the parts of maps that match a set of keys
;;; Analogous to SQL SELECT with specified subset of columns
;; Projection that returns only the name of the compositions
(project compositions [:name])
;; => #{{:name "The Art of the Fugue"} {:name "Musical Offering"} {:name "Requiem"}}

;;; Join sets
(for [m compositions c composers] (concat m c))

;;; `join` sets based on shared keys:
;; join composition names and composers on shared key `composer`
(join compositions composers)

;; Specify relationship with a keymap
(join composers nations {:country :nation})


;;; Combination of the relational primitives
;; Find the set of all countries that are home to the composer of a requiem
(project
 (join (select #(= (:name %) "Requiem") compositions)
       composers)
 [:country]) ;; => #{{:country "Italy"} {:country "Austria"}}
