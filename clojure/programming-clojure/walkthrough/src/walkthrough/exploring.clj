(require '[clojure.string :as str])

;;;; Anonymous functions
(defn indexable-word?
  "True if a word has more than 2 characters."
  [word]
  (> (count word) 2))

(filter indexable-word? (str/split "A fine day it is" #"\W+"))

;; with anonymous function
(filter (fn [w] (> (count w) 2)) (str/split "A fine day it is" #"\W+"))

;; with macro syntax for anonymous functions using implicit parameter names
(filter #(> (count %) 2) (str/split "A fine day it is" #"\W+"))

(defn indexable-words
  "Return a list containing words with more than 2 characters from text"
  [text]
  (let [indexable-word? (fn [w] (> (count w) 2))]
    (filter indexable-word? (str/split text #"\W+"))))

(indexable-words "A fine day it is.")

(defn make-greeter [greeting-prefix]
  (fn [username] (str greeting-prefix ", " username)))

(def hello-greeting (make-greeter "Hello"))
(hello-greeting "world")

(def aloha-greeting (make-greeter "Aloha"))
(aloha-greeting "world")

((make-greeter "Howdy") "partner")

;;;; Vars, Bindings, Namespaces
;; namespace - a collection of names (symbols) that refer to vars
;; vars are bound to values
;; root-binding - initial value of a var
;; Objects defined with def or defn are stored in Clojure vars.
;; The special form `var` returns the var itself, not the value bound to the var
(def foo 10)
(var foo)

;; The `var` special form has the reader macro `#'`
#'foo ;; => 'walkthrough.core/foo

;;; Destructuring
;; Use maps to destructure any associative collections
(defn greet-author [{fname :first-name}]
  (println "Hello, " fname))
(greet-author {:last-name "Vinge" :first-name "Vernor"}) ;; ==> "Hello, Vernor"

;; Use vectors to destructure any sequential collection
(let [[x y] [ 1 2 3]]
  [x y]) ;; => [1 2]

;; to skip over elements:
(let [[_ _ z] [1 2 3]]
  z) ;; => 3

;; You can bind elements within a collection and the entire collection
;; In a destructuring assignment, :as clause gives a binding for the entire coll
(let [[x y :as coords] [1 2 3 4 5]]
  [x y coords]) ;; => [1 2 [1 2 3 4 5]]

(defn ellipsize
  "Return the first 3 words followed by an ellipse (...)"
  [words]
  (let [[w1 w2 w3] (str/split words #"\s+")]
    (str/join " " [w1 w2 w3 "..."])))

(ellipsize "The quick brown fox jumped over the lazy dog.") ;; =>  "The quick brown ..."


(defn indexed
  "Return a sequence of pairs of the form [idx elt] containing an element and it's index"
  [coll]
  (map-indexed vector coll))

(indexed "abcde") ;; => ([0 \a] [1 \b] [2 \c] [3 \d] [4 \e])

(defn index-filter
  "Like filter, but return the indices instead of the matches themselves"
  [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))

;; Clojure sets are functions that test membership...
(index-filter #{\a \b} "abbcddb") ;; => (0 1 2 6)

(defn index-of-any [pred coll]
  (first (index-filter pred coll)))

(index-of-any #{\z \a} "zzabs") ;; => 0
(index-of-any #{\a \b} "xyz") ;; => nil
