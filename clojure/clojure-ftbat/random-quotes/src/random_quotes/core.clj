(ns random-quotes.core
  (:require [clojure.pprint :refer [pprint]])
  (:gen-class))

(defn merge-words-count
  "Merge words and increment their count if they already exist."
  [words new-words]
  (merge-with + words new-words))

(defn make-words-map
  "Convert a vector of words into a map of words with their respective count."
  [words]
  (reduce (fn [accumulated-words next-word]
            (merge-with +
                        accumulated-words
                        {(keyword (clojure.string/lower-case next-word)) 1}))
          {}
          words))

(defn quote->words
  "Return a vector of words in a quote, without newlines or the author name."
  [quote]
  (-> quote
      clojure.string/trim
      clojure.string/split-lines
      first
      (clojure.string/split #"\s+")))

(defn quote-word-count
  "Download n quotes and return a word: count map."
  [n]
  (let [urls (take n (repeat "https://www.braveclojure.com/random-quote"))
        futures-list (doall (map #(future (slurp %))
                                 urls))
        words (atom {})]
    (doall (map (fn [future-item]
                  (let [new-words (-> @future-item
                                      quote->words
                                      make-words-map)]
                    (swap! words #(merge-words-count % new-words))))
                futures-list))
    @words))

(defn -main
  [& args]
  (let [n (Integer/parseInt (get (vec args) 0 5))
        content (quote-word-count n)]
    (pprint content))
  (shutdown-agents))
