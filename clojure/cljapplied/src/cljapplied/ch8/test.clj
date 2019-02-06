(ns cljapplied.ch8.test
  (:use clojure.test))


(deftest test-range
  (is (= '(0 1 2 3 4) (range 5))
      "Got 0-indexed sequence when only end is specified")
  (is (= '() (range 0))
      "Got empty sequence when end index = 0"))
