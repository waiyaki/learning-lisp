(ns cljapplied.ch8.more
  (:use clojure.test))


(deftest test-range-group
  (testing "Testing (range endIndex)"
    (is (= '(0 1 2 3 4) (range 5))
        "Got 0-indexed sequenced when only end is specified")
    (is (= '() (range 0))
        "Got empty sequence when end index = 0")))


(deftest test-range-fail
  (testing "Testing (range endIndex)"
    (is (= '(0 1 2 3 4) (range 5))
        "Got 0-indexed sequenced when only end is specified")
    (is (= '() (range 0))
        "Got empty sequence when end index = 0")
    (is (= '(0 1) (range 0)))))


(deftest test-range-are
  (testing "Testing range(endIndex)"
    (are [expected end-index]
        (= expected (range end-index))
      '(0 1 2 3 4) 5
      '() 0)))


(deftest test-range-exception
  (try
    ;; Using doall to force seq realization
    (doall (range "boom"))
    (is nil)
    (catch ClassCastException e
      (is true))
    (catch Throwable t
      (is nil))))


(deftest test-range-exception-thrown
  (is (thrown? ClassCastException
               (doall (range "boom")))))


(deftest test-range-exception-msg
  (is (thrown-with-msg? ClassCastException
                        #"java.lang.String cannot be cast to java.lang.Number"
                        (doall (range "boom")))))
