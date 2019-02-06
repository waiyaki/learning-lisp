(ns cljapplied.ch8.fixture
  (:use clojure.test))

;;; Without fixtures
(deftest test-setup-db-add-user
  ;; setup
  (let [conn (create-db-conn)]
    (load-test-data conn)

    ;; test logic
    (add-user conn "user")
    (check-user conn "user")

    ;; tear down
    (destroy-test-data conn)
    (close-db-conn conn)))


;;; With fixtures

;; Need a dynamic var since deftests don't take args
;; Dynamic vars are thread local, so can run tests in parallel
(def ^:dynamic *conn*)


(defn db-fixture [test-function]
  (binding [*conn* (create-db-conn)]
    (load-test-data *conn*)
    (test-function)
    (destroy-test-data *conn*)
    (close-db-conn *conn*)))


(use-fixtures :each db-fixture)


(deftest test-db-add-user
  (add-user *conn* "user")
  (check-user *conn* "user"))
