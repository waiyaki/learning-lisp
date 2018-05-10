(ns db-examples.core
  (:require [clojure.java.jdbc :as sql]))


(def db {:subprotocol "postgresql"
         :subname "//localhost/reporting"})

(defn create-users-table! []
  (sql/db-do-commands
   db
   (sql/create-table-ddl :users
                         [[:id "varchar(32) PRIMARY KEY"]
                          [:pass "varchar(100)"]])))

(defn get-user [id]
  (first (sql/query db ["select * from users where id = ?" id])))

(defn add-user! [user]
  (sql/insert! db :users user))

(defn add-users! [users]
  (sql/insert-multi! db :users users))

(defn set-pass! [id pass]
  (sql/update! db
               :users
               {:pass pass}
               ["id=?" id]))

(defn remove-user! [id]
  (sql/delete! db :users ["id=?" id]))

(defn add-user-transaction [user]
  (sql/with-db-transaction [t-conn db]
    (if-not (find-user t-conn {:id (:id user)})
      (add-user! t-conn user))))
