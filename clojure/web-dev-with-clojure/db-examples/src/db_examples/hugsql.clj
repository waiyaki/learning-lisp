(ns db-examples.hugsql
  (:require [db-examples.core :refer [db]]
            [clojure.java.jdbc :as sql]
            [hugsql.core :as hugsql]))

(hugsql/def-db-fns "users.sql")
