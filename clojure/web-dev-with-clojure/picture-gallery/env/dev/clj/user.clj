(ns user
  (:require [picture-gallery.config :refer [env]]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [mount.core :as mount]
            [picture-gallery.figwheel :refer [start-fw stop-fw cljs]]
            [picture-gallery.core :refer [start-app]]
            [picture-gallery.db.core]
            [conman.core :as conman]
            [luminus-migrations.core :as migrations]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(defn start []
  (mount/start-without #'picture-gallery.core/repl-server))

(defn stop []
  (mount/stop-except #'picture-gallery.core/repl-server))

(defn restart []
  (stop)
  (start))

(defn restart-db []
  (mount/stop #'picture-gallery.db.core/*db*)
  (mount/start #'picture-gallery.db.core/*db*)
  (binding [*ns* 'picture-gallery.db.core]
    (conman/bind-connection picture-gallery.db.core/*db* "sql/queries.sql")))

(defn reset-db []
  (migrations/migrate ["reset"] (select-keys env [:database-url])))

(defn migrate []
  (migrations/migrate ["migrate"] (select-keys env [:database-url])))

(defn rollback []
  (migrations/migrate ["rollback"] (select-keys env [:database-url])))

(defn create-migration [name]
  (migrations/create name (select-keys env [:database-url])))


