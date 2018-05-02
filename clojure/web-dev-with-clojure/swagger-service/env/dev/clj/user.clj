(ns user
  (:require [swagger-service.config :refer [env]]
            [clojure.spec.alpha :as s]
            [expound.alpha :as expound]
            [mount.core :as mount]
            [swagger-service.figwheel :refer [start-fw stop-fw cljs]]
            [swagger-service.core :refer [start-app]]))

(alter-var-root #'s/*explain-out* (constantly expound/printer))

(defn start []
  (mount/start-without #'swagger-service.core/repl-server))

(defn stop []
  (mount/stop-except #'swagger-service.core/repl-server))

(defn restart []
  (stop)
  (start))


