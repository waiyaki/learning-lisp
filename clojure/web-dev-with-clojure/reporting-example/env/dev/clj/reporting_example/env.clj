(ns reporting-example.env
  (:require [selmer.parser :as parser]
            [clojure.tools.logging :as log]
            [reporting-example.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init
   (fn []
     (parser/cache-off!)
     (log/info "\n-=[reporting-example started successfully using the development profile]=-"))
   :stop
   (fn []
     (log/info "\n-=[reporting-example has shut down successfully]=-"))
   :middleware wrap-dev})
