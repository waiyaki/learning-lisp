(ns social.components.approvals
  (:require [com.stuartsierra.component :as component]))

(declare process-alerts process-responses)

(defrecord Approvals [approvals-config
                      alert-chan
                      knowledge-engine
                      response-chan]
  component/Lifecycle
  (start [component]
    (process-alerts alert-chan)
    (process-responses knowledge-engine response-chan)
    component)
  (stop [component]
    component))

(defn new-approvals [approval-config alert-chan response-chan]
  (map->Approvals {:approval-config approval-config
                   :alert-chan alert-chan
                   :response-chan response-chan}))
