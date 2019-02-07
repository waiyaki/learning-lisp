(ns social.components.kengine
  (:require [com.stuartsierra.component :as component]))

(declare watch-feeds)

(defrecord KnowledgeEngine
    [ke-config feed-chan alert-chan rules]
  component/Lifecycle
  (start [component]
    (watch-feeds feed-chan alert-chan)
    component)
  (stop [component]
    (component)))


(defn new-knowledge-engine
  "Create a new knowledge engine with no initial rules"
  [ke-config feed-chan alert-chan]
  (->KnowledgeEngine ke-config feed-chan alert-chan
                     (atom (:rule-set ke-config))))


(defn add-rule
  "Add rule to set"
  [ke rule]
  (swap! (:rules ke) conj rule))
