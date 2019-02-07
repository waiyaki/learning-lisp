(ns social.main
  (:require [clojure.core.async :as async]
            [com.stuartsierra.component :as component]
            [social.components.feed :as feed]
            [social.components.kengine :as kengine]
            [social.components.approvals :as approvals]))


(defn system [{:keys [twitter facebook knowledge approvals] :as config}]
  (let [twitter-chan           (async/chan 100)
        twitter-response-chan  (async/chan 10)
        facebook-chan          (async/chan 100)
        facebook-response-chan (async/chan 10)
        alert-chan             (async/chan 100)
        response-chan          (async/chan 100)
        feed-chan              (async/merge [twitter-chan facebook-chan])
        response-pub           (async/pub response-chan :feed)]
    (async/sub response-pub :twitter twitter-response-chan)
    (async/sub response-pub :facebeook facebook-response-chan)

    (component/system-map
     :twitter (feed/new-feed twitter twitter-chan twitter-response-chan)
     :facebook (feed/new-feed facebook facebook-chan facebook-response-chan)
     :knowledge-engine (kengine/new-knowledge-engine knowledge feed-chan alert-chan)
     :approvals (component/using
                 (approvals/new-approvals approvals alert-chan response-chan)
                 [:knowledge-engine]))))
