(ns swagger-service.routes.services
  (:require [ring.util.http-response :refer :all]
            [compojure.api.sweet :refer :all]
            [schema.core :as s]
            [clojure.java.io :as io]
            [clojure.xml :as xml]
            [clj-http.client :as client]))

(defn parse-xml [xml]
  (-> xml .getBytes io/input-stream xml/parse))

(defn get-first-child [tag xml-node]
  (->> xml-node :content (filter #(= (:tag %) tag)) first))

(defn parse-link [link]
  (->> link (get-first-child :url) :content first))

(defn parse-links [links]
  (->> links
       (get-first-child :data)
       (get-first-child :images)
       :content
       (map parse-link)))

(defn get-links [link-count]
  (-> "http://thecatapi.com/api/images/get?format=xml&results_per_page="
      (str link-count)
      client/get
      :body
      parse-xml
      parse-links))


(defapi service-routes
  {:swagger {:ui "/swagger-ui"
             :spec "/swagger.json"
             :data {:info {:version "1.0.0"
                           :title "Cat Link API"
                           :description "Cats API"}
                    :tags [{:name "thecatapi" :description "cat's api"}]}}}

  (context "/api" []
    :tags ["thecatapi"]

    (GET "/cat-links" []
      :query-params [link-count :- Long]
      :summary "returns a collection of cat image links"
      :return [s/Str]
      (ok (get-links link-count)))))
