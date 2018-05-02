(ns guestbook.routes.home
  (:require [guestbook.layout :as layout]
            [guestbook.db.core :as db]
            [ring.util.http-response :as response]
            [compojure.core :refer [GET defroutes]]))

(defn home-page []
  (layout/render "home.html"))

(defn about-page []
  (layout/render "about.html"))

(defroutes home-routes
  (GET "/" [] (home-page))
  (GET "/messages" [] (response/ok (db/get-messages)))
  (GET "/about" [] (about-page)))
