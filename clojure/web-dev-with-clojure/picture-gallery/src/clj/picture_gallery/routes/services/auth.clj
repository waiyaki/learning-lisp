(ns picture-gallery.routes.services.auth
  (:require [buddy.hashers :as hashers]
            [clojure.tools.logging :as log]
            [picture-gallery.db.core :as db]
            [picture-gallery.validation :refer [registration-errors]]
            [ring.util.http-response :as response]))

(defn handle-registration-error [e]
  (if (-> e
          (.getCause)
          (.getNextException)
          (.getMessage)
          (.startsWith "ERROR: duplicate key value"))
    (response/precondition-failed
     {:result :error
      :message "user with selected ID already exists"})
    (do
      (log/error e)
      (response/internal-server-error
       {:result :error
        :message "server error occurred while adding the user"}))))

(defn register! [{:keys [session]} user]
  (if-let [errors (registration-errors user)]
    (response/precondition-failed {:result :error :error errors})
    (try
      (prn user)
      (db/create-user!
       (-> user
           (dissoc :pass-confirm)
           (update :pass hashers/encrypt)))
      (-> {:result :ok}
          (response/ok)
          (assoc :session (assoc session :identity (:id user))))
      (catch Exception e
        (handle-registration-error e)))))
