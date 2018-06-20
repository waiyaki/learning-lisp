(ns picture-gallery.validation
  (:require [struct.core :as st]))

(defn registration-errors [params]
  (first
   (st/validate
    params
    {:id [st/required]
     :pass [st/required
            [st/min-count 7 :message "password must contain at least 8 characters"]
            [st/identical-to :pass-confirm :message "re-entered password does not match"]]})))
