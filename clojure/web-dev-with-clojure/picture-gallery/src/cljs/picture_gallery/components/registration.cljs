(ns picture-gallery.components.registration
  (:require [ajax.core :as ajax]
            [picture-gallery.components.common :as c]
            [picture-gallery.validation :refer [registration-errors]]
            [reagent.core :refer [atom]]
            [reagent.session :as session]))

(defn register! [fields errors]
  (reset! errors (registration-errors @fields))
  (when-not @errors
    (ajax/POST
     "/register"
     {:params @fields
      :handler #(do (session/put! :identity (:id @fields))
                    (reset! fields {})
                    (-> (js/jQuery "#modal") (.modal "hide")))
      :error-handler #(reset! errors
                              {:server-error (get-in % [:response :message])})})))

(defn registration-form []
  (let [fields (atom {})
        error (atom nil)]
    (fn []
      [c/modal
       [:div "Picture Gallery registration"]
       [:div
        [:div.container
         [:strong "* required field"]]
        [c/text-input "name" :id "enter a username" fields]
        [c/password-input "password" :pass "enter a password" fields]
        [c/password-input "password" :pass-confirm "re-enter the password" fields]
        (when-let [error (:server-error @error)]
          [:div.alert.alert-danger error])]
       [:div
        [:button.btn.btn-primary.mr-2
         {:on-click #(register! fields error)}
         "Register"]
        [:button.btn.btn-danger {:data-dismiss "modal"} "Cancel"]]])))

(defn registration-button []
  [:a.btn {:data-toggle "modal" :data-target "#modal"} "Register"])
