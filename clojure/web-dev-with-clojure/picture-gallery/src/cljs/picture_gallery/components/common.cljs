(ns picture-gallery.components.common)

(defn modal [header body footer]
  [:div#modal.modal.fade
   [:div.modal-dialog.modal-dialog-centered
    [:div.modal-content
     [:div.modal-header [:h3 header]]
     [:div.modal-body body]
     [:div.modal-footer footer]]]])

(defn input [type id placeholder fields]
  [:input.form-control.input-lg
   {:type type
    :placeholder placeholder
    :value (id @fields)
    :on-change #(swap! fields assoc id (-> % .-target .-value))}])

(defn form-input [type label id placeholder fields optional?]
  [:div.form-group
   [:label label]
   (if optional?
     [input type id placeholder fields]
     [:div.input-group
      [input type id placeholder fields]
      [:div.input-group-append
       [:span.input-group-text "*"]]])])

(defn text-input [label id placeholder fields & [optional?]]
  (form-input :text label id placeholder fields optional?))

(defn password-input [label id placeholder fields & [optional?]]
  (form-input :password label id placeholder fields optional?))
