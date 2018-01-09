(require '[clojure.spec.alpha :as s])

;; Specs describing an ingredient
(s/def ::ingredient (s/keys :req [::name ::quantity ::unit]))
(s/def ::name       string?)
(s/def ::quantity   number?)
(s/def ::unit       keyword?)

;; Function spec for scale-ingredient
(s/fdef scale-ingredient
        :args (s/cat :ingredient ::ingredient :factor number?)
        :ret ::ingredient)

(defn scale-ingredient [ingredient factor]
  (update ingredient :quantity * factor))

(every? #(s/valid? ::names %)
        [["James" "Muturi"]
         #{"James" "Muturi"}
         '("James" "Muturi")])
{:id #uuid "40e30dc1-55ac-33e1-85d3-1f1508140bfc"
 :artist "Rush"
 :title "Moving Pictures"
 :date #inst "1981-02-12"}
