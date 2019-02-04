(ns cljapplied.ch1.apollo)

(def mission-defaults {:orbits 0 :evas 0})

(defn make-mission [name system launched manned? opts]
  (let [{:keys [cm-name
                lm-name
                orbits
                evas]} (merge mission-defaults opts)]
    ,,,))
