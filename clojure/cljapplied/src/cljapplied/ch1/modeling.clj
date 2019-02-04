(ns ch1.modeling)

(def earth {:name "Earth"
            :moons 1
            :volume 1.08321e12
            :mass 5.97219e24
            :aphelion 152098232
            :perihelion 147098290
            :type :Planet})

(defrecord Planet [name moons volume mass aphelion perihelion])

(def earth (map->Planet earth))
