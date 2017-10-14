(ns multimethods-protocols-records.data-psychology)

(defprotocol Psychodynamics
  "Plumb the inner depths of your types."
  (thoughts [x] "The data type's innermost thoughts.")
  (feelings-about [x] [x y] "Feelings about self or other."))

(extend-type java.lang.String
  Psychodynamics
  (thoughts [x] (str x " thinks, 'Truly, the character defines the type'"))
  (feelings-about
    ([x] (str x " is longing for a simpler way of life."))
    ([x y] (str x " is envious of " y "'s simpler way of life."))))
