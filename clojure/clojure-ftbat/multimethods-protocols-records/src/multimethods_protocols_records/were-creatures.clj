(ns multimethods-protocols-records.were-creatures)

(defmulti full-moon-behaviour :were-type)

(defmethod full-moon-behaviour :wolf
  [were-creature]
  (str (:name were-creature) " will howl and murder."))

(defmethod full-moon-behaviour :simmons
  [were-creature]
  (str (:name were-creature) " will encourage people and sweat to the oldies."))

(defmethod full-moon-behaviour nil
  [were-creature]
  (str (:name were-creature) " will stay home and eat icecream."))

(defmethod full-moon-behaviour :default
  [were-creature]
  (str (:name were-creature) " will stay up all night fantasy footballing."))
