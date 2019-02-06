(ns cljapplied.ch1.multimethods)

;; `defmulti` form defines the name and signature of the function as well as the
;; dispatch function..
;; Each `defmethod` provides a function implementation of a particular dispatch value.
;; Invoking the multimethod first invokes the dispatch function to produce a dispatch value,
;; then selects the best match for that value and finally invokes that function
;; implementation.

(defrecord Store [,,,])

(defrecord Ingredient
    [name      ;; string
     quantity  ;; amount
     unit      ;; keyword
     ])


(defn cost-of [store ingredient] ,,,)

(defmulti cost (fn [entity store] (class entity)))

(defmethod cost Recipe [recipe store]
  (reduce +$ zero-dollars
          (map #(cost % store) (:ingredients recipe))))

(defmethod cost Ingredient [ingredient store]
  (cost-of store ingredient))

;;; Using Protocols:
;; Protocols are also defined in two steps:
;; 1. The `defprotocol` form declares the name and series of function
;;    signatures (but no function implementations)
;; 2. `extend-protocol`, `extend-type` or `extend` is used to declare that a type
;;    extends a protocol.
(defprotocol Cost
  (cost [entity store]))

(extend-protocol Cost
  Recipe
  (cost [recipe store]
    (reduce +$ zero-dollars
            (map #(cost % store) (:ingredients recipe))))

  Ingredient
  (cost [ingredient store]
    (cost-of store ingredient)))

;; Protocols are preferred for type based dispatch because:
;; 1. Faster as they leverage underlying JVM runtime optimizations for type based dispatch
;; 2. Can group related functions together in a single protocol.
;;
;; Multimethods are preferred for value-based dispatch that provides greater flexibility.

;;; Value-Based Dispatch
(defmulti convert
  "Convert a quantity from unit1 to unit2, matching on [unit1 unit2]"
  (fn [unit1 unit2 quantity] [unit1 unit2]))

(defmethod convert
  ;; "lb -> oz"
  [:lb :oz] [_ _ lb] (* lb 16))

(defmethod convert
  ;; oz -> lb
  [:oz :lb] [_ _ oz] (/ oz 16))

(defmethod convert
  ;; Fallthrough
  :default [u1 u2 q]
  (if (= u1 u2)
    q
    (assert false (str "Unknown unit conversing from " u1 " to " u2))))

(defn ingredient+
  "Add two ingredients into a single ingredient, combining their quantities
  with a unit conversion if necessary."
  [{q1 :quantity u1 :unit :as i1} {q2 :quantity u2 :unit}]
  (assoc i1 :quantity (+ q1 (convert u2 u1 q2))))



;; Protocols cannot extend protocols, but adapted protocols for concrete types
;; can be installed dynamically at runtime.
(defprotocol TaxedCost
  (taxed-cost [entity store]))

(extend-protocol TaxedCost
  Object  ;; Default fallthrough
  (taxed-cost [entity store]
    (if (satisfies? Cost entity)
      (do (extend-protocol TaxedCost
            (class entity)
            (taxed-cost [entity store]
              (* (cost entity store) (+ 1 (tax-rate store)))))
          (taxed-cost entity store))
      (assert false (str "Unhandled entity: " entity)))))

;; In the above example, if a type isn't extended to the TaxedCost protocol but
;; it _is_ extended to the Cost protocol, an extension for the concrete type is
;; dynamically installed and once installed, the same call is made again (line 89),
;; which is now rerouted to the just-installed implementation.
;; Note this only happens in the first call with an unknown entity type.
