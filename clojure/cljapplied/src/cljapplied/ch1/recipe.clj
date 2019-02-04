(ns cljapplied.ch1.recipe)
;;; Modelling relationships
;; Three main ways for an entity to refer to another entity:
;; - nesting
;; - identifiers
;; - stateful references.


(defrecord Recipe
    [name
     author       ;; Recipe creator
     description
     ingredients
     steps
     servings])

(defrecord Person
    [fname  ;; first name
     lname])  ;; last name


;; 1. Nesting
(def toast
  (->Recipe "Toast"
            (->Person "Alex" "Miller") ;; -> Nested reference
            "Crispy bread"
            ["Slice of bread"]
            ["Toast bread in toaster"]
            1))

;; 2. Identifiers - simple values (keyword, string, number) that refers to an
;;                  entity defined elsewhere.
(def people
  {:p1 (->Person "Alex" "Miller")})

(def recipes
  {:r1 (->Recipe
        "Toast"
        :p1       ;; Person id
        "Crispy bread"
        ["Slice of bread"]
        ["Toast bread in toaster"]
        1)})

;; 3. Stateful references.
;; - Use these when you want to refer to another entity and allow that
;;   relationship to change over time.
