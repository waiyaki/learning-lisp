(ns joy.ch3.core
  (:require [book-report.core :refer [lesson]]))

(lesson 3 "Dipping your toes in the pool"
  (title "3.1 Truthiness")

  (notes
    "Clojure has one Boolean context: the test expression of the `if` form."
    "Other forms that expect booleans are macros built on top of `if`"
    "Except for `false` and `nil`, every other value is logical `true`"
    "Java provides ability to create object that looks like, but isn't actually, `false`")
  (def evil-false (Boolean. "false"))
  evil-false

  (notes "Sometimes acts like `false`...")
  (= false evil-false)

  (notes "Sometimes acts like `true` 😱")
  (if evil-false :truthy :falsey)

  (notes
    "To parse a string, use the static `valueOf` method of the `Boolean` class
     instead of it's constructor")
  (Boolean/valueOf "false")

  (notes "Can use `nil?` and `false?` to distinguish between `nil` and `false`")

  (title "3.2 Nil pun with care")
  (notes
    "**_nil punning_** - Treating an empty sequence as nil."
    "Since empty colls act like `true` in Boolean contexts, you can test if
     there's actually anything in the collection using `seq`"
    "`seq` returns a seq on the collection, nil for empty colls")
  (seq [1 2 3])
  (seq [])

  (notes
    "Nil punning protects us from having to use a negated if check with `empty?`
     to determine termination conditions, e.g.")

  (defn print-seq [s]
    (when-not (empty? s)
      (prn (first s))
      (recur (rest s))))
  (print-seq [1 2 3])
  (print-seq [])

  (notes "We can rewrite the above as:")

  (defn print-seq [s]
    (when (seq s)
      (prn (first s))
      (recur (rest s))))
  (print-seq [1 2 3])
  (print-seq [])

  (title "3.3 Destructuring")
  (notes
    "Allows the positional binding of locals based on an expected form for some composite data structure."
    "Simplest form of destructuring, picking apart a vector, list (or any other sequential coll):")

  (def guys-whole-name ["Guy" "Lewis" "Steele"])

  (let [[f-name m-name l-name] guys-whole-name]
    (str l-name ", " f-name " " m-name))

  (let [[a b c & more :as all] (range 5)]
    (println "a b c are: " a b c)
    (println "more is: " more)
    (println "all is: " all))

  (notes
    "Positional destructuring doesn't work on maps and sets since they're
     not logically aligned sequentially"
    "`:as` must be placed after `&` if there is one.")

  (def guys-name-map {:f-name "Guy" :m-name "Lewis" :l-name "Steele"})

  (notes "Destructuring with a map, either:")

  (let [{f-name :f-name m-name :m-name l-name :l-name} guys-name-map]
    (str l-name ", " f-name " " m-name))

  (notes "or can use `:keys` to reduce the repetition...")

  (let [{:keys [f-name m-name l-name]} guys-name-map]
    (str l-name ", " f-name " " m-name))

  (notes
    "Using `:keys` indicates the next vector of names should be converted into
     keywords in order to look up their values in the map."
    "Using `:strs`, items in the map would be looked up using string keys, e.g \"f-name\""
    "Using `:syms`, items in the map are looked up using symbol keys."
    "These directives and regular named bindings can appear in any order and combination.")

  (notes "To destructure and still get the entire map, use `:as`:")

  (let [{f-name :f-name :as whole-name} guys-name-map]
    (println "First name is " f-name)
    (println "Whole name is:")
    whole-name)

  (notes
    "While destructuring, missing keys are bound to `nil`"
    "Can destructure while providing default values for missing values:")

  (let [{:keys [title f-name m-name l-name]
         :or   {title "Mr."}} guys-name-map]
    (println title f-name m-name l-name))

  (notes
    "All of map destructuring features work on lists, feature used primarily by functions to accept kwargs")

  (defn whole-name [& args]
    (let [{:keys [f-name m-name l-name]} args]
      (str l-name ", " f-name " " m-name)))

  (whole-name :f-name "Guy" :m-name "Lewis" :l-name "Steele")

  (notes
    "Notice how `whole-name` isn't called with a map, but rather with arguments
     alternating between keys and values")

  (notes "Can leverage associative destructuring to take apart vectors by their indices:")
  (let [{first-thing 0, last-thing 3} [1 2 3 4]]
    [first-thing last-thing])

  (notes "All of these destructuring features are available in function parameters.")

  (defn print-last-name [{:keys [l-name]}]
    (println l-name))
  (print-last-name guys-name-map))
