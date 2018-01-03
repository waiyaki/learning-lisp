# Assorted Notes
## Unifying Data with Sequences
- A Clojure sequence (_seq_ &mdash; pronounced as "seek") is a logical list (not concrete)

### Using the Sequence Library
- Sets act as functions that loop up a value in the set an either return the
element found or `nil` if not found.
- `complement` reverses the behaviour of another function

```clojure
(def vowel? #{\a\e\i\o\u})

(def consonant? (complement vowel?))
```

- Filter functions take a predicate and return a sequence

 #### Sequence predicates
 - Asks how some other predicate applies to every item in the sequence, e.g. the
   `every?` predicate asks whether some other predicate is true for every seq element

   ```clojure
   (every? odd? [1 3 5]) ;; => true
   ```

#### Functions on maps
- `assoc` - returns a new map with the key/value pair added
- `dissoc` - returns a map with a key removed
- `select-keys` - returns a map, keeping only a specified set of keys
- `merge` - combines maps. If multiple maps contain a key, the rightmost map wins.
- `merge-with` - like merge, but takes a function to merge maps with when they have the same key.


-----
- `declare` macro lets you create vars with no initial binding in a single line of code.

``` clojure
(declare my-odd? my-even?)

(defn my-odd? [n]
  (if (= n 0)
    false
    (my-even? (dec n))))

(defn my-even? [n]
  (if (= n 0)
    true
    (my-odd? (dec n))))
```
