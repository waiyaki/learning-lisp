# Assorted Notes

## Unifying Data with Sequences

* A Clojure sequence (_seq_ &mdash; pronounced as "seek") is a logical list (not concrete)

### Using the Sequence Library

* Sets act as functions that loop up a value in the set an either return the
  element found or `nil` if not found.
* `complement` reverses the behaviour of another function

```clojure
(def vowel? #{\a\e\i\o\u})

(def consonant? (complement vowel?))
```

* Filter functions take a predicate and return a sequence

#### Sequence predicates

* Asks how some other predicate applies to every item in the sequence, e.g. the
  `every?` predicate asks whether some other predicate is true for every seq element

  ```clojure
  (every? odd? [1 3 5]) ;; => true
  ```

#### Functions on maps

* `assoc` - returns a new map with the key/value pair added
* `dissoc` - returns a map with a key removed
* `select-keys` - returns a map, keeping only a specified set of keys
* `merge` - combines maps. If multiple maps contain a key, the rightmost map wins.
* `merge-with` - like merge, but takes a function to merge maps with when they have the same key.

---

* `declare` macro lets you create vars with no initial binding in a single line of code.

```clojure
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

## Specs

* Specs are logical combinations of predicates used to describe a set of data values.
* From (as of Clojure 1.9.0) `clojure.spec.alpha`. Commonly aliased to `s`

```clojure
(require '[clojure.spec.alpha :as s])
```

* `s/def` macro names and registers a spec in the global registry of specs accessible in current Clojure program.
  Spec names should be fully qualified keywords, e.g `:cognitect.transit.handler/name`.
  Can use auto-resolved keywords (start with `::`) to make typing these easier.
  When no qualifier is specified (e.g. `::ingredient`), the current ns is used as the qualifier.
  When a qualifier is specified (e.g. `::recipe/ingredient`), it can refer to an alias defined in the current ns.

### Validating Data

#### Predicates

Predicate functions are valid specs.

```clojure
(s/def :my.app/company-name string?)

(s/valid? :my.app/company-name "Andela") ;; => true
(s/valid? :my.app/company-name 23) ;; => false
```

#### Enumerated Values

Sets implement the function interface and are valid specs which can be used to spec enumerated values.

```clojure
(s/def ::marble/color #{:red :green :blue})

(s/valid? ::marble/color :red) ;; => true
(s/valid? ::marble/color ::pink) ;; => false
```

#### Range Specs

Used to validate a range of values. Clojure spec has a range 😆 of range specs.

```clojure
(s/def ::bowling/ranged-roll (s/int-in 0 11))

(s/valid? ::bowling/ranged-roll 10) ;; => true
```

* `s/int-in` - beginning (inclusive) -> end (exclusive) integer values.
* `s/double=in` - for doubles.
* `s/inst-in` - for time instants.

#### Handling `nil`

* Can use the `s/nilable` spec to extend a spec for it to handle `nil` value.
  e.g., for a field accepting a string or `nil`:

  ```clojure
  (s/def ::my.app/company-name-2 (s/nilable string?))

  (s/valid? ::my.app/company-name-2 nil) ;; => true
  ```

  To spec out the values `true`, `false` and `nil`, use `s/nilable` to extend the `boolean?` spec.

  ```clojure
  (s/def ::nilable-boolean (s/nilable boolean?))
  ```

#### Logical Specs

Create composite specs from other specs using `s/and` and/or `s/or`.

* `s/and` to spec out an odd integer:

  ```clojure
  (s/def ::odd-int (s/and int? odd?)

  (s/valid? ::odd-int 5) ;; => true
  ```

* Can similarly use `s/or`

  ```clojure
  (s/def ::odd-or-42 (s/or :odd ::odd-int :42 #{42}))
  ```

  For `s/or`, each option in the `s/or` contains a keyword tag used to report how a value matches (or doesn't match) the spec.

* `s/conform` is used to know how a value matched a spec.
  Returns the value annotated with info about optional choices or the components of the value, called the **_conformed value_**.

  ```clojure
  (s/conform ::odd-or-42 42) ;; => [:42 42]

  (s/conform ::odd-or-42 19) ;; => [:odd 19]
  ```

* `s/explain` describe all the ways an invalid value didn't match the spec.

```clojure
(s/explain ::odd-or-42 0)
;; ==> val: 0 fails spec: :walkthrough.core/odd-int at: [:odd] predicate: odd?
;; ==> val: 0 fails spec: :walkthrough.core/odd-or-42 at: [:42] predicate: #{42}
;; => nil
```

#### Collection Specs

* Most common are `s/coll-of` and `s/map-of`.
* `s/coll-of` describes lists, vectors, sets and seqs.
  Provide a spec that members of the collection must satisfy and the spec will check all members.

  ```clojure
  (s/def ::names (s/coll-of string?))

  (every? #(s/valid? ::names %)
        [["James" "Muturi"]
         #{"James" "Muturi"}
         '("James" "Muturi")]) ;; => true
  ```

  * Has more additional options (lifting...)

    * `:kind` - a predicate checked at the beginning of the spec. Common examples are `vector?`, `set?`, etc.
    * `:into` - one of these literal collections: `[]`, `()`, or `#{}`. Conformed values collect into the specified collection.
    * `:count` - an exact count for the collection.
    * `:min-count` - a minimum count for the collection.
    * `:max-count` - a maximum count for the collection.
    * `:distinct` - true if the elements of the collection must be unique

    These allow for specifying many collection shapes and contraints.
    E.g., match only int ses with at least 2 items:

    ```clojure
    (s/def ::my-set (s/coll-of int? :kind set? :min-count 2))

    (s/valid? ::my-set #{10 20}) ;; => true
    ```

* `s/map-of` specs a lookup map where the keys and values each follow a spec.
  E.g. a mapping from player names to scores:

  ```clojure
  (s/def ::scores (s/map-of string? int?))

  (s/valid? ::scores {"Stu" 100 "Alex" 200})
  ```

  * All of `s/coll-of` options apply. `:into` and `:kind` default to map specific settings.
  * Conforms to a map and always conforms values. Keys aren't conformed by default but can be configured to using `:conform-keys` flag.
  * For large maps where conforming every value is not desired, can use `s/every` or `s/every-kv` sampling specs.

#### Collection Sampling

* `s/every` - for collections
* `s/every-kv` - for maps
* Similar to `s/coll-of` and `s/map-of`, but only check up to `s/*coll-check-limit*` elements (default 101)
* Better performance for large collections and maps.

#### Tuples

* Are vectors with known structure where each fixed element has its own spec.
  E.g. a vector of x, y coords representing a point.

  ```clojure
  (s/def ::point (s/tuple rational? rational?))
  (s/conform ::point [1.3 2.7]) ;; => [1.3 2.7]
  ```

  Tuples don't name or tag returned fields.

#### Information Maps

* In Clojure, domain objects are commonly represented as maps with well-known fields
  E.g., a release may look like

  ```clojure
  {::music/id #uuid "40e30dc1-55ac-33e1-85d3-1f1508140bfc"
   ::music/artist "Rush"
   ::music/title "Moving Pictures"
   ::music/date #inst "1981-02-12"}
  ```

* Can spec this out with `s/keys`, describing both required and optional keys.

  ```clojure
  (s/def ::music/release
      (s/keys :req [::music/id]
              :opt [::music/artist
                    ::music/title
                    ::music/date]))
  ```

  * `s/keys` will validate all registered keys, even if they're not listed as required or optional.
    This encourages uniform validation of attributes as the maps evolve over time.

* If the maps don't use qualified keys, can use option variants `:req-un` and `:opt-un` that find the value to check using the unqualified spec name.

  > So if the spec name is `::music/id`, it'll find the value using `id`.

  E.g. for the following map:

  ```clojure
  {:id #uuid "40e30dc1-55ac-33e1-85d3-1f1508140bfc"
  :artist "Rush"
  :title "Moving Pictures"
  :date #inst "1981-02-12"}
  ```

  can have the following spec:

  ```clojure
  (s/def ::music/release-unqualified
      (s/keys :req-un [::music/id]
              :opt-un [::music/artist
                      ::music/title
                      ::music/date]))
  ```

### Validating Functions

* Function specs have up to 3 specs:

  * an `args` spec for the arguments
  * a `ret` spec describing the return value.
  * a `fn` spec for relating the args to the return value.

* **_regex ops_** specs apply one or more specs to match values in a collection.
* `s/cat` specifies a concatenation (series of elements, in order), where each element is another spec.

  ```clojure
  (s/def ::cat-example (s/cat :s string? :i int?))

  (s/valid? ::cat-example ["abc" 100]) ;; => true
  ```

  Each component has a keyword tag naming the components in the conformed result.

  ```clojure
  (s/conform ::cat-example ["abc" 100]) ;; => {:s "abc" :i 100}
  ```

* `s/alt` indicates alternatives within the sequential structure. Conformed value is an entry with matched tag and value.
  ```clojure
  walkthrough.core> (s/def ::alt-example (s/alt :i int? :k keyword?))
  :walkthrough.core/alt-example
  walkthrough.core> (s/valid? ::alt-example [100])
  true
  walkthrough.core> (s/valid? ::alt-example [:foo])
  true
  walkthrough.core> (s/conform ::alt-example [:foo])
  [:k :foo]
  walkthrough.core> (s/conform ::alt-example [100])
  [:i 100]
  walkthrough.core>
  ```

#### Repetition Operators

* 3 repetition operators

  * `s/?` for 0 or 1
  * `s/*` for 0 or more
  * `s/+` for 1 or more

* Can be combined and nested arbitrarily, but all connected regex ops describe the structure of a single sequential collection.

E.g., for a collection containing one or more odd numbers and an optional trailing even number:

```clojure
(s/def ::oe (s/cat :odds (s/+ odd?) :even (s/? even?)))

(s/conform ::oe [1 3 5 100]) ;; => {:odds [1 3 5] :even 100}
```

* The described structure is a single top-level collection (doesn't describe nested collections like `[[1 3 5]]`

* Named regex ops allow factoring or regex ops in smaller reusable pieces:

```clojure
walkthrough.core> (s/def ::odds (s/+ odd?))
:walkthrough.core/odds
walkthrough.core> (s/def ::optional-even (s/? even?))
:walkthrough.core/optional-even
walkthrough.core> (s/def ::oe (s/cat :odds ::odds :even ::optional-even))
:walkthrough.core/oe
walkthrough.core> (s/conform ::oe [1 3 5 100])
{:odds [1 3 5], :even 100}
walkthrough.core>
```

#### Variable Arguments Lists

* Consider the spec of arguments the set `intersection` function (clojure.set/intersection):

```clojure
walkthrough.core> (s/def ::intersection-args
                    (s/cat :s1 set?
                           :sets (s/* set?)))
:walkthrough.core/intersection-args
walkthrough.core> (s/conform ::intersection-args '[#{1 2} #{2 3} #{2 5}])
{:s1 #{1 2}, :sets [#{3 2} #{2 5}]}
walkthrough.core>
```

* Args are conformed by passing them in as a vector.
* Keyword arguments can be spec'ed by treating them as maps and using `s/keys*` (identical structure to `s/keys`)

E.g. For Clojure `atom` with signature `(atom x & options)` with `options` `:meta` and `:validator`:

```clojure
walkthrough.core> (s/def ::meta map?)
:walkthrough.core/meta
walkthrough.core> (s/def ::validator ifn?)
:walkthrough.core/validator
walkthrough.core> (s/def ::atom-args
                    (s/cat :x any? :options (s/keys* :opt-un [::meta ::validator])))
:walkthrough.core/atom-args
walkthrough.core> (s/conform ::atom-args [100 :meta {:foo 1} :validator int?])
{:x 100,
 :options {:meta {:foo 1},
           :validator #function[clojure.core/int?]}}
```

#### Multi-arity Arguments Lists

* Consider spec'ing the `repeat` function, which has two arities, one with and one without the length `n`, which is the second argument.
* The spec can declare the first arg as optional:

```clojure
walkthrough.core> (s/def ::repeat-args
                    (s/cat :n (s/? int?) :x any?))
:walkthrough.core/repeat-args
walkthrough.core> (s/conform ::repeat-args [100 "foo"])
{:n 100, :x "foo"}
walkthrough.core> (s/conform ::repeat-args ["foo"])
{:x "foo"}
walkthrough.core>
```

* If the arites are sufficiently different, `s/alt` can be used to fully describe each arity.

### Specifying Functions

* Function specs are a combination of specs for:

  * arguments
  * return value
  * the `fn` spec describing the relationship between the arguments and the return value

* Consider specifying the `rand` function:
  * First create an arg spec that is either empty or takes an optional number:
    ```clojure
    (s/def ::rand-args (s/cat :n (s/? number?)))
    ```
  * Create a return value spec
    ```clojure
    (s/def ::rand-ret double?)
    ```
  * Then create the `:fn` spec, which receives the conformed args and return value as a map based on their values. In the case of `rand`, the random number must be be >= 0 and <= n.
    ```clojure
    (s/def ::rand-fn
      (fn [{:keys [args ret]}]
        (let [n (or (:n args) 1)]
          (cond (zero? n) (zero? ret)
                (pos? n) (and (>= ret 0) (< ret n))
                (neg? n) (and (<= ret 0) (> ret n))))))
    ```
  * Now tie these together using `s/fdef`, which takes a fully qualified function name and one or more of the specs:
    ```clojure
    (s/fdef clojure.core/rand
      :args ::rand-args
      :ret  ::rand-ret
      :fn   ::rand-fn)
    ```

#### Anonymous Functions

* Use `s/fspec` to define the spec of an anonymous function.
* Same syntax as `s/fdef` but omits the name.
  E.g., for the following `opposite` function that takes a predicate function and creates the opposite predicate function:

```clojure
(defn opposite [pred]
  (comp not pred))
```

Can be specified as:

```clojure
(s/def ::pred
  (s/fspec :args (s/cat :x any?)
           :ret boolean?))

(s/fdef opposite
  :args (s/cat :pred ::pred)
  :ret ::pred)
```

### Instrumenting Functions

* _Instrumentation_ is used in development and testing to wrap a function with a version that uses spec to verify that the incoming arguments to a function conform to the function's spec.
* It's for `:args` only.
* Call `stest/instrument` on a spec'ed fully qualified function to instrument it.

```clojure
(require '[clojure.spec.test.alpha :as stest])
(stest/instrument 'clojure.core/rand) ;; => [clojure.core/rand]
```

* Can also use `stest/enumerate-namespace` to enumerate a collection of all symbols in a namespace to pass to `stest/instrument`.

```clojure
(stest/instrument (stest/enumerate-namespace 'clojure.core)) ;; => [clojure.core/rand]
```

* `instrument` returns a collection of all successfully instrumented symbols.
* Instrumenting a function replaces its var with a new var that will check args and invoke the old function via its var.

### Generative Function Testing

* Generative testing produces thousands of random inputs, runs a procedure and verifies a set of properties for each output.
* Can use the `check` function from `clojure.spec.test.alpha` (commonly aliased as `stest`) for automated generative testing.
* All invocations of spec generators require the inclusion of the `test.check` library as a dependency.
* When `check` is invoked, it:
  * generates 1000 sets of arguments that are valid according to the function's `:arg` spec.
  * invokes the function with each arg and checks the return value according to the `:ret` spec.
  * checks that the `:fn` spec is valid for the arguments and return value.

```clojure
walkthrough.core> (require '[clojure.spec.alpha :as s])
nil
walkthrough.core> (require '[clojure.spec.test.alpha :as stest])
nil
walkthrough.core> (s/fdef clojure.core/symbol
                          :args (s/cat :ns (s/? string?) :name string?)
                          :ret symbol?
                          :fn (fn [{:keys [args ret]}]
                                (and (= (name ret) (:name args))
                                     (= (namespace ret) (:ns args)))))
clojure.core/symbol
walkthrough.core> (stest/check 'clojure.core/symbol)
({:spec #object[clojure.spec.alpha$fspec_impl$reify__2451 0x10e6c3de "clojure.spec.alpha$fspec_impl$reify__2451@10e6c3de"], :clojure.spec.test.check/ret {:result true, :num-tests 1000, :seed 1515464793187}, :sym clojure.core/symbol})
walkthrough.core> (pprint *1)
({:spec
  #object[clojure.spec.alpha$fspec_impl$reify__2451 0x10e6c3de "clojure.spec.alpha$fspec_impl$reify__2451@10e6c3de"],
  :clojure.spec.test.check/ret
  {:result true, :num-tests 1000, :seed 1515464793187},
  :sym clojure.core/symbol})
nil
walkthrough.core>
```

#### Generating Examples

* Every spec is both a validator and a data generator for values that match the spec.
* `s/exercise` produces pairs of examples and conformed value of those examples:

```clojure
walkthrough.core> (pprint (s/exercise (s/cat :ns (s/? string?) :name string?)))
([("" "") {:ns "", :name ""}]
 [("5") {:name "5"}]
 [("X" "") {:ns "X", :name ""}]
 [("" "b60") {:ns "", :name "b60"}]
 [("UrW" "Yd") {:ns "UrW", :name "Yd"}]
 [("5vF" "w36H3") {:ns "5vF", :name "w36H3"}]
 [("") {:name ""}]
 [("9QT" "") {:ns "9QT", :name ""}]
 [("747") {:name "747"}]
 [("szX") {:name "szX"}])
nil
walkthrough.core>
```

##### Combining generators with `s/and`

* Specs are commonly combined using `s/and`
* `s/and` will use the generator of its first component spec then filter the values by each subsequent component spec.
* Custom predicates don't have generator mappings.

So `s/exercise` doesn't work in:

```clojure
(defn big? [x] (> x 100))

(s/def ::big-odd (s/and odd? big?))

(s/exercise ::big-odd)
```

because the `odd?` predicate works on more than one numeric type and `big?` is a custom predicate.

* Can be fixed by adding an initial predicate with a mapped generator:

```clojure
(s/def ::big-odd-int (s/and int? odd? big?))

(take 3 (s/exercise ::big-odd-int)) ;; => ([165 165] [105 105] [169897 169897])
```

> **NOTE:** For `s/and` specs, only the first component spec's generator will be used.

##### Creating Custom Generators

* Some specs accept a custom generator option, e.g `s/coll-of`, `s/map-of`, `s/every`, `s/every-kv`, `s/keys`.
* Can explicitly add a replacement generator using `s/with-gen`.
* Can temporarily override a generator by name or path with generator overrides in some functions like `s/exercise`, `stest/instrument` `stest/check`.
  The overrides only take effect for the duration of the invocation.

E.g., replace the generator for `::marble-color` with one that only generates red marbles.

```clojure
walkthrough.core> (s/def ::marble-color #{:red :green :blue})
:walkthrough.core/marble-color
walkthrough.core> (s/exercise ::marble-color)
([:red :red] [:blue :blue] [:green :green] [:red :red] [:red :red] [:blue :blue] ...)
walkthrough.core> (s/def ::marble-color-red
                    (s/with-gen ::marble-color #(s/gen #{:red})))
:walkthrough.core/marble-color-red
walkthrough.core> (s/exercise ::marble-color-red)
([:red :red] [:red :red] [:red :red] [:red :red] ...)
walkthrough.core>
```

* `clojure.spec.gen.alpha` (commonly aliased as `gen`) has functions to generate all of Clojure types, collections, e.t.c.
* `gen/fmap` allows you to start from a source generator then modify each generator value by applying another function.
  E.g., to generate string starting with s standard string, generate a random suffix then concat the prefix.

```clojure
walkthrough.core> (require '[clojure.string :as str])
nil
walkthrough.core> (require '[clojure.spec.gen.alpha :as gen])
nil
walkthrough.core> (s/def ::sku
                    (s/with-gen (s/and string? #(str/starts-with? % "SKU-"))
                      (fn [] (gen/fmap #(str "SKU-" %) (s/gen string?)))))
:walkthrough.core/sku
walkthrough.core> (s/exercise ::sku)
(["SKU-" "SKU-"] ["SKU-P" "SKU-P"] ["SKU-" "SKU-"] ["SKU-R" "SKU-R"] ["SKU-" "SKU-"] ["SKU-k" "SKU-k"] ["SKU-E" "SKU-E"] ["SKU-X7EtbQT" "SKU-X7EtbQT"] ["SKU-ozD3" "SKU-ozD3"] ["SKU-9ZprY2D" "SKU-9ZprY2D"])
walkthrough.core>
```

# State and Concurrency
