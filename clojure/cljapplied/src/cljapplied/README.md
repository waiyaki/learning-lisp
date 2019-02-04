# Clojure Applied Workthrough

## Chapter 1.

### Domain Operations

- **Polymorphism** - Means of abstraction, allowing domain operations to be decoupled from the types to which it can be applied.

- Creation of generic domain operations in Clojure can be done using **_multimethods_** and **_protocols_**.

- **Dispatch** - Choosing the specific function to invoke for a generic operation.

- Multimethods and Protocols can dispatch based on argument type.
- Multimethods can also dispatch based on argument value.

- `defmulti` form defines the name and signature of the function as well as the dispatch function..
- Each `defmethod` provides a function implementation of a particular dispatch value.
- Invoking the multimethod first invokes the dispatch function to produce a dispatch value,
  then selects the best match for that value and finally invokes that function
  implementation.

```clojure
(defrecord Store [,,,])

(defn cost-of [store ingredient] ,,,)

(defmulti cost (fn [entity store] (class entity)))

(defmethod cost Recipe [recipe store]
  (reduce +$ zero-dollars
          (map #(cost % store) (:ingredients recipe))))

(defmethod cost Ingredient [ingredient store]
  (cost-of store ingredient))
```

### Using Protocols:

Protocols are also defined in two steps:

1. The `defprotocol` form declares the name and series of function
   signatures (but no function implementations)
2. `extend-protocol`, `extend-type` or `extend` is used to declare that a type
   extends a protocol.

```clj
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
```

Protocols are preferred for type based dispatch because:

1. Faster as they leverage underlying JVM runtime optimizations for type based dispatch
2. Can group related functions together in a single protocol.

Multimethods are preferred for value-based dispatch that provides greater flexibility.

### Value-Based Dispatch

```clj
(defmulti convert
  "Convert a quantity from unit1 to unit2, matching on [unit1 unit2]"
  (fn [unit1 unit2 quantity] [unit1 unit2]))

(defmethod convert
  "lb -> oz"
  [:lb :oz] [_ _ lb] (* lb 16))

(defmethod convert
  oz -> lb
  [:oz :lb] [_ _ oz] (/ oz 16))

(defmethod convert
  Fallthrough
  :default [u1 u2 q]
  (if (= u1 u2)
    q
    (assert false (str "Unknown unit conversing from " u1 " to " u2))))

(defn ingredient+
  "Add two ingredients into a single ingredient, combining their quantities
  with a unit conversion if necessary."
  [{q1 :quantity u1 :unit :as i1} {q2 :quantity u2 :unit}]
  (assoc i1 :quantity (+ q1 (convert u2 u1 q2))))
```

Protocols cannot extend protocols, but adapted protocols for concrete types
can be installed dynamically at runtime.

```clj
(defprotocol TaxedCost
  (taxed-cost [entity store]))

(extend-protocol TaxedCost
  Object  Default fallthrough
  (taxed-cost [entity store]
    (if (satisfies? Cost entity)
      (do (extend-protocol TaxedCost
            (class entity)
            (taxed-cost [entity store]
              (* (cost entity store) (+ 1 (tax-rate store)))))
          (taxed-cost entity store))
      (assert false (str "Unhandled entity: " entity)))))
```

In the above example, if a type isn't extended to the TaxedCost protocol but
it _is_ extended to the Cost protocol, an extension for the concrete type is
dynamically installed and once installed, the same call is made again (line 89),
which is now rerouted to the just-installed implementation.
Note this only happens in the first call with an unknown entity type.

## Chapter 2 - Collect and Organize Your Data

### Sequential Collections

- Sequential data - any ordered series of values.
- Two main concerns:

  - Where the data will be added and removed
  - Whether indexed access is needed.

- `conj` adds elements at the natural insertion point -- at the beginning for lists, at the end for vectors.

- Sequential collections retain their insertion order.

### Building Custom Collections

- Requires the use of `deftype` to implement the traits interfaces Clojure uses internally.
- Clojure collections are based on a generic set of traits that define key abstractions. These collection traits are implemented internally as Java interfaces.
- Predicate functions to detect the presence of the Clojure collection traits are provided:
  - `counted?` - Is collection countable in constnt time?
  - `sequential?` - Are the collection values stored in a particular traversable order?
  - `associative?` - Does the collection store an association of keys and values?
  - `reversible?` - Is the collection reversible?
  - `sorted?` - Is the collection maintained in a sorted order?
- These traits correspond to these Java interfaces: `Counted`, `Sequential`, `Associative`, `Reversible`, `Sorted`.

- The `deftype` macro looks similar to `defrecord` but provides more features and fewer built-in similarities to maps.
- E.g. Define a `Pair` type with two values, `a` and `b`. `Pair` should work with `seq`, `count`, `nth` and `get`.

```clj
(ns cljapplied.ch2.pair
  (import [clojure.lang Counted Indexed ILookup Seqable]
          [java.io Writer]))

(deftype Pair [a b]
  Seqable
  (seq [_] (seq [a b]))

  Counted
  (count [_] 2)

  Indexed
  (nth [_ i]
    (case i
      0 a
      1 b
      (throw (IllegalArgumentException.))))
  (nth [this i _] (nth this i))

  ILookup
  (valAt [_ k _]
    (case k
      0 a
      1 b
      (throw (IllegalArgumentException.))))
  (valAt [this k] (.valAt this k nil)))

(defmethod print-method Pair
  [pair ^Writer w]
  (.write w "#cljapplied.ch2.pair.Pair")
  (print-method (vec (seq pair)) w))

(defmethod print-dup Pair
  [pair w]
  (print-method pair w))
```

#### Custom printing for Types

- The printing apparatus can be hooked into using multimethods:
  - `print-method` - Called when printing is done for the user (e.g printing strings without their surrounding quotes)
  - `print-dup` - Called when printing is done for the reader.

The example above implements custom printing for Pair, making it print the same in either case, so `print-dup` just calls `print-method`. The syntax chosen was because the Clojure reader uses that form to construct a Java object (format: `#class[args]`)

## Chapter 3 - Processing Sequential Data

- Sequences are a generalization that allow working with lists, vectors and other collections as if they were sequential data structures.
- Most of the Clojure data transformation capabilities are built on top of this more general abstraction, instead of being tied to specific collections.

- Sequence functions expect as input a `seqable`.

  - `seqable` - Something that can yield a sequence when `seq` is applied to it and also return the same.

- `map` returns a persistent list. For vectors, `mapv` exists which takes in a vector and returns a vector.

### Transducers

- Transducer definition avoids specifying the input source and how the input is used.
- Instead defines only the actual work the transducer is doing.
- To create a map transducer, omit the input collection on the call to `map`:

```clj
(defn orbital-period-transformation
    "Create a map transformation for planet->orbital-period"
    [star]
    (map #(orbital-period %) (:mass star)))
```

To produce an output sequence similar to the non-transducer `map`, use the `sequence` function:

```clj
(defn orbital-periods
    [planets star]
    (sequence (orbital-period-transformation star) planets))
```

To create output vector like `mapv`, use `into`:

```clj
(defn orbital-periods
    [planets star]
    (into [] (orbital-period-transformation star) planets))
```

To produce a list, replace the `[]` in the call to `into` with `()` in the snippet above.

> Transducers are nothing more than a means of composing reducing functions together so that a single reduction can take place instead of many.


Processing sequential data follows a similar pattern as:
- Figure out the requirements (question(s) that the data should answer)
- Filter the data to remove unneeded elements (selection)
- Transform the elements into the desired form (mapping)
- Reduce the transformed elements into the answer.

> select -> transform -> reduce

## Chapter 4 - State, Identity, Change
- State is a value or a set of values belonging to an identity.
- Identity is a series of states, separated by time.
- In Clojure, references to identities with values that need to be updated are created.
- A `reference` is a mutable container for a succession of immutable values.
- In Clojure, this approach of managing values that change over time is called the **_unified update model_**. The unified update model generally looks like:

``` clj
(update-fn container data-fn & args)
```

The `data-fn` is applied to the currency stored value in the reference, creating a new value which replaces the current value held in the reference. The reference types, `var`, `atom`, `agent` and `ref` use this model to implement successive values.

#### Atomic succession
- Updates a single identity.
- Given an update function and an identity, the update function is applied to the current value of the identity to produce a new value, which then replaces the current value to become the new current value. If multiple threads try to update the same identity's value at the same time, Clojure handles this by retrying the function on the new value until it either succeeds or reaches retry limit.

- Use atomic succession when you have standalone value (or composite value) that can change independently of the rest of the system.
- Atomic updates can inform other system values when they complete via watch functions.
- Atomic updates don't require coordination with other stateful resources.
- Collections make good candidates for atomic succession.
- Atomic succession has both sync and async representations using `atom` and `agent` respectively.

#### Transactional succession
- Use when two identities require to be updated **_together_**. Involves a transaction to ensure either all or none of the coordinated updates happen.
- If conflicts happen during update, the entire transaction is retried.
- A single transaction can contain many updates to several coordinated identities.
- This coordination is accomplished using software transaction memory (STM) implementing the mutiversion concurrency control (MVCC) model. 😐

### Tools for managing change
- The four reference types (atom, var, agent, ref) can be used to store state.
- In every case, the mechanism provides a mutable container for immutable values.
- The container can be created with an initial value and the value can be reset.
- The state is advanced using the unified update model.

- The reference types implement `IRef`

| IRef     | create-fn | update-fn(s)    | set-fn
| :------: | :-------: | :-------------: | ------
| Atom     | atom      | swap!           | reset!
| Ref      | ref       | alter,commute   | ref-set
| Var      | def       | alter-var-root  | var-set
| Agent    | agent     | send,send-off   | restart-agent

``` clj
;; to-create:
(create-fn container)

;; to update:
(update-fn container data-fn & args)

;; to assign new value:
(set-fn container new-val)
```

- Agents can include error handling options during creation.

#### Managed Updates with Atom
- An atom is a synchronized construct &mdash; every change we inflict on the atom completely happens before the next one is applied.
- Atoms update immediately.
- An atom can have a validator function. Before the value of the atom is changed, the proposed value is passed to the validator function. If the validator returns `false`, attempts to update the atom throw `IllegalStateException`. If the validator throws an exception of its own, that exception takes the place of the `IllegalStateException`.
- Validator functions must be pure as they can be called many times.
- Validator functions take a single argument.
- The validator function can be declared when defining the atom. If the atom has an initial state, the validator validates the initial state.


- All reference types can have **_watch_** functions.
- A **_watch_** function is a function of four arguments, a key, the watched reference, the old value and the new value. Watch functions are notified when the reference changes value successfully.

``` clj
(defn watch-fn [watch-key reference old-value new-value] ,,,)
```
- The `watch-key` is used under-the-cover to identify the watch function in the map of watch functions attached to the reference. Watch functions are retrievable using the `.getWatches` function. Can be removed using `remove-watch` and replaced using `add-watch`.


#### Transactional Change with Ref
- Changing the value of a ref must be done inside a transaction, using `dosync`. A single transaction can be composed of many updates:

``` clj
(defn init []
  (store/init {:eggs 2 :bacon 3 :apples 3
               :candy 5 :soda 2 :milk 1
               :bread 3 :carrots 1 :potatoes 1
               :cheese 3})
  (dosync
   (ref-set shopping-list #{:milk :butter :bacon :eggs
                            :carrots :potatoes :cheese :apples})
   (ref-set assignments {})
   (ref-set shopping-cart #{})))
```
- 3 ways of updating refs:
  - Use `ref-set` to update ref value directly. Will mostly see `ref-set` in the context of reinitialization.
  - Use `alter` to update using a function and retry if value of ref isn't the same as it was when the transaction began or when the last `alter` was applied within the current transaction. Use if the transaction needs to run without any outside interference.
  - Use `commute` to update using a function that will not retry if the value of the ref has changed in the course of the transaction's execution (when the tx doesn't require internal consistency). The function passed to commute is passed whichever value the ref has at that time.

In high performance apps, replacing `alter` with `commute` where possible can boost performance. This should only be done when the function passed to commute is commutative.

- The `ensure` function can be used to ensure a ref's value hasn't changed outside the current tx without updating it. Triggers a retry if the value has changed. Useful when refs have interdependent values.

## Chapter 5 - Use Your Cores
- Fire and forget async work can be shipped off to a background thread using `future`, which takes a body and invokes that body on a background thread pool maintained by Clojure. Can also use `future-call` to invoke a no-argument function instead of passing in a body. Either func returns a `java.lang.Future` object.
  - `future-cancel` will cancel the future object's execution
  - `future-done?` and `future-cancelled?` give status info

### Async and Stateful
- For stateful, async containers, use `agents`.
- Agents hold an immutable value and are updated asynchronously.
- Update actions on agents can be invoked asynchronously using `send` and `send-off`
- `send` is used for computational updates that won't block for IO. Underlying thread pool uses a fixed set of threads.
- `send-off` is used for updates that might block for any arbitrary time. Underlying thread pool will grow as needed, blocking isn't an issue. Same thread pool used for futures.
- Any agent update invoked on an agent inside an STM transaction or inside an agent action itself is delayed until the tx completes, making agents safe to call from within an STM tx or within other agent update actions to produce a side effect.

#### Waiting for a Response
- To block and wait for results from futures, dereference the future using either `deref` or the `@` shorthand.

#### Promises
- A promise is used to transfer one value from one thread to another.
- Can only transfer only one value.
- Multiple promises can be used to return values at different times in an async computation.


- Futures and Agents can be used to do async work.
- Futures and Promises allow to control of how results can be returned from those async tasks.


### Queues and Workers
- Sample queue implementation wrapping a persistent queue in a ref.

```clojure

(defn queue
  "Create a new stateful queue"
  []
  (ref clojure.lang.PersistentQueue/EMPTY))


(defn enq
  "Enqueue and item in q"
  [q item]
  (dosync
   (alter q conj item)))


(defn deq
  "Dequeue an item from q (nil if none)"
  [q]
  (dosync
   (let [item (peek @q)]
     (alter q pop)
     item)))
```
However, this queue doesn't block in `deq` when the queue is empty and waiting for data to arrive and returns `nil` instead. Consumer would have to repeatedly poll it. As such, Clojure persistent queues are not a good tool for managing a queue of work across threads.

#### Java Queues
- Java provides many implementations of a blocking queue in `java.util.concurrent` package, all implementations of `java.util.concurrent.BlockingQueue`.
- One of main differences in Java queue implementations is how they buffer data:
  - `LinkedBlockingQueue` - provided an optionally bounded buffer
  - `ArrayBlockingQueue` - provides a bounded buffer
  - `SynchronousQueue` - Provides no buffer (producer and consumer must wait until both are ready to hand a value from one to the other).
  - `LinkedTransferQueue` - combines the hand-off capabilities of `SynchronousQueue` with an optionally bounded buffer.

- These queues provide values in FIFO order. Java has two queues that reorder items:
  - `PriorityBlockingQueue` - bubbles high-priority items to the front of the queue.
  - `DelayQueue` - takes messages with a delay and makes them available only as the delay expires.


- Bounded buffer queues provide an opportunity for customization when a producer encounters a full buffer, allowing for blocking, times blocking, returning a special value and throwing an exception.

Sample queue implementation using Java queues
```clojure
(ns cljapplied.ch5.jqueue
  (:import [java.util.concurrent LinkedBlockingQueue]))


(defn pusher [q n]
  (loop [i 0]
    (when (< i n)
      (.put q i)
      (recur (inc i))))
  (.put q :END))


(defn popper [q]
  (loop [items []]
    (let [item (.take q)]
      (if (= item :END)
        items
        (recur (conj items item))))))


(defn flow [n]
  (let [q (LinkedBlockingQueue.)
        consumer (future (popper q))
        begin (System/currentTimeMillis)
        producer (future (pusher q n))
        received @consumer
        end (System/currentTimeMillis)]
    (println "Received:" (count received) "in" (- end begin) "ms")))
```

#### Making Threads
- `ThreadFactory` - interface representing a factory for threads
- `ExecutorService` - interface representing a combination of queue and worker pool

- Create a fixed pool of computation threads sized to the processor count like:
```clojure
(import '[java.util.concurrent Executors])
(def processors (.availableProcessors (Runtime/getRuntime)))
(defonce executor (Executors/newFixedThreadPool processors))

(defn submit-task [^Runnable task]
    (.submit executor task))
```

- Java represents a runnable task using the `Runnable` or `Callble` interfaces.
- Any task (any Clojure) function can be passed to ExecutorService for invocation

### Parallelism with Reducers
- Reducers provide a way to structure data transformation as a series of composable fine-grained operations like with sequences, but achieve parallelism while executing the entire transformation.
- A reducer consists of a reducible collection combined with a reducing function.
- A reducible collection is a collection that knows how to perform a reduce operation on itself as efficiently as possible.
- The reducing function is a function describing how to accumulate a result during a reduce (just like normal function passed to `reduce`)
- Reducers provide operations that mirror their sequence counterparts, e.g map, filter, mapcat, e.t.c
- Each of the reducer operations takes and returns a reducer **_but doesn't perform the transformation_**.
- Instead, these operations only modify the reduction function to take the new operation into account.
- To perform the operation, a new reduce-like function, `fold`, is invoked.
- `fold` partitions the source collection into groups, performs a reduce on each group by using the reducing function then combines the partitions using a combining function.
- Currently, only persistent vectors and maps can be folded in parallel; other collections fall back to a serial reduce.

Example with sequence function vs reducer functions
```clojure
(def ex-product {:id "230984234" :class :ground :weight 10 :volume 300})

;; Calculate the total weight of all ground shipments
(defn ground? [product]
  (= :ground (:class product)))

(defn ground-weight [products]
  (->> products
       (filter ground?)
       (map :weight)
       (reduce +)))
```

Using Reducer functions
```clojure
(require '[clojure.core.reducers :as r])

(defn ground-weight [products]
  (->> products
       (r/filter ground?)
       (r/map :weight)
       (r/fold +)))
```

## Chapter 6: Creating Components
- Components are collections of finer grained elements (functions, records, protocols) that have a greater overall purpose.
- Namespaces are named, hierarchical containers used to collect, organize and name groups of forms.
- Dependencies are declared and loaded at the namespace level.
- Namespaces are a language feature for organizing functions, components are a means of organizing at the problem level.

### Namespace Categories
- Utility - generic funcs organised by domain or purpose
- Data definition - A custom collection or set of domain entities alongside helper funcs using the collection or entities.
- Abstraction
- Implementation - Implements an abstraction defined by a protocol or interface
- Assembly - Given a set of implementations and a config specifying how the implementations should be constructed and connected, this namespace ties everything together.
- Entry point - Connect the start of the application (includes gathering the config) to initiate assembly and other life-cycle ops.

### Public vs Private Functions
- Clojure is biased towards making data and functions public by default.
- Data and functions can be marked as private using:
  - `defn-`
  - `^:private` meta tag
  - Omitting docstrings, for tools like autodoc which omit functions without docstrings.

- Vars marked as private can still be accessed directly with the reader var syntax or by calling directly into the namespace object.

### Designing Component APIs
- After identifying a component, think about the purpose it'll serve and how it'll be used by other components
- Typical component kinds:
  - Information managers - track state, either in-memory or in external data store, providing ops to create, modify, query that data
  - Processors - all about data transformaton or computation
  - Facade - Exist to primarily make another external system accessible and pluggable.

- Components can be interacted with by:
  - invoking functions
  - passing messages on a queue or channel


- Component configuration should be passed as a map or record to allow the config data to evolve as needed.

- If a component needs to reference another component, instead of passing a component directly to another component as a dependency, pass a channel. In which case the channel is treated as the dependency instead of the remote component.

- Components need someplace to store config, deps and internal runtime state; records are best suited for this since protocols defining component behaviour can be implemented.

- Custom functions for creating component instances are usually provided since components often need to set up stateful fields with initial values.
- Should also provide a function to stop the component instance.


## Chapter 7: Compose Your Application
