;; Prefer `alter` to `commute` when updates are not commutative, and most aren't.
(def counter (ref 0))

(defn next-counter [] (dosync (alter counter inc)))


;;; Agents
;; Use agents for asynchronous updates - tasks that can proceed independently with minimal coordination.

(def counter (agent 0))

(send counter inc)

;; The call to `send` doesn't return the new value of the agent, returns the agent itself.
;; `send` queues the update-fn to run later and immediately returns.

;; Agents can be dereferenced:
@counter ;; => 1

;; To be sure the agent has completed the actions sent to it, call `await` or `await-for`
;; `(await & agents)`
;; `(await-for timeout-millis & agents)`
;; These will cause the thread to block until all actions sent from current
;; thread or agent have completed.
;; `await-for` returns `nil` if the timeout expires, a non-nil value otherwise.
;; `await` has no timeout, it waits forever.

;;; Validating Agents and Handling Errors
;; Agents can take validation functions
;; `(agent initial-state options*)`, where `options` include:
;; `:validator` validate-fn
;; `:meta` metadata-map
;; `:error-handler` handler-fn
;; `:error-mode` mode-keyword (:continue or :fail)

(def counter (agent 0 :validator number?))

(send counter (fn [_] "boo"))

@counter ;; => 0

;; When no `:error-handler` is provided, the error mode will be set to `:fail`
(agent-error counter)
;; => #error {
;; :cause "Invalid reference state"
;; :via
;; [{:type java.lang.IllegalStateException
;; :message "Invalid reference state"
;; :at [clojure.lang.ARef validate "ARef.java" 33]}]
;; ...

;; When an agent has an error, all actions will be queued until `restart-agent` is called.
;; When with errors, all subsequent attempts to query the agent will return an error.
(restart-agent counter 0)
@counter ;; => 0

;; When an `:error-handler` is provided, the error mode will be set to `:continue`
(defn handler [agent err]
  (println "ERR!! " (.getMessage err)))

(def counter2 (agent 0 :validator number? :error-handler handler))

(send counter (fn [_] "boo")) ;; => #agent[{:status :failed, :val 0} 0x457fa6f1]

;;; Including Agents in Transactions
;; Transactions can be retried, so shouldn't have side-effects.
;; If side-effects are desired, agents can be used.
;; If an action is sent to an agent within a transaction, that action is sent exactly once,
;; if and only if the transaction succeeds.

;; Consider an agent that writes to a file when a trx succeeds:
(def messages (ref ()))
(defrecord Message [sender text])

(def backup-agent (agent "output/messages-backup.clj"))

(defn add-message-with-backup [msg]
  (dosync
   (let [snapshot (commute messages conj msg)]
     (send-off backup-agent (fn [filename]
                              (spit filename snapshot)
                              filename)))))

;; `send-off` is a variant of `send` for actions that expect to block, as a file
;; write with `spit` might do.
(add-message-with-backup (->Message "John" "Message One"))
(add-message-with-backup (->Message "Jane" "Message Two"))


;;; Managing Per-Thread State with Vars
;; When you call `def` or `defn`, you create a `var`.
;; The initial value passed to `def` becomes the **_root binding_** or the `var`.
(def ^:dynamic foo 10)

;; The binding of `foo` is shared by all threads
foo ;; => 10

(.start (Thread. (fn [] (println foo))))
;; => nil
;; ==> 10
;; The call to `.start` returns `nil`, and the value of `foo` is printed from
;; the new thread.


;; Bindings have *dynamic* scope
;; Visible everywhere during a thread's execution, until the thread exits the
;; scope where the binding began.
;; A binding is not visible to other threads.

;; E.g., create a thread local binding for `foo` and check its value.
;; Observe `let` has no effect outside its own form
;; The binding stays in effect down the chain.
(binding [foo 42] foo) ;; => 42

(defn print-foo [] (println foo))

(let [foo "let foo"] (print-foo))
;; => nil
;; ==> 10

(binding [foo "bound foo"] (print-foo))
;; => nil
;; ==> bound foo

;;; State Models
;; Refs and STM - Coordinated, synchronous updates    - Pure functions
;; Atoms        - Uncoordinated, synchronous updates  - Pure functions
;; Agents       - Uncoordinated, asynchronous updates - Any functions
;; Vars         - Thread-local dynamic scopes         - Any functions
;; Java locks   - Coordinated, synchronous updates    - Any functions
