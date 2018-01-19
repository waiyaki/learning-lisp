;; Build an example to show updates where transactions need to update existing information
(defrecord Message [sender text])

;; `messages` reference that initially points to an empty list.
(def messages (ref ()))

;; Then add new messages to the front of messages:

(defn naive-addd-message [msg]
  (dosync (ref-set messages (cons msg @messages))))

;; Better, perform the read and update in a single step.
;; Use `alter` for this => (alter ref update-fn & args)
(defn add-message [msg]
  (dosync (alter messages conj msg)))

(add-message (->Message "user 1" "Hello"))
(add-message (->Message "user 2" "Howdy"))

;; When you don't care about transaction ordering, use `commute` for better performance
;; Commutes allow updates to occur in any order.
(defn add-message-commute [msg]
  (dosync (commute messages conj msg)))

;; Options to `ref` include an optional validation function that can throw
;; an exception and prevent a transaction from completing.
;; These options are not a map, they're a sequence of key-value pairs
;; If the validation fails, the entire transaction rolls back.

;; E.g., ensure all messages have non-nil sender and text
(defn valid-message? [msg]
  (and (:sender msg) (:text msg)))

(def validate-message-list #(every? valid-message? %))

(def messages (ref () :validator validate-message-list))

(add-message "Invalid message") ;; IllegalStateException Invalid reference state

@messages ;; => ()

(add-message (->Message "Aaron" "Message")) ;; => (#walkthrough.core.Message{:sender "Aaron", :text "Message"})

;; Refs are great for coordinated access to shared state.
;; For uncoordinated, synchronous updates, use `atoms`.
;; Atoms can be updated using either `reset!` or `swap!`.
(def current-track (atom "Venus, the Bringer of Peace"))
@current-track ;; => "Venus, the Bringer of Peace"

(reset! current-track "Credo")
@current-track ;; => "Credo"

(def current-track (atom {:title "Credo" :composer "Byrd"}))
(reset! current-track {:title "Spem in Alium" :composer "Tallis"})
@current-track ;; => {:title "Spem in Alium", :composer "Tallis"}

;;`swap!` will perform an update by calling a function against the current atom
;; value with any additional args.
(swap! current-track assoc :title "Sante Deus") ;; => {:title "Sante Deus", :composer "Tallis"}

;; Calls to `swap!` may be retried, so the function passed to it should be pure.

;; Refs and atoms perform synchronous updates.
;; For asynchronous updates at some later time, use Agents.
