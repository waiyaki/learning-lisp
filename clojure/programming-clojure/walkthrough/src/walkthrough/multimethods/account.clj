(ns walkthrough.multimethods.account)

;; `alias` can be used to specify shorter alias for longer names
(alias 'acc 'walkthrough.multimethods.account)

(def test-savings {:id 1 :tag ::acc/savings ::balance 100M})

(def test-checking {:id 2 :tag ::acc/checking ::balance 250M})

(defmulti interest-rate :tag)
(defmethod interest-rate ::acc/checking [_] 0M)
(defmethod interest-rate ::acc/savings [_] 0.05M)

(defmulti account-level :tag)

(defmethod account-level ::acc/checking [acct]
  (if (>= (:balance acct) 5000) ::acc/premium ::acc/basic))

(defmethod account-level ::acc/savings [acct]
  (if (>= (:balance acct) 1000) ::acc/premium ::acc/basic))

(defmulti service-charge account-level)

(defmethod service-charge ::basic [acct]
  (if (= (:tag acct) ::checking) 25 10))

(defmethod service-charge ::premium [_] 0)

;; Instead of having the conditional in service-charge, can dispatch by
;; two criteria
(defmulti service-charge (fn [acct] [(account-level acct) (:tag acct)]))
(defmethod service-charge [::acc/basic ::acc/checking] [_] 25)
(defmethod service-charge [::acc/basic ::acc/savings] [_] 10)
(defmethod service-chage [::acc/premium ::acc/checking] [_] 0)
(defmethod service-charge [::acc/premium ::acc/savings] [_] 0)

;; Since all premium accounts have the same charge, it's redundant to define
;; separate methods for the two accounts.
;; Instead, we can define arbitrary parent-child relationship using `derive`
;; (derive child parent)
(derive ::acc/savings ::acc/account)
(derive ::acc/checking ::acc/account)

(isa? ::acc/savings ::acc/account) ;; => true

;; Can now define a single `service-charge` to handle ::premium
(defmethod service-charge [::acc/premium ::acc/account] [_] 0)
