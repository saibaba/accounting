(ns accounting.core)

(def database (atom { }))
(def alias-database (atom { }))

(defn create-set-of-books
  [sob]
  (reset! database (update-in @database [sob] (fn[_] { })))
  (reset! alias-database (update-in @database [sob] (fn[_] { }))))

(create-set-of-books 100)

(defn create-account
  [sob id type alias]
  (reset! database (update-in @database [sob id] (fn[_] { :type type :balance 0 })))
  (reset! alias-database (update-in @alias-database [sob alias] (fn[_] id))))

(defn update-account-balance
  [sob id balance]
  (reset! database (update-in @database [sob id :balance] (fn[_] balance))))

(defn is-debit?
  [acct]
  (= (:type acct) :debit))

(defn balancer
  [r a]
  (if (is-debit? a)
    (update-in r [:debit-balances] (fn[_] (+ (:debit-balances r) (:balance a))))
    (update-in r [:credit-balances] (fn[_] (+ (:credit-balances r) (:balance a))))))

(defn get-balances
  [sob]
  (let [accounts (vals (get @database sob))]
    (reduce (fn [r a] (balancer r a)) {:debit-balances 0 :credit-balances 0} accounts)))

(defn balances-ok?
  [sob]
  (let [b (get-balances sob) debits (:debit-balances b) credits (:credit-balances b)]
    (= debits credits)))

(defn resolve-id
  [sob idorsym]
  (if (instance? clojure.lang.Keyword idorsym) (get (get @alias-database sob) idorsym) idorsym))

(defn get-account
  [sob id]
  (get (get @database sob) id))

(create-account 100 12310 :debit  :cash)
(create-account 100 13310 :debit  :vehicles)
(create-account 100 52310 :credit :loan)
(create-account 100 90000 :credit :equity)

(defn cr-by-id
  [sob id amt]
  (let [a (get-account sob id) type (:type a) balance (:balance a)
    new-balance (if (is-debit? a) (- balance amt) (+ balance amt))]
    (update-account-balance sob id new-balance)))

(defn cr
  [sob idorsym amt]
  (cr-by-id sob (resolve-id sob idorsym) amt))

(defn dr-by-id
  [sob id amt]
  (let [a (get-account sob id) type (:type a) balance (:balance a)
    new-balance (if (= type :debit) (+ balance amt) (- balance amt))]
    (update-account-balance sob id new-balance)))

(defn dr
  [sob idorsym amt]
  (dr-by-id sob (resolve-id sob idorsym) amt))
