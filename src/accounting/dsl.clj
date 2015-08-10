(ns accounting.dsl)

(defn is-debit?
  [acct]
  (= (:type acct) :debit))

;;; accounting system state monads


(defn set-of-books
  [sobid]
  (fn [state]
    ['() (assoc state :set-of-books sobid)]))

(defn account
  [type id alias]
  (fn [state]
    (let [ r1 (update-in state [:accounts id]   (fn [_] {:type type :balance 0}))
           r2 (update-in r1    [:aliases alias] (fn [_] {:id id}))]
      ['() r2])))

; due to the way get works, if aliases does not contain, returns nil as first of the response
(defn resolve-id
  [idorsym]
  (fn [state]
    [(if (instance? clojure.lang.Keyword idorsym) (:id (get (get state :aliases) idorsym)) idorsym) state]))

; 
(defn get-account
  [id]
  (fn [state]
    [(get (get state :accounts) id) state]))

(defn update-account-balance
  [id balance]
  (fn [state]
    (let [ ok (contains? (:accounts state) id)]
      (if ok
        [balance (update-in state [:accounts id :balance] (fn[_] balance))]
        [nil state]))))

(defn balancer
  [r a]
  (if (is-debit? a)
    (update-in r [:debit-balances] (fn[_] (+ (:debit-balances r) (:balance a))))
    (update-in r [:credit-balances] (fn[_] (+ (:credit-balances r) (:balance a))))))

(defn get-balances
  []
  (fn [state]
    (let [accounts (vals (:accounts state))
          balances (reduce (fn [r a] (balancer r a)) {:debit-balances 0 :credit-balances 0} accounts)]
      [balances state])))

(defn balances-ok?
  [state]
  (let [ bf (get-balances) [balances state] (bf state)]
    [(= (:credit-balances balances) (:debit-balances balances)) state]))

(defn transaction-note
  [note]
  (fn [state]
    ['() (update-in state [:transaction-notes] (fn [_] (conj (:transaction-notes state) note )))]))

;;;; monad stuff ;;;;;;

;;;;;;;;;; return [nil state] in case error - ideally should use monad composer or transformer
;;;;;;;;;;; how is error message conveyed ?

(defn >>=
  [mv mf]
  (fn [state]
    (let [ [v new-state] (mv state) ]
      (if (not (nil? v))
        (let [mv2 (mf v)]
          (mv2 new-state))
        [nil state]))))

(defn build-accounting-system1
  [r form]
  `(>>= ~form ~r))

(defn build-accounting-system
  [r form]
  (if (and (= 3 (count form)) (= '<- (second form)))
     `(>>= ~(last form) (fn[~(first form)] ~r))
     `(>>= ~form (fn [~'_] ~r))))

(defmacro accounting-system
  [& forms]
  (let [rforms (reverse forms)]
    (reduce (fn [r form] (build-accounting-system r form)) rforms)))

(defn get-state
  [state]
  [state state])

(defn get-transaction-notes
  [state]
  [(into [] (reverse (:transaction-notes state))) state])

;; orchestration (one or more applications of m-bind)

(defn cr
  [idorsym amt]
  (fn [state]
    ( (accounting-system
      (id <- (resolve-id idorsym))
      (a <- (get-account id))
      (new-balance <- (fn [state]
        (let [type (:type a) balance (:balance a)
          new-balance (if (= type :debit) (- balance amt) (+ balance amt))]
          [new-balance state])))
      (update-account-balance id new-balance)) state)))

(defn dr
  [idorsym amt]
  (fn [state]
    ( (accounting-system
      (id <- (resolve-id idorsym))
      (a <- (get-account id))
      (new-balance <- (fn [state]
        (let [type (:type a) balance (:balance a)
          new-balance (if (= type :debit) (+ balance amt) (- balance amt))]
          [new-balance state])))
      (update-account-balance id new-balance)) state)))

(defmacro accounting-transaction
  [& forms]
  `(fn [~'state] ((accounting-system
     ~@forms
     (get-balances)
   ) ~'state )))

(defn >=>
  [f g]
  (fn [s] (let [mf (>>= g (fn [_] f))] (mf s) )))

(defmacro >$>
  [& forms]
  (reduce (fn [r form] `(>=> ~form ~r)) forms))

""" note that we pretty much never need m-return for state monad !!!!!

(m-return
  [v]
  (fn [state]
    [v state]))
"""
