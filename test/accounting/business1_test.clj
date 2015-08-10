(ns accounting.business1-test
  (:require [clojure.test :refer :all]
            [accounting.dsl :refer :all]))

; for business http://www.accountingverse.com/accounting-basics/journal-entry-examples.html

(def accounting-system-100
  (accounting-system
    (set-of-books 100)
    (account :credit 30300 :service-revenue)
    (account :debit 60200 :accounts-receivable)))

(def start-business (accounting-transaction
  (transaction-note "start business Mr. Gray corp")
  (account :debit  10100 :cash)
  (account :credit 99100 :grays-equity)))

(defn get-investment
  [amt]
  (accounting-transaction
    (transaction-note "got investment!")
    (dr :cash amt)
    (cr :grays-equity amt)))

(def pay-tax-and-license-fee (accounting-transaction
  (transaction-note "pay tax and license fee")
  (account :debit 20100 :tax-and-license)
  (cr :cash 370.0)
  (dr :tax-and-license 370.0)))

(def buy-furniture (accounting-transaction
  (transaction-note "Buy furniture and fixtures")
  (account :debit 20200 :furniture-and-fixtures)
  (cr :cash 3000.0)
  (dr :furniture-and-fixtures 3000.0)))

(def buy-service-equipment (accounting-transaction
  (transaction-note "Buy service equipment with half cash and half payable in 30 days")
  (account :debit 20300 :service-equipment)
  (account :credit 60100 :accounts-payable)
  (dr :service-equipment 16000.0)
  (cr :cash 8000.0)
  (cr :accounts-payable 8000.0)))

(def buy-service-supplies (accounting-transaction
  (transaction-note "Buy service supplies for 15K on account")
  (account :debit 20400 :service-supplies)
  (dr :service-supplies 15000.0)
  (cr :accounts-payable 15000.0)))
 
(defn render-cash-services
  [amt]
  (accounting-transaction
    (transaction-note "made a cash sale!!")
    (dr :cash amt)
    (cr :service-revenue amt)))

(defn render-services
  [amt]
  (accounting-transaction
    (transaction-note "made a sale!")
    (dr :accounts-receivable amt)
    (cr :service-revenue amt)))

(defn receive-cash
  [amt]
  (accounting-transaction
    (transaction-note "got cash from sale")
    (cr :accounts-receivable amt)
    (dr :service-revenue amt)))

(defn pay-liability
  [amt]
  (accounting-transaction
    (transaction-note "pay liability")
    (dr :accounts-payable amt)
    (cr :cash amt)))

(defn pay-investor
  [amt]
  (accounting-transaction
    (transaction-note "pay owner")
    (dr :grays-equity amt)
    (cr :cash amt)))

(deftest conduct-business
  (testing "conduct"
    (let [c (>$> 
              accounting-system-100
              start-business
              (get-investment 10000.0)
              pay-tax-and-license-fee
              buy-furniture
              buy-service-equipment
              buy-service-supplies
              (render-cash-services 1900.0)
              (render-services 4250.0)
              (get-investment 3200.0)
              (render-services 3400.0)
              (receive-cash 4250.0)
              (pay-liability 500.0)
              (pay-investor 7000.0)
              balances-ok?)
          r (c {})  ok (first r)]
      (println (first (get-transaction-notes (last r))))
      (is (= ok true)))))

