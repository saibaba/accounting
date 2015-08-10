(ns accounting.business1-test
  (:require [clojure.test :refer :all]
            [accounting.dsl :refer :all]))

(def accounting-system-100
  (accounting-system
    (set-of-books 100)
    (account :credit 30300 :service-revenue)
    (account :debit 60200 :accounts-receivable)))

(def start-business (accounting-transaction
  (transaction-note "start business with 10K from Mr. Gray")
  (account :debit  10100 :cash)
  (account :credit 99100 :grays-equity)
  (dr :cash 10000.0)
  (cr :grays-equity 10000.0)))

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
 
(defn render-services
  [amt]
  (accounting-transaction
    (transaction-note "made a sale!")
    (dr :accounts-receivable amt)
    (cr :service-revenue amt)))
 
(deftest conduct-business
  (testing "conduct"
    (let [c (>$> 
              accounting-system-100
              start-business
              pay-tax-and-license-fee
              buy-furniture
              buy-service-equipment
              buy-service-supplies
              (render-services 1900.0)
              balances-ok?)
          r (c {})  ok (first r)]
      (println (first (get-transaction-notes (last r))))
      (is (= ok true)))))

