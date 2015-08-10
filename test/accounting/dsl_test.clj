(ns accounting.dsl-test
  (:require [clojure.test :refer :all]
            [accounting.dsl :refer :all]))

(def accounting-system-100
  (accounting-system
    (set-of-books 100)
    (account :debit  13210 :cash)
    (account :debit  61321 :vehicles)
    (account :credit 61331 :loan)
    (vehicles-account-id <- (resolve-id :vehicles))
    (account :credit 99000 :equity)))

(def buy-van (accounting-transaction
  (dr :cash    20000.0)
  (cr :equity  20000.0)
  (cr :cash     1000.0)
  (dr :vehicles 5000.0)
  (cr :loan     4000.0)))

(deftest buy-vehicle
  (testing "Start business with 20K, buy a 5K vehicle for 1K cash and 4K loan"
    (let [initial-state (last (accounting-system-100 {}))
          current-state (last (buy-van initial-state))
          r ( balances-ok? current-state) ok (first r)]
      (is (= ok true)))))

(deftest compose
  (testing "compose two accounting monads"
    (let [ c (>=> balances-ok? (>=> buy-van accounting-system-100))  r (c {}) ok (first r) ]
      (is (= ok true)))))

(deftest buy-vehicle2
  (testing "Start business with 20K, buy a 5K vehicle for 1K cash and 4K loan by multi compose function"
    (let [c (>$> accounting-system-100 buy-van balances-ok?) r (c {})  ok (first r)]
      (is (= ok true)))))

