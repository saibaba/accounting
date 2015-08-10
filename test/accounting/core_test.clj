(ns accounting.core-test
  (:require [clojure.test :refer :all]
            [accounting.core :refer :all]))

(deftest buy-vehicle
  (testing "Start business with 20K, buy a 5K vehicle for 1K cash and 4K loan"
    (dr 100 :cash    20000.0)
    (cr 100 :equity  20000.0)
    (cr 100 :cash     1000.0)
    (dr 100 :vehicles 5000.0)
    (cr 100 :loan     4000.0)
    (is (= (balances-ok? 100)))))

"""
(def accounting-system-100
  (accounting-system
    (set-of-books 100)
    (account :debit  :cash     13210)
    (account :debit  :vehicles 61321)
    (account :credit :loan     61331)
    (account :credit :equity   99000)))

(defmacro transaction
  [system & forms]
  (let [ storage { :accouting { } :aliases { } } ]

  (defn create-set-of-books
  [sob]
  (reset! database (update-in @database [sob] (fn[_] { })))
  (reset! alias-database (update-in @database [sob] (fn[_] { }))))

)

(accounting-transaction
  accounting-system-100
  (dr :cash    20000.0)
  (cr :equity  20000.0)
  (cr :cash     1000.0)
  (dr :vehicles 5000.0)
  (cr :loan     4000.0)
)
"""
