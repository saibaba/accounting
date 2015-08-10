(ns accounting.state-test
  (:require [clojure.test :refer :all]
            [accounting.state :refer :all]))

(def r 
  (m-bind (inc-s 31)      (fn [inc-val]
  (m-bind (dec-s inc-val) (fn [inc-dec-val]
          (double-s inc-dec-val))))))


(deftest t-state-monad-bind
  (testing "State monad bind"
    (let [result (r '()) val (first result) state (last result) ]
      (is (= val 62))
      (is (= state '(:double :dec :inc))))))

(def r2  (state-transition
  (inc-val         <- (inc-s 31))
  (inc-dec-val     <- (dec-s inc-val))
  (inc-dec-dbl-val <- (double-s inc-dec-val))
  (m-result inc-dec-dbl-val) ))

(deftest t-state-monad-macro
  (testing "State monad macro"
    (let [result (r2 '()) val (first result) state (last result) ]
      (is (= val 62))
      (is (= state '(:double :dec :inc))))))
