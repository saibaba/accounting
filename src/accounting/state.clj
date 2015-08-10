(ns accounting.state)

(defn inc-s [x]
  (fn [state]
    [(inc x) (conj state :inc)]))
(defn double-s [x]
  (fn [state]
    [(* 2 x) (conj state :double)]))
(defn dec-s [x]
  (fn [state]
    [(dec x) (conj state :dec)]))

(defn m-bind [mv mf]
  (fn [state]
    (let [[temp-v temp-state] (mv state)
          new-mv (mf temp-v)]
      (new-mv temp-state))))

(defn m-result [x]
  (fn [state]
    [x state]))

(defn build-form
  [r form]
  (let [v (first form) e (last form)]
    `(m-bind ~e (fn [~v] ~r))))

(defmacro state-transition
  [& forms]
  (let [rforms (reverse forms) init (first rforms) rem (rest rforms)]
    (reduce (fn [r form] (build-form r form)) init rem)))
