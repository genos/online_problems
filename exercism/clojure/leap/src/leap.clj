(ns leap)

(defn leap-year? [y]
  (letfn [(div? [n d] (zero? (mod n d)))]
    (cond
      (div? y 400) true
      (div? y 100) false
      (div? y 4) true
      :else false)))
