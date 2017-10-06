(ns perfect-numbers)

(defn classify [n]
  (if
   (< n 1)
    (throw (IllegalArgumentException. "Positive numbers only"))
    (let [aliquot-sum (reduce + (filter #(zero? (mod n %)) (range 1 n)))]
      (cond
        (< aliquot-sum n) :deficient
        (= aliquot-sum n) :perfect
        :else :abundant))))
