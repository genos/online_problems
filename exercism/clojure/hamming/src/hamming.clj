(ns hamming)

(defn distance [a b]
  (when (= (count a) (count b))
    (reduce + (map #(if (= %1 %2) 0 1) a b))))
