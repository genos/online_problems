(ns hamming)

(defn distance [a b]
  (when (= (count a) (count b))
    (count (filter false? (map = a b)))))
