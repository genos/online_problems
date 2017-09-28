(ns collatz-conjecture)

(defn collatz [n]
  (if (< n 1) (throw (IllegalArgumentException. "n must be positive"))
      (loop [k n
             i 0]
        (if (= k 1) i
          (recur (if (even? k) (quot k 2) (inc (* 3 k)))
                 (inc i))))))
