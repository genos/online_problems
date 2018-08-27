(ns collatz-conjecture)

(defn- step [k]
  (if
    (even? k)
    (quot k 2)
    (inc (* 3 k))))

(defn- sequence-length [k]
  (count
    (take-while
      #(> % 1)
      (iterate step k))))

(defn collatz [n]
  (if (< n 1)
    (throw (IllegalArgumentException. "n must be positive"))
    (sequence-length n)))
