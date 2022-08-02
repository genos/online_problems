(ns bird-watcher)

(def last-week
  "Counts of birds seen last week"
  [0 2 5 3 7 8 4])

(defn today
  "Number of birds seen today (the last element in a vector)."
  [birds]
  (peek birds))

(defn inc-bird
  "Increment today's bird count (inc the last element in a vector)."
  [birds]
  (let [n (count birds)
        c (peek birds)]
    (assoc birds (dec n) (inc c))))

(defn day-without-birds?
  "Did any day have a bird count of zero?"
  [birds]
  (not-every? pos? birds))

(defn n-days-count
  "Sum of first n day counts."
  [birds n]
  (reduce + (take n birds)))

(defn busy-days
  "Number of days with 5 or more bird visits."
  [birds]
  (count (filter (partial < 4) birds)))


(defn odd-week?
  "Does this week match our weird pattern?"
  [birds]
  (= (take 7 (iterate (partial - 1) 1)) birds))
