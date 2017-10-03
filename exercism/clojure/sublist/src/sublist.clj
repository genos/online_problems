(ns sublist)

(defn helper-
  "If `small` is an *in-order* subsequence of `big`, returns `message`;
  otherwise, returns `:unequal`."
  [big small message]
  (let [k (count small)
        candidates (partition k 1 big)]
    (if (some #(= % small) candidates) message :unequal)))

(defn classify [xs ys]
  (cond
    (= xs ys) :equal
    (> (count xs) (count ys)) (helper- xs ys :superlist)
    :else (helper- ys xs :sublist)))
