(ns armstrong-numbers)

(defn digits
  "Digits of `n` in base `b` (defaults to 10)"
  ([n] (digits 10 n))
  ([b n] (digits b n []))
  ([b n ds]
   (if (zero? n)
     ds
     (recur b (quot n b) (conj ds (rem n b))))))

(defn armstrong? [n]
  {:pre [(pos? n)]}
  (let [ds (digits n)
        len (count ds)]
    (= n (reduce + (map #(.pow (BigInteger. (str %)) len) ds)))))
