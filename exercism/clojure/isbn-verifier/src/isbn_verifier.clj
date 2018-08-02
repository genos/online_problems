(ns isbn-verifier)

(defn clean [s]
  (map first (re-seq #"[0-9X]" s)))

(defn length-ok? [c]
  (= (count c) 10))

(defn x-only-last? [c]
  (let [xs (keep-indexed #(if (= \X %2) %1) c)]
    (or
     (empty? xs)
     (and (= 1 (count xs))
          (= 9 (first xs))))))

(def int-map
  (apply hash-map (apply concat (map-indexed (fn [i x] [x i]) "0123456789X"))))

(defn polynomial [c]
  (apply + (map-indexed (fn [i x] (* (inc i) (int-map x))) (reverse c))))

(defn isbn? [s]
  (->
   s
   clean
   (as-> c
         (and
          (length-ok? c)
          (x-only-last? c)
          (zero? (mod (polynomial c) 11))))))
