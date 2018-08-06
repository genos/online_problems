(ns isbn-verifier)

(defn x-only-last? [c]
  (let [xs (keep-indexed #(if (= \X %2) %1) c)
        n  (count xs)
        x  (first xs)]
    (or (empty? xs)
        (and (= 1 n) (= 9 x)))))

(defn modular-polynomial [c]
  (let [int-map (zipmap "0123456789X" (range))]
    (mod
     (apply + (map #(mod (* %1 (int-map %2)) 11)
                   (iterate inc 1)
                   (reverse c)))
     11)))

(defn isbn? [s]
  (let [c (map first (re-seq #"[0-9X]" s))]
    (and
     (= (count c) 10)
     (x-only-last? c)
     (zero? (modular-polynomial c)))))
