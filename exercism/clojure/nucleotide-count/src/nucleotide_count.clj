(ns nucleotide-count)

(def base {\A 0 \C 0 \G 0 \T 0})

(defn nucleotide-counts [dna]
  (let [freqs (frequencies dna)
        result (merge base freqs)]
    (assert (= (keys base) (keys result)))
    result))

(defn count [s dna]
  (assert (contains? base s))
  ((nucleotide-counts dna) s 0))
