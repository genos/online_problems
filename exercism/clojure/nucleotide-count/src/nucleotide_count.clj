(ns nucleotide-count)

(def base {\A 0 \C 0 \G 0 \T 0})
(def base-keys (set (keys base)))

(defn nucleotide-counts [dna]
  {:post [(= base-keys (set (keys %)))]}
  (merge-with + base (frequencies dna)))

(defn count [s dna]
  {:pre [(base-keys s)]}
  ((nucleotide-counts dna) s 0))
