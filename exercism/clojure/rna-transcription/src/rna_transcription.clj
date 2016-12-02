(ns rna-transcription
  (:require [clojure.string :refer [join]]))

(defn to-rna [dna]
  (join (map #(or ({\A \U \C \G \G \C \T \A} %) (assert false)) dna)))
