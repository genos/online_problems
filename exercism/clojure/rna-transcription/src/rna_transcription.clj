(ns rna-transcription
  (:require [clojure.string :as str]))

(defn to-rna [dna]
  (str/join
    (map #(or ({\A \U \C \G \G \C \T \A} %) (assert false)) dna)))
