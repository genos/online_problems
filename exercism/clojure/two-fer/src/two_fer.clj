(ns two-fer)

(defn two-fer
  ([] (two-fer "you"))
  ([n] (str "One for " n ", one for me.")))
