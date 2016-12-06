(ns flatten-array)

(defn flatten [array]
  (remove nil? (clojure.core/flatten array)))
