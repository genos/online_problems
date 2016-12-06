(ns flatten-array)

(defn flatten [array]
  (vec (remove nil? (clojure.core/flatten array))))
