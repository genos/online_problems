(ns series
  (:require [clojure.string :refer [join]]))

(defn slices [string n]
  (if (zero? n)
    [""]
    (map join (partition n 1 string))))
