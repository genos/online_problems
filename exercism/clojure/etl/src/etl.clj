(ns etl
  (:require [clojure.string :refer [lower-case]]))

(defn transform [m]
  (into {}
        (for [[score words] m
              word words]
          [(lower-case word) score])))
