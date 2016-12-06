(ns etl
  (:require [clojure.string :refer [lower-case]]))

(defn invert [k vs]
  (let [ms (map (fn [v] {(lower-case v) k}) vs)]
    (into {} ms)))

(defn transform [m]
  (into {} (map (partial apply invert)) m))
