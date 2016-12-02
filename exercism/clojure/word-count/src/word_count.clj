(ns word-count
  (:require [clojure.string :as str]))

(defn word-count [s]
  (->> s
       (map str/lower-case)
       str/join
       (re-seq #"[a-z0-9]+")
       frequencies))
