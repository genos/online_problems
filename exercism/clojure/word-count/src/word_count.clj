(ns word-count
  (:require [clojure.string :as str]))

(defn word-count [s]
  (->> s
       str/lower-case
       (re-seq #"\w+")
       frequencies))
