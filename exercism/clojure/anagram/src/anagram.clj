(ns anagram
  (:require [clojure.string :as str]))

(defn anagrams-for [x xs]
  (let [y (str/lower-case x)
        z (frequencies y)
        zs (->> xs
                (filter #(not= y (str/lower-case %)))
                (group-by (comp frequencies str/lower-case)))]
    (get zs z [])))
