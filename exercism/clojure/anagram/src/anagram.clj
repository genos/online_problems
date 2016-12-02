(ns anagram
  (:require [clojure.string :refer [lower-case]]))

(defn- is-anagram [a b]
  (let [x (lower-case a)
        y (lower-case b)]
    (and (not= x y) (= (frequencies x) (frequencies y)))))

(defn anagrams-for [x xs]
  (filter (partial is-anagram x) xs))
