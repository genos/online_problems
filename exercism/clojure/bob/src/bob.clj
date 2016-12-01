(ns bob
  (:require [clojure.string :as str]))

(defn response-for [s]
  (cond
    (str/blank? s) "Fine. Be that way!"
    (and
      (some #(Character/isLetter %) s)
      (= s (str/upper-case s))) "Whoa, chill out!"
    (str/ends-with? s "?") "Sure."
    :else "Whatever."))
