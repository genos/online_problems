(ns phone-number
  (:require [clojure.string :as str]))

(defn number [s]
  (let [c (str/replace s #"\D" "")
        n (count c)]
    (cond
      (= 10 n) c
      (and (= 11 n) (str/starts-with? c "1")) (subs c 1)
      :else "0000000000")))

(defn area-code [s]
  (subs (number s) 0 3))

(defn pretty-print [s]
  (str/replace (number s) #"(\d{3})(\d{3})(\d{4})" "($1) $2-$3"))
