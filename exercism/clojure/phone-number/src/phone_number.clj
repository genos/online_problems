(ns phone-number
  (:require [clojure.string :as str]))

(defrecord PhoneNumber [area exchange line])

(defn- validate [s]
  (let [c (str/replace s #"\D" "")
        n (count c)]
    (cond
      (= 10 n) c
      (and (= 11 n) (str/starts-with? c "1")) (subs c 1)
      :else "0000000000")))

(defn- parse [s]
  (->> s
       (re-find #"(\d{3})(\d{3})(\d{4})")
       rest
       (apply ->PhoneNumber)))

(defn- to-phone-number [s]
  (-> s validate parse))

(defn- flat [p]
  (let [a (:area p)
        e (:exchange p)
        l (:line p)]
    (str a e l)))

(defn- formatted [p]
  (let [a (:area p)
        e (:exchange p)
        l (:line p)]
    (format "(%s) %s-%s" a e l)))

(defn number [s]
  (-> s to-phone-number flat))

(defn area-code [s]
  (-> s to-phone-number :area))

(defn pretty-print [s] s
  (-> s to-phone-number formatted))
