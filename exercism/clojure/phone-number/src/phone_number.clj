(ns phone-number
  (:require [clojure.string :as str]))

(defrecord PhoneNumber [area exchange line])

(defn- clean [s]
  (str/replace s #"[^0-9]" ""))

(defn- validate [s]
  (let [c (clean s)
        n (count c)]
    (cond
      (= 10 n) s
      (and (= 11 n) (str/starts-with? s "1")) (subs s 1)
      :else "0000000000")))

(defn- parse [s]
  (->> s
       (re-find #"(\d{3})(\d{3})(\d{4})")
       (rest)
       (apply ->PhoneNumber)))

(defn- to-phone-number [s]
  (-> s clean validate parse))

(defn- print-flat [p]
  (let [a (:area p)
        e (:exchange p)
        l (:line p)]
    (str a e l)))

(defn print-formatted [p]
  (let [a (:area p)
        e (:exchange p)
        l (:line p)]
    (format "(%s) %s-%s" a e l)))

(defn number [s]
  (-> s to-phone-number print-flat))

(defn area-code [s]
  (-> s to-phone-number :area))

(defn pretty-print [s] s
  (-> s to-phone-number print-formatted))
