(ns beer-song
  (:require [clojure.string :refer [join capitalize]]))

(defn- amount [n]
  (condp = n
    0 "no more"
    -1 "99"
    (str n)))

(defn- pluralize [n]
  (if (not= n 1) "s" ""))

(defn- continue [n]
  (if (zero? n)
    "Go to the store and buy some more"
    (str "Take" (if (= n 1) " it " " one ") "down and pass it around")))

(defn verse [n]
  (format
    (str
      "%s bottle%s of beer on the wall, %s bottle%s of beer.\n"
      "%s, %s bottle%s of beer on the wall.\n")
    (capitalize (amount n))
    (pluralize n)
    (amount n)
    (pluralize n)
    (continue n)
    (amount (dec n))
    (pluralize (dec n))))


(defn sing
  ([n] (sing n 0))
  ([start end]
   (->>
     (range start (dec end) -1)
     (map verse)
     (join "\n"))))
