(ns beer-song
  (:require [clojure.string :refer [join capitalize]]))

(defn- a [n]
  (cond
    (zero? n) "no more"
    (= -1 n) "99"
    :else (str n)))

(defn- b [n]
  (if (= n 1)
    "bottle"
    "bottles"))

(defn- c [n]
  (if (zero? n)
    "Go to the store and buy some more"
    (str "Take " (if (= n 1) "it" "one") " down and pass it around")))

(defn verse [n]
  (format
    (str
      "%s %s of beer on the wall, %s %s of beer.\n"
      "%s, %s %s of beer on the wall.\n")
    (capitalize (a n))
    (b n)
    (a n)
    (b n)
    (c n)
    (a (dec n))
    (b (dec n))))


(defn sing
  ([n] (sing n 0))
  ([start end]
   (->>
     (range start (dec end) -1)
     (map verse)
     (join "\n"))))
