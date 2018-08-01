(ns reverse-string)

(defn reverse-string-into [s]
  (->>
    s
    (into nil)
    (apply str)))

(defn reverse-string-reduce [s]
  (->>
    s
    (reduce conj nil)
    (apply str)))

(defn reverse-string-recursion [s]
  (loop [s s
         l nil]
    (if (empty? s)
      (apply str l)
      (recur (rest s) (conj l (first s))))))

(def reverse-string reverse-string-into)
