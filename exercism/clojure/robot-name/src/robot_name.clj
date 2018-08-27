(ns robot-name
  (:require [clojure.string :refer [join]]))

(def +seen-names+ (atom #{}))

(defn- rand-char []
  (char (+ (rand-int 26) (int \A))))

(defn- rand-digit []
  (str (rand-int 10)))

(defn- new-name []
  (let [n (join (concat (repeatedly 2 rand-char) (repeatedly 3 rand-digit)))]
    (if
      (not (contains? @+seen-names+ n))
      (do
        (swap! +seen-names+ conj n)
        n)
      (recur))))

(defn robot []
  (atom (new-name)))

(defn robot-name [r]
  @r)

(defn reset-name [r]
  (reset! r (new-name)))
