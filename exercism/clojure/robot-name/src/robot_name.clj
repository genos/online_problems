(ns robot-name
  (:require [clojure.string :refer [join]]))

(defn- rand-char []
  (char (+ (rand-int 26) (int \A))))

(defn- rand-digit []
  (str (rand-int 10)))

(defn- new-name []
  (join (concat (repeatedly 2 rand-char) (repeatedly 3 rand-digit))))

(defn robot []
  (atom (new-name)))

(defn robot-name [r]
  @r)

(defn reset-name [r]
  (reset! r (new-name)))
