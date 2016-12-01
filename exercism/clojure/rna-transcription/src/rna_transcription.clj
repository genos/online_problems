(ns rna-transcription
  (:require [clojure.string :as str]))

(defn- transcribe [c]
  (cond
    (= c \A) \U
    (= c \C) \G
    (= c \G) \C
    (= c \T) \A
    :else (throw (AssertionError. "invalid character"))))

(defn to-rna [dna]
  (str/join (map transcribe dna)))
