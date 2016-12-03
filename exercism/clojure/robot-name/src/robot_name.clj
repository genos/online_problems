(ns robot-name)

(defn- rand-char []
  (rand-nth "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

(defn- rand-digit []
  (str (rand-int 10)))

(defn- new-name []
  (str (rand-char) (rand-char) (rand-digit) (rand-digit) (rand-digit)))

(defn robot []
  (atom (new-name)))

(defn robot-name [r]
  @r)

(defn reset-name [r]
  (reset! r (new-name)))
