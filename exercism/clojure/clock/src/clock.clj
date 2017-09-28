(ns clock)

;; Per http://exercism.io/submissions/468867ee4cbd47ca83a4d850f3082e42,
;; we represent clocks in terms of minutes

(defn- to-minutes [c] (mod c 1440))

(defn clock [h m] (to-minutes (+ (* 60 h) m)))

(defn add-time [c m] (to-minutes (+ c m)))

(defn clock->string [c]
  (format "%02d:%02d" (quot c 60) (mod c 60)))
