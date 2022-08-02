(ns cars-assemble)

(def base-rate
  "Base rate of cars made per hour"
  221)

(defn success-rate
  "Success rate given a speed"
  [speed]
  (case speed
    0 0.0
    (1 2 3 4) 1.0
    (5 6 7 8) 0.9
    9 0.8
    10 0.77))

(defn production-rate
  "Returns the assembly line's production rate per hour,
   taking into account its success rate"
  [speed]
  (* base-rate speed (success-rate speed)))


(defn working-items
  "Calculates how many working cars are produced per minute"
  [speed]
  (-> speed
      (production-rate)
      (/ 60)
      int))
