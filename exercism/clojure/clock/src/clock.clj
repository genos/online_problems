(ns clock)

(defn clock [a b]
  (let [q (Math/floorDiv b 60)
        h (mod (+ a q) 24)
        m (mod b 60)]
    {:hours h :minutes m}))

(defn add-time [c n]
  (clock (:hours c) (+ (:minutes c) n)))

(defn clock->string [c]
  (format "%02d:%02d" (:hours c) (:minutes c)))
